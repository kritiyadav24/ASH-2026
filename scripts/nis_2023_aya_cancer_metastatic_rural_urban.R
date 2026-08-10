# ============================================================
# NIS 2023 Core: Metastatic disease at presentation in AYA cancer
# patients, by rural vs. urban
#
# RQ: Among adolescent and young adult (AYA, age 18-39) cancer
# patients, does the rate of metastatic disease present at THIS
# hospitalization differ by rural vs. urban patient location?
#
# WHY THIS QUESTION: the earlier AYA transfer-out script came back
# null (rural AYA cancer patients were NOT transferred out more
# than urban ones). This is a sharper test of the same underlying
# "delayed access to care" hypothesis, using something visible in
# a single inpatient snapshot -- unlike transfer patterns, disease
# stage at presentation doesn't require tracking a patient over
# time. Real precedent: a SEER-registry population-based study
# found low-SES AYA patients were ~2x more likely to present with
# metastatic disease than high-SES AYA patients, and metastatic
# disease at diagnosis is the strongest predictor of death in this
# population. That study used socioeconomic status via SEER, not
# rural/urban via NIS -- this script tests the NIS/rural-urban
# version of the same underlying mechanism, which a literature
# search this session did not find already done.
#
# COHORT: Age 18-39, PRINCIPAL diagnosis (I10_DX1) is a malignant
# neoplasm ("C" code), EXCLUDING non-melanoma skin cancer (C44) --
# same definition as the AYA transfer-out script.
#
# EXPOSURE: Rural vs. urban via PL_NCHS2 -- CONFIRMED mapping
# (HCUP User Support): 21 = Urban, 22 = Rural.
#
# OUTCOME: metastatic disease present ANYWHERE in this admission's
# diagnosis list (all 40 dx fields, not just secondary -- a patient
# whose PRINCIPAL diagnosis is itself a metastatic code, e.g. a
# brain metastasis admission, clearly has metastatic disease too).
# ICD-10-CM codes C77 (secondary malignant neoplasm of lymph
# nodes), C78 (secondary malignant neoplasm of respiratory/
# digestive organs), C79 (secondary malignant neoplasm of other/
# unspecified sites).
#
# SECONDARY OUTCOMES: in-hospital mortality, length of stay --
# included for context.
#
# STRATIFICATION: age, sex.
#
# ARCHITECTURE: same chunked base-R streaming approach as every
# other script in this repo. NEW checkpoint file (not reusing the
# AYA transfer-out script's checkpoint) because this scan needs
# ALL 40 dx fields checked for metastasis codes, which that
# checkpoint didn't compute.
# ============================================================

suppressMessages({
  library(tidyverse)
  library(survey)
})

nis_file       <- "~/Downloads/NIS_2023/NIS_2023_Core.ASC"
checkpoint_rds <- "~/Downloads/NIS_2023/nis_2023_aya_cancer_metastatic_checkpoint.rds"


# ----- 1. COLUMN LAYOUT (positions validated in earlier scripts) -----
full_spec <- tibble::tribble(
  ~var, ~start, ~end, ~type, ~informat,
  "HOSP_NIS", 1L, 5L, "numeric", "N5PF",
  "KEY_NIS", 6L, 15L, "numeric", "N10PF",
  "NIS_STRATUM", 16L, 19L, "numeric", "N4PF",
  "AGE", 20L, 22L, "numeric", "N3PF",
  "DIED", 29L, 30L, "numeric", "N2PF",
  "DISCWT", 31L, 41L, "numeric", "N11P7F",
  "FEMALE", 56L, 57L, "numeric", "N2PF",
  "LOS", 533L, 537L, "numeric", "N5PF",
  "PL_NCHS2", 641L, 643L, "numeric", "N3PF"
)
dx_starts <- seq(67, 340, by = 7); dx_ends <- dx_starts + 6   # I10_DX1-40

hcup_missing_codes <- list(
  N2PF    = c(-9, -8, -6, -5),
  N3PF    = c(-99, -88, -66),
  N4PF    = c(-999, -888, -666),
  N5PF    = c(-9999, -8888, -6666),
  N10PF   = c(-999999999, -888888888, -666666666),
  N11P7F  = c(-99.9999999, -88.8888888, -66.6666666)
)
clean_missing <- function(x, informat) { x[x %in% hcup_missing_codes[[informat]]] <- NA; x }


# ----- 2. ICD-10-CM CODE DEFINITIONS (no decimal points) -----
cancer_regex               <- "^C"
skin_cancer_exclude_regex  <- "^C44"
metastasis_regex           <- "^C7[789]"

any_match <- function(lines, starts, ends, regex) {
  hit <- rep(FALSE, length(lines))
  for (i in seq_along(starts)) {
    codes <- trimws(substr(lines, starts[i], ends[i]))
    hit <- hit | grepl(regex, codes)
  }
  hit
}


# ----- 3. STREAM THE FILE -----
if (file.exists(checkpoint_rds)) {
  cat("Found existing checkpoint, loading instead of re-scanning:", checkpoint_rds, "\n")
  full_derived <- readRDS(checkpoint_rds)
} else {
  con <- file(nis_file, "r")
  chunk_size <- 200000
  all_chunks <- list()
  chunk_i <- 0

  repeat {
    lines <- readLines(con, n = chunk_size)
    if (length(lines) == 0) break
    chunk_i <- chunk_i + 1
    n <- length(lines)

    scalar_df <- as.data.frame(lapply(seq_len(nrow(full_spec)), function(i) {
      r <- full_spec[i, ]
      vals <- as.numeric(substr(lines, r$start, r$end))
      clean_missing(vals, r$informat)
    }))
    names(scalar_df) <- full_spec$var

    dx1 <- trimws(substr(lines, dx_starts[1], dx_ends[1]))
    is_cancer_principal <- grepl(cancer_regex, dx1) & !grepl(skin_cancer_exclude_regex, dx1)
    age_in_range <- scalar_df$AGE >= 18 & scalar_df$AGE <= 39 & !is.na(scalar_df$AGE)

    scalar_df$cohort <- is_cancer_principal & age_in_range
    cohort_mask <- scalar_df$cohort

    # Expensive check (all 40 dx fields) -- only for this chunk's cohort rows
    scalar_df$has_metastasis <- NA
    if (any(cohort_mask)) {
      sub_lines <- lines[cohort_mask]
      scalar_df$has_metastasis[cohort_mask] <- any_match(sub_lines, dx_starts, dx_ends, metastasis_regex)
    }

    all_chunks[[chunk_i]] <- scalar_df
    cat("Chunk", chunk_i, "--", n, "rows,", sum(cohort_mask), "cohort rows so far this chunk\n")
  }
  close(con)

  full_derived <- bind_rows(all_chunks)
  cat("\nTotal rows scanned:", nrow(full_derived), "\n")
  cat("Total AYA cancer cohort rows:", sum(full_derived$cohort), "\n")

  rm(all_chunks); gc()

  dir.create(dirname(checkpoint_rds), showWarnings = FALSE, recursive = TRUE)
  saveRDS(full_derived, checkpoint_rds)
  cat("Saved checkpoint to", checkpoint_rds, "\n")
}


# ----- 4. DERIVE rural/urban (CONFIRMED mapping) -----
full_derived <- full_derived %>%
  mutate(
    rural = case_when(
      PL_NCHS2 == 21 ~ "Urban",
      PL_NCHS2 == 22 ~ "Rural",
      TRUE ~ NA_character_
    ),
    rural = factor(rural, levels = c("Urban", "Rural"))
  )


# ----- 5. SURVEY DESIGN + SUBPOPULATION SUBSET -----
svy_design_met <- svydesign(
  ids = ~HOSP_NIS, strata = ~NIS_STRATUM, weights = ~DISCWT,
  data = full_derived, nest = TRUE
)
svy_cohort_met <- subset(svy_design_met, cohort)

cat("\n========== COHORT SIZE ==========\n")
cat("Unweighted AYA cancer cohort n:", nrow(svy_cohort_met$variables), "\n")
cat("Weighted (national estimate) cohort n:", round(sum(weights(svy_cohort_met))), "\n")

cat("\nRural/urban distribution:\n")
print(table(svy_cohort_met$variables$rural, useNA = "always"))

cat("\nOverall weighted metastatic disease rate:\n")
print(svymean(~has_metastasis, svy_cohort_met, na.rm = TRUE))


# ----- 6. PRIMARY: METASTATIC DISEASE RATE BY RURAL/URBAN -----
cat("\n========== PRIMARY: metastatic disease rate by rural/urban ==========\n")
print(svyby(~has_metastasis, ~rural, svy_cohort_met, svymean, na.rm = TRUE))

cell_counts <- svy_cohort_met$variables %>%
  filter(!is.na(rural)) %>%
  count(rural, has_metastasis)
cat("\nCell counts (rural x has_metastasis) -- flag anything sparse:\n")
print(cell_counts)

model_met <- svyglm(has_metastasis ~ rural + AGE + FEMALE, design = svy_cohort_met, family = quasibinomial())
cat("\nAdjusted OR (Rural vs. Urban, ref = Urban):\n")
print(round(exp(cbind(OR = coef(model_met), confint(model_met))), 3))
rm(model_met); gc()


# ----- 7. SECONDARY: MORTALITY AND LOS BY RURAL/URBAN -----
cat("\n========== SECONDARY: mortality by rural/urban ==========\n")
print(svyby(~DIED, ~rural, svy_cohort_met, svymean, na.rm = TRUE))
model_mortality <- svyglm(DIED ~ rural + AGE + FEMALE, design = svy_cohort_met, family = quasibinomial())
print(round(exp(cbind(OR = coef(model_mortality), confint(model_mortality))), 3))
rm(model_mortality); gc()

cat("\n========== SECONDARY: length of stay by rural/urban ==========\n")
print(svyby(~LOS, ~rural, svy_cohort_met, svymean, na.rm = TRUE))
model_los <- svyglm(LOS ~ rural + AGE + FEMALE, design = svy_cohort_met, family = gaussian())
print(round(cbind(Estimate = coef(model_los), confint(model_los)), 3))
rm(model_los); gc()

cat("\n========== ALL ANALYSES COMPLETE ==========\n")
