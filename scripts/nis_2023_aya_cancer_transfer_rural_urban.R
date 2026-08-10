# ============================================================
# NIS 2023 Core: AYA cancer patients -- transfer-out rates by
# rural vs. urban
#
# RQ: Among adolescent and young adult (AYA, age 18-39 -- the
# standard NCI-recognized AYA age range, capped at 18 since this
# is the adult NIS Core file) cancer patients, are rural-admitted
# patients transferred OUT to another facility at higher rates
# than urban-admitted patients?
#
# WHY THIS QUESTION: the AYA oncology literature repeatedly states
# that rural AYA patients face "decreased access to specialized
# cancer centers," but a literature search this session did not
# find an NIS-based study directly testing transfer-out rates as
# the outcome -- most AYA+NIS work focuses on palliative care /
# end-of-life utilization (2016-2019 data) or general disparities
# commentary, not this specific access-pathway question.
#
# COHORT: Age 18-39, PRINCIPAL diagnosis (I10_DX1) is a malignant
# neoplasm ("C" code), EXCLUDING non-melanoma skin cancer (C44) --
# same cancer definition used in the early-onset cancer script.
#
# EXPOSURE: Rural vs. urban via PL_NCHS2 -- CONFIRMED mapping
# (HCUP User Support replied directly): 21 = Urban (metropolitan),
# 22 = Rural (non-metropolitan). No diagnostic caveat needed.
#
# OUTCOME: transferred OUT to another short-term hospital
# (TRAN_OUT). UNLIKE PL_NCHS2, this variable's exact value scheme
# has NOT been independently confirmed -- Step 3 prints its raw
# distribution before anything is trusted. Working assumption
# (consistent with how TRAN_IN was used in the transfusion script):
# TRAN_OUT > 0 means transferred out; STOP and check if the printed
# distribution doesn't look like a small integer code (e.g. 0/1, or
# 0/1/2 for different transfer destination types).
#
# SECONDARY OUTCOMES: in-hospital mortality, length of stay --
# included for context, not the primary question.
#
# STRATIFICATION: sex. Cancer type is NOT stratified in this first
# pass (kept minimal per the deliberate "don't overcomplicate"
# preference established earlier in this project) -- can be added
# later reusing the 10-category cancer_type logic from the
# early-onset cancer script if the top-line result looks promising.
#
# ARCHITECTURE: same chunked base-R streaming approach as every
# other script in this repo.
# ============================================================

suppressMessages({
  library(tidyverse)
  library(survey)
})

nis_file       <- "~/Downloads/NIS_2023/NIS_2023_Core.ASC"
checkpoint_rds <- "~/Downloads/NIS_2023/nis_2023_aya_cancer_transfer_checkpoint.rds"


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
  "TRAN_OUT", 623L, 624L, "numeric", "N2PF",
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


# ----- 3. STREAM THE FILE, WITH A DIAGNOSTIC CHECK FOR TRAN_OUT -----
if (file.exists(checkpoint_rds)) {
  cat("Found existing checkpoint, loading instead of re-scanning:", checkpoint_rds, "\n")
  full_derived <- readRDS(checkpoint_rds)
} else {
  con <- file(nis_file, "r")
  chunk_size <- 200000
  all_chunks <- list()
  chunk_i <- 0
  diagnostic_printed <- FALSE

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

    if (!diagnostic_printed) {
      cat("\n========== DIAGNOSTIC: TRAN_OUT raw value distribution (first chunk) ==========\n")
      print(table(scalar_df$TRAN_OUT, useNA = "always"))
      cat("This script treats TRAN_OUT > 0 as 'transferred out to another facility'\n")
      cat("(same convention as TRAN_IN in the transfusion script). If the distribution\n")
      cat("doesn't look like a clean small-integer code, STOP and tell Claude before\n")
      cat("trusting the transfer-rate results below.\n")
      cat("=================================================================\n\n")
      diagnostic_printed <- TRUE
    }

    dx1 <- trimws(substr(lines, dx_starts[1], dx_ends[1]))
    is_cancer_principal <- grepl(cancer_regex, dx1) & !grepl(skin_cancer_exclude_regex, dx1)
    age_in_range <- scalar_df$AGE >= 18 & scalar_df$AGE <= 39 & !is.na(scalar_df$AGE)

    scalar_df$cohort <- is_cancer_principal & age_in_range

    all_chunks[[chunk_i]] <- scalar_df
    cat("Chunk", chunk_i, "--", n, "rows,", sum(scalar_df$cohort), "cohort rows so far this chunk\n")
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


# ----- 4. DERIVE rural/urban (CONFIRMED mapping) AND transferred_out -----
full_derived <- full_derived %>%
  mutate(
    rural = case_when(
      PL_NCHS2 == 21 ~ "Urban",
      PL_NCHS2 == 22 ~ "Rural",
      TRUE ~ NA_character_
    ),
    rural = factor(rural, levels = c("Urban", "Rural")),
    transferred_out = TRAN_OUT > 0 & !is.na(TRAN_OUT)
  )


# ----- 5. SURVEY DESIGN + SUBPOPULATION SUBSET -----
svy_design_aya <- svydesign(
  ids = ~HOSP_NIS, strata = ~NIS_STRATUM, weights = ~DISCWT,
  data = full_derived, nest = TRUE
)
svy_cohort_aya <- subset(svy_design_aya, cohort)

cat("\n========== COHORT SIZE ==========\n")
cat("Unweighted AYA cancer cohort n:", nrow(svy_cohort_aya$variables), "\n")
cat("Weighted (national estimate) cohort n:", round(sum(weights(svy_cohort_aya))), "\n")

cat("\nRural/urban distribution:\n")
print(table(svy_cohort_aya$variables$rural, useNA = "always"))


# ----- 6. PRIMARY: TRANSFER-OUT RATE BY RURAL/URBAN -----
cat("\n========== PRIMARY: transfer-out rate by rural/urban ==========\n")
print(svyby(~transferred_out, ~rural, svy_cohort_aya, svymean, na.rm = TRUE))

cell_counts <- svy_cohort_aya$variables %>%
  filter(!is.na(rural)) %>%
  count(rural, transferred_out)
cat("\nCell counts (rural x transferred_out) -- flag anything sparse:\n")
print(cell_counts)

model_transfer <- svyglm(transferred_out ~ rural + AGE + FEMALE, design = svy_cohort_aya, family = quasibinomial())
cat("\nAdjusted OR (Rural vs. Urban, ref = Urban):\n")
print(round(exp(cbind(OR = coef(model_transfer), confint(model_transfer))), 3))
rm(model_transfer); gc()


# ----- 7. SECONDARY: MORTALITY AND LOS BY RURAL/URBAN -----
cat("\n========== SECONDARY: mortality by rural/urban ==========\n")
print(svyby(~DIED, ~rural, svy_cohort_aya, svymean, na.rm = TRUE))
model_mortality <- svyglm(DIED ~ rural + AGE + FEMALE, design = svy_cohort_aya, family = quasibinomial())
print(round(exp(cbind(OR = coef(model_mortality), confint(model_mortality))), 3))
rm(model_mortality); gc()

cat("\n========== SECONDARY: length of stay by rural/urban ==========\n")
print(svyby(~LOS, ~rural, svy_cohort_aya, svymean, na.rm = TRUE))
model_los <- svyglm(LOS ~ rural + AGE + FEMALE, design = svy_cohort_aya, family = gaussian())
print(round(cbind(Estimate = coef(model_los), confint(model_los)), 3))
rm(model_los); gc()

cat("\n========== ALL ANALYSES COMPLETE ==========\n")
