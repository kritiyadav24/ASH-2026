# ============================================================
# NIS 2023 Core: Sepsis escalation of care by rural vs. urban
#
# RQ: Among adults hospitalized with a principal diagnosis of
# sepsis, does escalation to ICU-level care (mechanical
# ventilation) differ by rural vs. urban patient location, and are
# there differences in in-hospital mortality or length of stay?
#
# COHORT: Age 18+, PRINCIPAL diagnosis (I10_DX1) is sepsis --
# same codes already validated in the Complicated Hospitalization
# and early-onset cancer scripts: A40-A41, R65.2x.
#
# EXPOSURE: Rural vs. urban via PL_NCHS2. UNLIKE every earlier
# script in this repo, this mapping is now CONFIRMED (HCUP User
# Support replied directly): PL_NCHS2 == 21 (metropolitan) = Urban,
# consistent with old PL_NCHS 1-4; PL_NCHS2 == 22 (non-metropolitan)
# = Rural, consistent with old PL_NCHS 5-6. No diagnostic caveat
# needed on this variable anymore.
#
# OUTCOMES: mechanical ventilation (ICD-10-PCS 5A19-, the same
# ICU-level-care proxy used in the Complicated Hospitalization
# script -- invasive ventilation only, NIV/BiPAP not captured),
# in-hospital mortality, length of stay.
#
# STRATIFICATION: age, sex.
#
# ARCHITECTURE: same chunked base-R streaming approach as every
# other script in this repo.
# ============================================================

suppressMessages({
  library(tidyverse)
  library(survey)
})

nis_file       <- "~/Downloads/NIS_2023/NIS_2023_Core.ASC"
checkpoint_rds <- "~/Downloads/NIS_2023/nis_2023_sepsis_rural_urban_checkpoint.rds"


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
pr_starts <- seq(355, 523, by = 7); pr_ends <- pr_starts + 6  # I10_PR1-25

hcup_missing_codes <- list(
  N2PF    = c(-9, -8, -6, -5),
  N3PF    = c(-99, -88, -66),
  N4PF    = c(-999, -888, -666),
  N5PF    = c(-9999, -8888, -6666),
  N10PF   = c(-999999999, -888888888, -666666666),
  N11P7F  = c(-99.9999999, -88.8888888, -66.6666666)
)
clean_missing <- function(x, informat) { x[x %in% hcup_missing_codes[[informat]]] <- NA; x }


# ----- 2. ICD-10-CM/PCS CODE DEFINITIONS (no decimal points) -----
sepsis_regex   <- "^(A4[01]|R652)"
mechvent_regex <- "^5A19"

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
    is_sepsis_principal <- grepl(sepsis_regex, dx1)
    age_in_range <- scalar_df$AGE >= 18 & !is.na(scalar_df$AGE)

    scalar_df$cohort <- is_sepsis_principal & age_in_range
    cohort_mask <- scalar_df$cohort

    scalar_df$mech_vent <- NA
    if (any(cohort_mask)) {
      sub_lines <- lines[cohort_mask]
      scalar_df$mech_vent[cohort_mask] <- any_match(sub_lines, pr_starts, pr_ends, mechvent_regex)
    }

    all_chunks[[chunk_i]] <- scalar_df
    cat("Chunk", chunk_i, "--", n, "rows,", sum(cohort_mask), "cohort rows so far this chunk\n")
  }
  close(con)

  full_derived <- bind_rows(all_chunks)
  cat("\nTotal rows scanned:", nrow(full_derived), "\n")
  cat("Total sepsis cohort rows:", sum(full_derived$cohort), "\n")

  rm(all_chunks); gc()

  dir.create(dirname(checkpoint_rds), showWarnings = FALSE, recursive = TRUE)
  saveRDS(full_derived, checkpoint_rds)
  cat("Saved checkpoint to", checkpoint_rds, "\n")
}


# ----- 4. DERIVE rural/urban (CONFIRMED mapping -- see header) -----
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
svy_design_sep <- svydesign(
  ids = ~HOSP_NIS, strata = ~NIS_STRATUM, weights = ~DISCWT,
  data = full_derived, nest = TRUE
)
svy_cohort_sep <- subset(svy_design_sep, cohort)

cat("\n========== COHORT SIZE ==========\n")
cat("Unweighted sepsis cohort n:", nrow(svy_cohort_sep$variables), "\n")
cat("Weighted (national estimate) cohort n:", round(sum(weights(svy_cohort_sep))), "\n")

cat("\nRural/urban distribution:\n")
print(table(svy_cohort_sep$variables$rural, useNA = "always"))


# ----- 6. OUTCOMES BY RURAL/URBAN -----
cat("\n========== Mechanical ventilation (ICU-level care) by rural/urban ==========\n")
print(svyby(~mech_vent, ~rural, svy_cohort_sep, svymean, na.rm = TRUE))
model_vent <- svyglm(mech_vent ~ rural + AGE + FEMALE, design = svy_cohort_sep, family = quasibinomial())
print(round(exp(cbind(OR = coef(model_vent), confint(model_vent))), 3))
rm(model_vent); gc()

cat("\n========== Mortality by rural/urban ==========\n")
print(svyby(~DIED, ~rural, svy_cohort_sep, svymean, na.rm = TRUE))
model_mortality <- svyglm(DIED ~ rural + AGE + FEMALE, design = svy_cohort_sep, family = quasibinomial())
print(round(exp(cbind(OR = coef(model_mortality), confint(model_mortality))), 3))
rm(model_mortality); gc()

cat("\n========== Length of stay by rural/urban ==========\n")
print(svyby(~LOS, ~rural, svy_cohort_sep, svymean, na.rm = TRUE))
model_los <- svyglm(LOS ~ rural + AGE + FEMALE, design = svy_cohort_sep, family = gaussian())
print(round(cbind(Estimate = coef(model_los), confint(model_los)), 3))
rm(model_los); gc()

cat("\n========== ALL ANALYSES COMPLETE ==========\n")
