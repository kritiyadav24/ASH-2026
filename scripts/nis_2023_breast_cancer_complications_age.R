# ============================================================
# NIS 2023 Core: Complication burden in breast cancer
# hospitalizations, AYA vs. older-onset
#
# RQ: Among adults hospitalized with a principal diagnosis of
# breast cancer, does complication burden (surgical site
# infection, sepsis, VTE, transfusion need, ICU-level care) differ
# between AYA (age 18-39) and older-onset (age 40-64) patients?
#
# WHY THIS QUESTION: a SEER-based population study (real, verified
# this session) found breast cancer is one of the few cancer types
# where YOUNG patients present with metastatic disease at a HIGHER
# rate than older patients -- most other cancers show the opposite
# pattern. SEER tracks stage/survival well but not in-hospital
# complications. This script asks the complementary question SEER
# structurally cannot answer: given that young breast cancer
# patients may present more advanced, does their in-hospital
# complication burden differ from older-onset patients? A
# literature search this session found no NIS-based (or any
# nationally-representative US) study directly comparing these two
# age groups on complications -- the closest match uses a military/
# universal-coverage health system, a very different population
# from NIS's diverse-payer-mix cohort. One relevant data point: a
# study on nipple-sparing mastectomy specifically found NO
# complication difference by age -- so an age-related gap here is
# a genuine open question, not an assumed answer.
#
# COHORT: Age 18-64, PRINCIPAL diagnosis (I10_DX1) is breast
# cancer (C50), split into AYA (18-39) and older-onset (40-64).
# Capped at 64 (not including 65+) to compare against a still-
# generally-healthy, pre-Medicare population rather than mixing in
# elderly comorbidity/frailty effects -- same rationale used in
# the earlier AYA-vs-older-onset metastatic script design.
#
# OUTCOMES (all reuse code definitions already validated in
# earlier scripts in this repo):
#   - Surgical site / postprocedural infection: T81.4
#   - Sepsis: A40-A41, R65.2x
#   - VTE: I26 (PE), I824 (acute lower-extremity DVT)
#   - Transfusion: ICD-10-PCS table 302 (Administration,
#     Circulatory, Transfusion -- verified structure, any body
#     part/substance/approach)
#   - Mechanical ventilation (ICU-level care proxy): ICD-10-PCS
#     5A19- (invasive ventilation only, NIV/BiPAP not captured)
#
# STRATIFICATION: age group (AYA 18-39 vs. older-onset 40-64).
#
# ARCHITECTURE: same chunked base-R streaming approach as every
# other script in this repo.
# ============================================================

suppressMessages({
  library(tidyverse)
  library(survey)
})

nis_file       <- "~/Downloads/NIS_2023/NIS_2023_Core.ASC"
checkpoint_rds <- "~/Downloads/NIS_2023/nis_2023_breast_cancer_complications_checkpoint.rds"


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
  "LOS", 533L, 537L, "numeric", "N5PF"
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
breast_cancer_regex <- "^C50"
infection_regex      <- "^T814"
sepsis_regex         <- "^(A4[01]|R652)"
vte_regex            <- "^(I26|I824)"
transfusion_regex    <- "^302"
mechvent_regex       <- "^5A19"

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
    is_breast_principal <- grepl(breast_cancer_regex, dx1)
    age_in_range <- scalar_df$AGE >= 18 & scalar_df$AGE <= 64 & !is.na(scalar_df$AGE)

    scalar_df$cohort <- is_breast_principal & age_in_range
    cohort_mask <- scalar_df$cohort

    # Expensive checks -- only for this chunk's cohort rows
    scalar_df$has_infection <- NA
    scalar_df$has_sepsis <- NA
    scalar_df$has_vte <- NA
    scalar_df$has_transfusion <- NA
    scalar_df$has_mechvent <- NA

    if (any(cohort_mask)) {
      sub_lines <- lines[cohort_mask]
      scalar_df$has_infection[cohort_mask]   <- any_match(sub_lines, dx_starts, dx_ends, infection_regex)
      scalar_df$has_sepsis[cohort_mask]      <- any_match(sub_lines, dx_starts, dx_ends, sepsis_regex)
      scalar_df$has_vte[cohort_mask]         <- any_match(sub_lines, dx_starts, dx_ends, vte_regex)
      scalar_df$has_transfusion[cohort_mask] <- any_match(sub_lines, pr_starts, pr_ends, transfusion_regex)
      scalar_df$has_mechvent[cohort_mask]    <- any_match(sub_lines, pr_starts, pr_ends, mechvent_regex)
    }

    all_chunks[[chunk_i]] <- scalar_df
    cat("Chunk", chunk_i, "--", n, "rows,", sum(cohort_mask), "cohort rows so far this chunk\n")
  }
  close(con)

  full_derived <- bind_rows(all_chunks)
  cat("\nTotal rows scanned:", nrow(full_derived), "\n")
  cat("Total breast cancer cohort rows:", sum(full_derived$cohort), "\n")

  rm(all_chunks); gc()

  dir.create(dirname(checkpoint_rds), showWarnings = FALSE, recursive = TRUE)
  saveRDS(full_derived, checkpoint_rds)
  cat("Saved checkpoint to", checkpoint_rds, "\n")
}


# ----- 4. DERIVE AGE GROUP -----
full_derived <- full_derived %>%
  mutate(
    age_group = case_when(
      AGE >= 18 & AGE <= 39 ~ "AYA (18-39)",
      AGE >= 40 & AGE <= 64 ~ "Older-onset (40-64)",
      TRUE ~ NA_character_
    ),
    age_group = factor(age_group, levels = c("Older-onset (40-64)", "AYA (18-39)"))
  )


# ----- 5. SURVEY DESIGN + SUBPOPULATION SUBSET -----
svy_design_bc <- svydesign(
  ids = ~HOSP_NIS, strata = ~NIS_STRATUM, weights = ~DISCWT,
  data = full_derived, nest = TRUE
)
svy_cohort_bc <- subset(svy_design_bc, cohort)

cat("\n========== COHORT SIZE ==========\n")
cat("Unweighted breast cancer cohort n:", nrow(svy_cohort_bc$variables), "\n")
cat("Weighted (national estimate) cohort n:", round(sum(weights(svy_cohort_bc))), "\n")

cat("\nAge group distribution:\n")
print(table(svy_cohort_bc$variables$age_group, useNA = "always"))


# ----- 6. HELPER FUNCTION FOR EACH COMPLICATION OUTCOME -----
analyze_complication <- function(outcome_name, outcome_label) {
  cat("\n========== ", outcome_label, " by age group ==========\n")

  form_rate <- as.formula(paste0("~", outcome_name))
  cat("Weighted rate by age group:\n")
  print(svyby(form_rate, ~age_group, svy_cohort_bc, svymean, na.rm = TRUE))

  cell_counts <- svy_cohort_bc$variables %>%
    filter(!is.na(age_group)) %>%
    count(age_group, .data[[outcome_name]])
  cat("\nCell counts (age_group x outcome) -- flag anything sparse:\n")
  print(cell_counts)

  form_adj <- as.formula(paste0(outcome_name, " ~ age_group"))
  model_adj <- svyglm(form_adj, design = svy_cohort_bc, family = quasibinomial())
  cat("\nAdjusted OR (AYA vs. Older-onset, ref = Older-onset):\n")
  print(round(exp(cbind(OR = coef(model_adj), confint(model_adj))), 3))

  rm(model_adj); gc()
}


# ----- 7. RUN ALL COMPLICATION OUTCOMES -----
analyze_complication("has_infection", "Surgical site / postprocedural infection")
analyze_complication("has_sepsis", "Sepsis")
analyze_complication("has_vte", "VTE")
analyze_complication("has_transfusion", "Transfusion")
analyze_complication("has_mechvent", "Mechanical ventilation (ICU-level care)")


# ----- 8. SECONDARY: MORTALITY AND LOS BY AGE GROUP -----
cat("\n========== SECONDARY: mortality by age group ==========\n")
print(svyby(~DIED, ~age_group, svy_cohort_bc, svymean, na.rm = TRUE))

cat("\n========== SECONDARY: length of stay by age group ==========\n")
print(svyby(~LOS, ~age_group, svy_cohort_bc, svymean, na.rm = TRUE))

cat("\n========== ALL ANALYSES COMPLETE ==========\n")
