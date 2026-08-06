# ============================================================
# NIS 2023 Core: Complicated index hospitalization across 5
# common medicine admissions
#
# RQ: Among adults hospitalized with a PRINCIPAL diagnosis of
# pneumonia, CHF exacerbation, sepsis, COPD exacerbation, or DKA,
# how do rates of in-hospital mortality, ICU-level care (mechanical
# ventilation), and length of stay compare across these 5
# admission types, and what patient factors (age, sex) predict
# each within diagnosis?
#
# WHY THIS REPLACES A READMISSION ANALYSIS: NIS Core is a
# discharge-level file with no patient-level identifier that
# persists across hospitalizations -- there is no way to tell if
# the same person appears twice in the file, so readmissions
# cannot be measured here at all (that requires HCUP's separate
# Nationwide Readmissions Database, a different file entirely).
# "Complicated index hospitalization" (mortality / ICU-level care /
# prolonged LOS during THIS stay) is the closest answerable
# question using Core alone.
#
# WHY 3 SEPARATE OUTCOMES INSTEAD OF ONE "COMPLICATED" FLAG: a
# composite would force an arbitrary LOS-percentile cutoff and
# would hide which component (death vs. ICU-level care vs. long
# stay) is actually driving any result. Each outcome is reported
# on its own, same as the early-onset-cancer script's pattern.
#
# WHY MECHANICAL VENTILATION ONLY (not NIV) AS THE ICU-LEVEL-CARE
# PROXY: invasive ventilation's ICD-10-PCS code (5A19-) was
# already validated in earlier scripts. Noninvasive ventilation
# has a plausible but NOT yet verified PCS code family -- rather
# than guess it, this script sticks to the already-confirmed code.
#
# COHORT: Age 18+, PRINCIPAL diagnosis (I10_DX1) only -- same
# rationale as the early-onset cancer script: this captures
# admissions where the condition was the reason for THIS stay, not
# incidental history. ICD-10-CM code definitions (no decimal
# points, per NIS convention):
#   Pneumonia:  J12-J18 (viral/bacterial/other/unspecified)
#   CHF:        I50.x (all heart failure subtypes)
#   Sepsis:     A40-A41, R65.2x (same codes as the cancer+VTE and
#               early-onset cancer scripts)
#   COPD exac.: J44.0 (with acute lower resp infection), J44.1
#               (with acute exacerbation) -- EXCLUDES J44.9
#               (COPD, unspecified, no exacerbation indicated)
#   DKA:        E10.1x (Type 1), E11.1x (Type 2)
#
# ARCHITECTURE: same chunked base-R streaming approach as the
# other 2023 Core scripts -- full tidyverse/vroom pipelines were
# too slow on the real 6.74M-row file. Cohort-defining checks run
# on every row; the expensive mechanical-ventilation procedure
# scan only runs on this chunk's cohort rows.
# ============================================================

suppressMessages({
  library(tidyverse)
  library(survey)
})

nis_file       <- "~/Downloads/NIS_2023/NIS_2023_Core.ASC"
checkpoint_rds <- "~/Downloads/NIS_2023/nis_2023_complicated_hosp_checkpoint.rds"


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
pneumonia_regex <- "^J1[2-8]"
chf_regex       <- "^I50"
sepsis_regex    <- "^(A4[01]|R652)"
copd_regex      <- "^J44[01]"
dka_regex       <- "^(E101|E111)"

mechvent_regex  <- "^5A19"

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
    age_in_range <- scalar_df$AGE >= 18 & !is.na(scalar_df$AGE)

    scalar_df$diagnosis_category <- case_when(
      grepl(pneumonia_regex, dx1) ~ "Pneumonia",
      grepl(chf_regex, dx1)       ~ "CHF",
      grepl(sepsis_regex, dx1)    ~ "Sepsis",
      grepl(copd_regex, dx1)      ~ "COPD",
      grepl(dka_regex, dx1)       ~ "DKA",
      TRUE ~ NA_character_
    )
    scalar_df$cohort <- !is.na(scalar_df$diagnosis_category) & age_in_range

    cohort_mask <- scalar_df$cohort

    # Expensive check (procedure scan) -- only for this chunk's cohort rows
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
  cat("Total complicated-hospitalization cohort rows:", sum(full_derived$cohort), "\n")

  dir.create(dirname(checkpoint_rds), showWarnings = FALSE, recursive = TRUE)
  saveRDS(full_derived, checkpoint_rds)
  cat("Saved checkpoint to", checkpoint_rds, "\n")
}

full_derived <- full_derived %>%
  mutate(diagnosis_category = factor(diagnosis_category,
                                      levels = c("Pneumonia", "CHF", "Sepsis", "COPD", "DKA")))


# ----- 4. SURVEY DESIGN + SUBPOPULATION SUBSET -----
svy_design_ch <- svydesign(
  ids = ~HOSP_NIS, strata = ~NIS_STRATUM, weights = ~DISCWT,
  data = full_derived, nest = TRUE
)
svy_cohort_ch <- subset(svy_design_ch, cohort)

cat("\n========== COHORT SIZE ==========\n")
cat("Unweighted cohort n:", nrow(svy_cohort_ch$variables), "\n")
cat("Weighted (national estimate) cohort n:", round(sum(weights(svy_cohort_ch))), "\n")

cat("\nDiagnosis category distribution (unweighted):\n")
print(table(svy_cohort_ch$variables$diagnosis_category, useNA = "always"))


# ----- 5. HELPER FUNCTIONS -----

# Binary outcome: descriptive rate by diagnosis, adjusted OR
# (Pneumonia as reference -- most common admission type), cell
# counts, and within-diagnosis age/sex breakdown.
analyze_binary_by_diagnosis <- function(outcome_name, outcome_label) {
  cat("\n========== OUTCOME:", outcome_label, "==========\n")

  form_rate <- as.formula(paste0("~", outcome_name))
  cat("\nWeighted rate by diagnosis:\n")
  print(svyby(form_rate, ~diagnosis_category, svy_cohort_ch, svymean, na.rm = TRUE))

  form_adj <- as.formula(paste0(outcome_name, " ~ diagnosis_category + AGE + FEMALE"))
  model_adj <- svyglm(form_adj, design = svy_cohort_ch, family = quasibinomial())
  cat("\nAdjusted OR (vs. Pneumonia reference):\n")
  co <- coef(model_adj); ci <- confint(model_adj)
  or_table <- round(exp(cbind(OR = co, ci)), 3)
  print(or_table[grepl("diagnosis_category", rownames(or_table)), , drop = FALSE])

  cell_counts <- svy_cohort_ch$variables %>%
    filter(!is.na(diagnosis_category)) %>%
    count(diagnosis_category, .data[[outcome_name]]) %>%
    pivot_wider(names_from = all_of(outcome_name), values_from = n, values_fill = 0)
  cat("\nCell counts (diagnosis x outcome) -- flag anything sparse:\n")
  print(cell_counts)

  cat("\nWeighted rate by diagnosis x sex:\n")
  print(svyby(form_rate, ~diagnosis_category + FEMALE, svy_cohort_ch, svymean, na.rm = TRUE))

  invisible(model_adj)
}

# Continuous outcome (LOS): same structure, linear model
analyze_continuous_by_diagnosis <- function(outcome_name, outcome_label) {
  cat("\n========== OUTCOME:", outcome_label, "==========\n")

  form_rate <- as.formula(paste0("~", outcome_name))
  cat("\nWeighted mean by diagnosis:\n")
  print(svyby(form_rate, ~diagnosis_category, svy_cohort_ch, svymean, na.rm = TRUE))

  form_adj <- as.formula(paste0(outcome_name, " ~ diagnosis_category + AGE + FEMALE"))
  model_adj <- svyglm(form_adj, design = svy_cohort_ch, family = gaussian())
  cat("\nAdjusted difference in", outcome_label, "(vs. Pneumonia reference):\n")
  ci_table <- round(cbind(Estimate = coef(model_adj), confint(model_adj)), 3)
  print(ci_table[grepl("diagnosis_category", rownames(ci_table)), , drop = FALSE])

  cat("\nWeighted mean by diagnosis x sex:\n")
  print(svyby(form_rate, ~diagnosis_category + FEMALE, svy_cohort_ch, svymean, na.rm = TRUE))

  invisible(model_adj)
}


# ----- 6. RUN ALL OUTCOMES -----
result_mortality <- analyze_binary_by_diagnosis("DIED", "In-hospital mortality")
result_mechvent  <- analyze_binary_by_diagnosis("mech_vent", "Mechanical ventilation (ICU-level care proxy)")
result_los       <- analyze_continuous_by_diagnosis("LOS", "Length of stay")

cat("\n========== ALL OUTCOMES COMPLETE ==========\n")
