# ============================================================
# NIS 2023 Core: Rural vs. urban disparities in early-onset
# cancer hospitalizations
#
# RQ: Among adults aged 18-50 hospitalized with a PRINCIPAL
# cancer diagnosis in 2023, what are the differences in
# in-hospital complications, treatment intensity, and outcomes
# between rural and urban patients, stratified by sex and cancer
# type? Do disparities vary by cancer type?
#
# COHORT: Age 18-50 at admission, principal diagnosis (I10_DX1)
# is a malignant neoplasm (ICD-10-CM "C" code), EXCLUDING
# non-melanoma skin cancer (C44). Using the PRINCIPAL diagnosis
# only (not "any listed diagnosis") means this captures admissions
# where cancer was the reason for hospitalization, not patients
# admitted for something unrelated who happen to have a cancer
# history -- a deliberate scope decision, confirmed with the user.
#
# EXPOSURE: Rural vs. urban via PL_NCHS2 (NCHS Urban-Rural Code).
# CONFIRMED by HCUP User Support (email, see project records): 2023
# NIS replaced the old 6-category PL_NCHS with a simplified 2-category
# PL_NCHS2 due to a change in which states participate in 2023 NIS.
# PL_NCHS2 == 21 (metropolitan) is consistent with old PL_NCHS 1-4
# (large central/large fringe/medium/small metro) = Urban; PL_NCHS2
# == 22 (non-metropolitan) is consistent with old PL_NCHS 5-6
# (micropolitan/noncore) = Rural. This was the working hypothesis
# used throughout this script and is now empirically confirmed --
# no results need to be revisited.
#
# OUTCOMES: in-hospital sepsis (A40-A41, R65.2x), VTE (I26 PE +
# I82.4x acute lower-extremity DVT), AKI (N17), in-hospital
# mortality, length of stay.
#
# TREATMENT INTENSITY (proxies -- NIS Core has no ICU flag):
# major OR procedure (PCLASS_ORPROC), procedure count (I10_NPR),
# mechanical ventilation (ICD-10-PCS 5A19-, any duration).
# PCLASS_ORPROC's exact value coding is ALSO unconfirmed --
# Step 3 prints its raw distribution too.
#
# STRATIFICATION: sex, cancer type (10 categories). RACE IS NOT
# STRATIFIED -- confirmed absent from this NIS 2023 extract
# (checked Core, Hospital, and Severity file structures earlier
# in this project; see README).
#
# ARCHITECTURE: same chunked base-R streaming approach validated
# in the earlier cancer+VTE analysis -- full tidyverse/vroom
# pipelines were too slow/memory-heavy to reliably finish on
# ordinary laptop hardware against the real 6.74M-row file.
# Cohort-defining checks (principal dx, age) run on every row
# (required for correct survey subpopulation estimation); the
# expensive multi-code-category checks only run on this chunk's
# cohort rows.
# ============================================================

suppressMessages({
  library(tidyverse)
  library(survey)
})

nis_file       <- "~/Downloads/NIS_2023/NIS_2023_Core.ASC"
checkpoint_rds <- "~/Downloads/NIS_2023/nis_2023_early_onset_checkpoint.rds"


# ----- 1. COLUMN LAYOUT (from HCUP's SASload_NIS_2023_Core.SAS) -----
full_spec <- tibble::tribble(
  ~var, ~start, ~end, ~type, ~informat,
  "HOSP_NIS", 1L, 5L, "numeric", "N5PF",
  "KEY_NIS", 6L, 15L, "numeric", "N10PF",
  "NIS_STRATUM", 16L, 19L, "numeric", "N4PF",
  "AGE", 20L, 22L, "numeric", "N3PF",
  "DIED", 29L, 30L, "numeric", "N2PF",
  "DISCWT", 31L, 41L, "numeric", "N11P7F",
  "FEMALE", 56L, 57L, "numeric", "N2PF",
  "I10_NPR", 351L, 352L, "numeric", "N2PF",
  "LOS", 533L, 537L, "numeric", "N5PF",
  "PCLASS_ORPROC", 544L, 545L, "numeric", "N2PF",
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
cancer_regex               <- "^C"
skin_cancer_exclude_regex  <- "^C44"

sepsis_regex   <- "^(A4[01]|R652)"
vte_regex      <- "^(I26|I824)"
aki_regex      <- "^N17"
mechvent_regex <- "^5A19"

colorectal_regex <- "^C(18|19|20)"
breast_regex     <- "^C50"
pancreatic_regex <- "^C25"
esophageal_regex <- "^C15"
gyn_regex        <- "^C5[3-7]"
lung_regex       <- "^C34"
melanoma_regex   <- "^C43"
lymphoma_regex   <- "^C8[1-6]"
leukemia_regex   <- "^C9[1-5]"

any_match <- function(lines, starts, ends, regex) {
  hit <- rep(FALSE, length(lines))
  for (i in seq_along(starts)) {
    codes <- trimws(substr(lines, starts[i], ends[i]))
    hit <- hit | grepl(regex, codes)
  }
  hit
}


# ----- 3. STREAM THE FILE, WITH AN EARLY DIAGNOSTIC CHECK -----
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

    # ---- DIAGNOSTIC: check PL_NCHS2 and PCLASS_ORPROC coding on the ----
    # ---- first chunk before trusting the assumptions above ----
    if (!diagnostic_printed) {
      cat("\n========== DIAGNOSTIC: verify these before trusting results ==========\n")
      cat("PL_NCHS2 raw value distribution (first chunk):\n")
      print(table(scalar_df$PL_NCHS2, useNA = "always"))
      cat("\nExpected: values 1-6 only (NCHS urban-rural code). If you see\n")
      cat("anything else (e.g. just 0/1, or a totally different range),\n")
      cat("STOP and tell Claude before trusting the rural/urban results below.\n")
      cat("\nPCLASS_ORPROC raw value distribution (first chunk):\n")
      print(table(scalar_df$PCLASS_ORPROC, useNA = "always"))
      cat("\nExpected: this script treats PCLASS_ORPROC == 1 as 'major OR\n")
      cat("procedure present'. If the distribution suggests otherwise (e.g.\n")
      cat("a 0/1/2/3 multi-category scheme), STOP and tell Claude.\n")
      cat("======================================================================\n\n")
      diagnostic_printed <- TRUE
    }

    dx1 <- trimws(substr(lines, dx_starts[1], dx_ends[1]))
    is_cancer_principal <- grepl(cancer_regex, dx1) & !grepl(skin_cancer_exclude_regex, dx1)
    age_in_range <- scalar_df$AGE >= 18 & scalar_df$AGE <= 50 & !is.na(scalar_df$AGE)

    scalar_df$is_cancer_principal <- is_cancer_principal
    scalar_df$cohort <- is_cancer_principal & age_in_range

    # NOTE: rural/urban and major_or_proc are NOT derived here anymore --
    # see Section 3b below, which runs on full_derived regardless of
    # whether it came from a fresh scan or an existing checkpoint. This
    # keeps a fix to those mappings (like the one below) effective on
    # checkpoint reloads without needing to re-scan the whole file.

    cohort_mask <- scalar_df$cohort

    # Expensive checks -- only for this chunk's cohort rows
    scalar_df$cancer_type <- NA_character_
    scalar_df$sepsis <- NA; scalar_df$vte <- NA; scalar_df$aki <- NA; scalar_df$mech_vent <- NA

    if (any(cohort_mask)) {
      dx1_cohort <- dx1[cohort_mask]
      scalar_df$cancer_type[cohort_mask] <- case_when(
        grepl(colorectal_regex, dx1_cohort) ~ "Colorectal",
        grepl(breast_regex, dx1_cohort) ~ "Breast",
        grepl(pancreatic_regex, dx1_cohort) ~ "Pancreatic",
        grepl(esophageal_regex, dx1_cohort) ~ "Esophageal",
        grepl(gyn_regex, dx1_cohort) ~ "Ovarian/Gynecologic",
        grepl(lung_regex, dx1_cohort) ~ "Lung",
        grepl(melanoma_regex, dx1_cohort) ~ "Melanoma",
        grepl(lymphoma_regex, dx1_cohort) ~ "Lymphoma",
        grepl(leukemia_regex, dx1_cohort) ~ "Leukemia",
        TRUE ~ "Other"
      )

      sub_lines <- lines[cohort_mask]
      scalar_df$sepsis[cohort_mask]    <- any_match(sub_lines, dx_starts, dx_ends, sepsis_regex)
      scalar_df$vte[cohort_mask]       <- any_match(sub_lines, dx_starts, dx_ends, vte_regex)
      scalar_df$aki[cohort_mask]       <- any_match(sub_lines, dx_starts, dx_ends, aki_regex)
      scalar_df$mech_vent[cohort_mask] <- any_match(sub_lines, pr_starts, pr_ends, mechvent_regex)
    }

    all_chunks[[chunk_i]] <- scalar_df
    cat("Chunk", chunk_i, "--", n, "rows,", sum(cohort_mask), "cohort rows so far this chunk\n")
  }
  close(con)

  full_derived <- bind_rows(all_chunks)
  cat("\nTotal rows scanned:", nrow(full_derived), "\n")
  cat("Total early-onset cancer cohort rows:", sum(full_derived$cohort), "\n")

  dir.create(dirname(checkpoint_rds), showWarnings = FALSE, recursive = TRUE)
  saveRDS(full_derived, checkpoint_rds)
  cat("Saved checkpoint to", checkpoint_rds, "\n")
}


# ----- 3b. DERIVE rural/urban AND major_or_proc (runs every time, from ----
# ----- the raw columns, whether full_derived just came from a fresh   ----
# ----- scan or from the checkpoint) -----
#
# *** MAPPING CONFIRMED BY HCUP USER SUPPORT (email reply) ***
# The first diagnostic run showed PL_NCHS2 has only two values: 21 and
# 22 (not the expected 1-6 NCHS scale) -- 2023 NIS replaced PL_NCHS
# with this simplified 2-category version due to a change in which
# states participate in 2023 NIS. HCUP User Support confirmed directly:
# PL_NCHS2 == 21 (metropolitan) = Urban, consistent with old PL_NCHS
# 1-4; PL_NCHS2 == 22 (non-metropolitan) = Rural, consistent with old
# PL_NCHS 5-6. The mapping used throughout this script was correct.
cat("\nPL_NCHS2 mapping (21=Urban, 22=Rural) confirmed by HCUP User",
    "Support -- see project records.\n\n")

full_derived <- full_derived %>%
  mutate(
    rural = case_when(
      PL_NCHS2 == 21 ~ "Urban",
      PL_NCHS2 == 22 ~ "Rural",
      TRUE ~ NA_character_
    ),
    rural = factor(rural, levels = c("Urban", "Rural")),
    major_or_proc = PCLASS_ORPROC == 1
  )

cat("Post-fix rural/urban distribution (should now be non-zero):\n")
print(table(full_derived$rural, useNA = "always"))


# ----- 4. SURVEY DESIGN + SUBPOPULATION SUBSET -----
svy_design_eo <- svydesign(
  ids = ~HOSP_NIS, strata = ~NIS_STRATUM, weights = ~DISCWT,
  data = full_derived, nest = TRUE
)
svy_cohort_eo <- subset(svy_design_eo, cohort)

cat("\n========== COHORT SIZE ==========\n")
cat("Unweighted early-onset cancer cohort n:", nrow(svy_cohort_eo$variables), "\n")
cat("Weighted (national estimate) cohort n:", round(sum(weights(svy_cohort_eo))), "\n")

cat("\nSex distribution:\n")
print(table(svy_cohort_eo$variables$FEMALE, useNA = "always"))
cat("\nCancer type distribution:\n")
print(table(svy_cohort_eo$variables$cancer_type, useNA = "always"))
cat("\nRural/urban distribution:\n")
print(table(svy_cohort_eo$variables$rural, useNA = "always"))


# ----- 5. HELPER FUNCTIONS FOR REPEATED ANALYSIS PATTERN -----

# Binary outcome: descriptive rate by rural/urban, adjusted OR,
# sex-stratified rate, and cancer-type interaction (global test +
# cell-count-aware descriptive, same sparse-cell-safe pattern used
# in the insurance/IVC filter analysis).
analyze_binary_outcome <- function(outcome_name, outcome_label) {
  cat("\n========== OUTCOME:", outcome_label, "==========\n")

  form_rate <- as.formula(paste0("~", outcome_name))
  cat("\nWeighted rate by rural/urban:\n")
  print(svyby(form_rate, ~rural, svy_cohort_eo, svymean, na.rm = TRUE))

  form_adj <- as.formula(paste0(outcome_name, " ~ rural + AGE + FEMALE + cancer_type"))
  model_adj <- svyglm(form_adj, design = svy_cohort_eo, family = quasibinomial())
  cat("\nAdjusted OR (Rural vs. Urban, ref = Urban):\n")
  co <- coef(model_adj); ci <- confint(model_adj)
  print(round(exp(cbind(OR = co, ci))["ruralRural", , drop = FALSE], 3))

  cat("\nWeighted rate by rural/urban x sex:\n")
  print(svyby(form_rate, ~rural + FEMALE, svy_cohort_eo, svymean, na.rm = TRUE))

  cell_counts <- svy_cohort_eo$variables %>%
    filter(!is.na(rural), !is.na(cancer_type)) %>%
    count(rural, cancer_type, .data[[outcome_name]]) %>%
    pivot_wider(names_from = all_of(outcome_name), values_from = n, values_fill = 0)
  cat("\nCell counts (rural x cancer_type x outcome) -- flag anything sparse:\n")
  print(cell_counts, n = 30)

  form_interact <- as.formula(paste0(outcome_name, " ~ rural * cancer_type + AGE + FEMALE"))
  model_interact <- svyglm(form_interact, design = svy_cohort_eo, family = quasibinomial())
  test_result <- regTermTest(model_interact, ~rural:cancer_type)
  cat("\nGlobal test: does the rural/urban disparity vary by cancer type?\n")
  cat("(valid even with sparse cells -- individual coefficients not reported)\n")
  print(test_result)

  cat("\nDescriptive: rate by rural/urban within each cancer type\n")
  cat("(compare against cell counts above before trusting any specific rate):\n")
  print(svyby(form_rate, ~cancer_type + rural, svy_cohort_eo, svymean, na.rm = TRUE))

  invisible(list(model_adj = model_adj, model_interact = model_interact, test = test_result))
}

# Continuous outcome (LOS, procedure count): same structure, linear model
analyze_continuous_outcome <- function(outcome_name, outcome_label) {
  cat("\n========== OUTCOME:", outcome_label, "==========\n")

  form_rate <- as.formula(paste0("~", outcome_name))
  cat("\nWeighted mean by rural/urban:\n")
  print(svyby(form_rate, ~rural, svy_cohort_eo, svymean, na.rm = TRUE))

  form_adj <- as.formula(paste0(outcome_name, " ~ rural + AGE + FEMALE + cancer_type"))
  model_adj <- svyglm(form_adj, design = svy_cohort_eo, family = gaussian())
  cat("\nAdjusted difference (Rural vs. Urban, ref = Urban):\n")
  print(round(cbind(Estimate = coef(model_adj), confint(model_adj))["ruralRural", , drop = FALSE], 3))

  cat("\nWeighted mean by rural/urban x sex:\n")
  print(svyby(form_rate, ~rural + FEMALE, svy_cohort_eo, svymean, na.rm = TRUE))

  cat("\nWeighted mean by rural/urban within each cancer type:\n")
  print(svyby(form_rate, ~cancer_type + rural, svy_cohort_eo, svymean, na.rm = TRUE))

  invisible(model_adj)
}


# ----- 6. RUN ALL OUTCOMES -----
result_sepsis    <- analyze_binary_outcome("sepsis", "In-hospital sepsis")
result_vte       <- analyze_binary_outcome("vte", "VTE")
result_aki       <- analyze_binary_outcome("aki", "Acute kidney injury")
result_mortality <- analyze_binary_outcome("DIED", "In-hospital mortality")
result_mechvent  <- analyze_binary_outcome("mech_vent", "Mechanical ventilation (treatment intensity)")
result_majoror   <- analyze_binary_outcome("major_or_proc", "Major OR procedure (treatment intensity)")

result_los       <- analyze_continuous_outcome("LOS", "Length of stay (treatment intensity)")
result_nproc     <- analyze_continuous_outcome("I10_NPR", "Number of procedures (treatment intensity)")

cat("\n========== ALL OUTCOMES COMPLETE ==========\n")
