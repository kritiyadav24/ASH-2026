# ============================================================
# NIS 2023: Does the cancer-associated reperfusion-treatment
# divergence (less thrombolysis, more thrombectomy) vary by
# hospital teaching status or bed size?
#
# WHY THIS QUESTION: the primary stroke-cancer script found cancer
# patients receive significantly less IV thrombolysis but
# significantly more mechanical thrombectomy than non-cancer
# stroke patients. That could reflect either (a) a genuine
# access/facility-capability story -- e.g., cancer patients happen
# to be triaged toward hospitals with thrombectomy capability -- or
# (b) a patient-level clinical decision-making story (e.g.,
# thrombolysis contraindications common in active cancer:
# thrombocytopenia, recent anticoagulation, bleeding risk) that
# would hold regardless of what hospital they're at. Stratifying
# by hospital teaching status/bed size -- a standard access proxy,
# since rural and non-teaching hospitals have much lower baseline
# thrombectomy capability -- helps distinguish these: if the
# cancer-treatment gap persists even at rural/non-teaching
# hospitals, that argues against a pure facility-access
# explanation.
#
# DATA: reuses the existing stroke-cancer cohort checkpoint
# (nis_2023_stroke_cancer_checkpoint.rds, built by
# nis_2023_stroke_cancer_treatment_access.R) and merges in
# hospital-level characteristics from the separate NIS 2023
# Hospital file by HOSP_NIS.
#
# HOSPITAL FILE LAYOUT: no load program was available to confirm
# byte positions directly, so positions were reverse-engineered
# from the raw ASCII file structure (60-char fixed-width, 4,181
# rows matching HCUP's published HOSP_NIS range of 1-4,181) and
# VERIFIED by matching computed category distributions against
# HCUP's own published 2023 statistics -- not guessed:
#   HOSP_BEDSIZE (cols 21-22): computed 57.36% / 22.27% / 20.38%
#     for Small/Medium/Large vs. HCUP-published 57.35% / 22.27% /
#     20.38% -- exact match.
#   HOSP_LOCTEACH (cols 23-24): computed 38.12% Rural vs.
#     HCUP-published 38.12% Rural -- exact match.
#   HOSP_NIS (cols 1-5): sequential 1-4181, matches HCUP's stated
#     range exactly.
# ============================================================

suppressMessages({
  library(tidyverse)
  library(survey)
})

checkpoint_rds <- "~/Downloads/NIS_2023/nis_2023_stroke_cancer_checkpoint.rds"
hospital_file  <- "~/Downloads/NIS_2023/NIS_2023_Hospital.ASC"

if (!file.exists(checkpoint_rds)) {
  stop("Stroke-cancer checkpoint not found. Run nis_2023_stroke_cancer_treatment_access.R first.")
}
full_derived <- readRDS(checkpoint_rds)
cat("Loaded stroke-cancer checkpoint:", nrow(full_derived), "total rows\n")

# The checkpoint is saved BEFORE cancer_status is derived in the original
# script (that derivation happens in-memory only) -- re-derive it here.
full_derived <- full_derived %>%
  mutate(
    cancer_status = case_when(
      cohort & has_cancer ~ "Cancer",
      cohort & !has_cancer ~ "No cancer",
      TRUE ~ NA_character_
    ),
    cancer_status = factor(cancer_status, levels = c("No cancer", "Cancer")),
    cancer_type = factor(cancer_type,
                          levels = c("Colorectal", "Breast", "Pancreatic", "Esophageal",
                                     "Ovarian/Gynecologic", "Lung", "Melanoma", "Lymphoma",
                                     "Leukemia", "Other"))
  )


# ----- 1. LOAD + VERIFY HOSPITAL FILE -----
hosp_lines <- readLines(hospital_file)
cat("Hospital file rows:", length(hosp_lines), "\n")

hosp_df <- data.frame(
  HOSP_NIS      = as.numeric(substr(hosp_lines, 1, 5)),
  HOSP_BEDSIZE  = as.numeric(substr(hosp_lines, 21, 22)),
  HOSP_LOCTEACH = as.numeric(substr(hosp_lines, 23, 24))
)

cat("\nVERIFICATION -- these must match HCUP's published 2023 statistics:\n")
cat("HOSP_BEDSIZE distribution (expect ~57.35 / 22.27 / 20.38):\n")
print(round(100 * prop.table(table(hosp_df$HOSP_BEDSIZE)), 2))
cat("HOSP_LOCTEACH distribution (expect ~38.12 for category 1, Rural):\n")
print(round(100 * prop.table(table(hosp_df$HOSP_LOCTEACH)), 2))
cat("HOSP_NIS range (expect 1 to 4181):", range(hosp_df$HOSP_NIS), "\n")

hosp_df <- hosp_df %>%
  mutate(
    bedsize_label  = factor(HOSP_BEDSIZE, levels = c(1, 2, 3),
                             labels = c("Small", "Medium", "Large")),
    locteach_label = factor(HOSP_LOCTEACH, levels = c(1, 2, 3),
                             labels = c("Rural", "Urban nonteaching", "Urban teaching"))
  ) %>%
  select(HOSP_NIS, bedsize_label, locteach_label)


# ----- 2. MERGE ONTO STROKE COHORT -----
full_derived <- full_derived %>% left_join(hosp_df, by = "HOSP_NIS")

cat("\n========== MERGE CHECK ==========\n")
cohort_rows <- full_derived %>% filter(cohort)
cat("Cohort rows with missing hospital match:",
    sum(is.na(cohort_rows$locteach_label)), "of", nrow(cohort_rows), "\n")


# ----- 3. SURVEY DESIGN + SUBPOPULATION SUBSET -----
svy_design_stroke <- svydesign(
  ids = ~HOSP_NIS, strata = ~NIS_STRATUM, weights = ~DISCWT,
  data = full_derived, nest = TRUE
)
svy_cohort_stroke <- subset(svy_design_stroke, cohort)

cat("\n========== HOSPITAL CHARACTERISTICS WITHIN STROKE COHORT ==========\n")
cat("Teaching status distribution:\n")
print(table(svy_cohort_stroke$variables$locteach_label, useNA = "always"))
cat("Bed size distribution:\n")
print(table(svy_cohort_stroke$variables$bedsize_label, useNA = "always"))


# ----- 4. CELL COUNTS BEFORE TRUSTING ANY STRATUM-SPECIFIC ESTIMATE -----
cat("\n========== CELL COUNTS: cancer_status x locteach_label x treatment ==========\n")
cell_counts_lysis <- svy_cohort_stroke$variables %>%
  filter(!is.na(cancer_status), !is.na(locteach_label)) %>%
  count(locteach_label, cancer_status, has_thrombolysis)
cat("\nThrombolysis cell counts -- flag anything sparse:\n")
print(as.data.frame(cell_counts_lysis))

cell_counts_ect <- svy_cohort_stroke$variables %>%
  filter(!is.na(cancer_status), !is.na(locteach_label)) %>%
  count(locteach_label, cancer_status, has_thrombectomy)
cat("\nThrombectomy cell counts -- flag anything sparse:\n")
print(as.data.frame(cell_counts_ect))


# ----- 5. STRATIFIED DESCRIPTIVE RATES -----
cat("\n========== Thrombolysis rate by cancer status x teaching status ==========\n")
print(svyby(~has_thrombolysis, ~cancer_status + locteach_label, svy_cohort_stroke, svymean, na.rm = TRUE))

cat("\n========== Thrombectomy rate by cancer status x teaching status ==========\n")
print(svyby(~has_thrombectomy, ~cancer_status + locteach_label, svy_cohort_stroke, svymean, na.rm = TRUE))


# ----- 6. STRATUM-SPECIFIC ADJUSTED ORs (cancer vs. no cancer, within each teaching-status level) -----
for (lvl in levels(svy_cohort_stroke$variables$locteach_label)) {
  cat("\n========== Within", lvl, "==========\n")
  svy_stratum <- subset(svy_cohort_stroke, locteach_label == lvl)

  model_lysis_s <- svyglm(has_thrombolysis ~ cancer_status + AGE + FEMALE,
                           design = svy_stratum, family = quasibinomial())
  cat("Thrombolysis adjusted OR (Cancer vs. No cancer):\n")
  print(round(exp(cbind(OR = coef(model_lysis_s), confint(model_lysis_s))), 3))
  rm(model_lysis_s); gc()

  model_ect_s <- svyglm(has_thrombectomy ~ cancer_status + AGE + FEMALE,
                         design = svy_stratum, family = quasibinomial())
  cat("Thrombectomy adjusted OR (Cancer vs. No cancer):\n")
  print(round(exp(cbind(OR = coef(model_ect_s), confint(model_ect_s))), 3))
  rm(model_ect_s); gc()
}


# ----- 7. FORMAL INTERACTION TEST: does the cancer effect on treatment vary by teaching status? -----
cat("\n========== INTERACTION TEST: cancer_status x locteach_label ==========\n")

model_lysis_int <- svyglm(has_thrombolysis ~ cancer_status * locteach_label + AGE + FEMALE,
                           design = svy_cohort_stroke, family = quasibinomial())
cat("Thrombolysis -- interaction term test:\n")
print(regTermTest(model_lysis_int, ~cancer_status:locteach_label))
rm(model_lysis_int); gc()

model_ect_int <- svyglm(has_thrombectomy ~ cancer_status * locteach_label + AGE + FEMALE,
                         design = svy_cohort_stroke, family = quasibinomial())
cat("Thrombectomy -- interaction term test:\n")
print(regTermTest(model_ect_int, ~cancer_status:locteach_label))
rm(model_ect_int); gc()


# ----- 8. SECONDARY: SAME QUESTIONS BY BED SIZE -----
cat("\n========== SECONDARY: by bed size ==========\n")

cat("\nThrombolysis rate by cancer status x bed size:\n")
print(svyby(~has_thrombolysis, ~cancer_status + bedsize_label, svy_cohort_stroke, svymean, na.rm = TRUE))
cat("\nThrombectomy rate by cancer status x bed size:\n")
print(svyby(~has_thrombectomy, ~cancer_status + bedsize_label, svy_cohort_stroke, svymean, na.rm = TRUE))

model_lysis_bed_int <- svyglm(has_thrombolysis ~ cancer_status * bedsize_label + AGE + FEMALE,
                               design = svy_cohort_stroke, family = quasibinomial())
cat("\nThrombolysis -- cancer_status x bedsize_label interaction test:\n")
print(regTermTest(model_lysis_bed_int, ~cancer_status:bedsize_label))
rm(model_lysis_bed_int); gc()

model_ect_bed_int <- svyglm(has_thrombectomy ~ cancer_status * bedsize_label + AGE + FEMALE,
                             design = svy_cohort_stroke, family = quasibinomial())
cat("\nThrombectomy -- cancer_status x bedsize_label interaction test:\n")
print(regTermTest(model_ect_bed_int, ~cancer_status:bedsize_label))
rm(model_ect_bed_int); gc()

cat("\n========== ALL ANALYSES COMPLETE ==========\n")
