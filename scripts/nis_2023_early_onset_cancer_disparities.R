# ============================================================
# HCUP NIS 2023: Rural-Urban Disparities in Early-Onset Cancer
# Hospitalizations
#
# Research Question: Among adults aged 18-50 hospitalized with early-onset
# cancer in 2023, what are the differences in in-hospital complications,
# treatment intensity, and outcomes between rural and urban patients,
# stratified by sex and race/ethnicity? Do disparities vary by cancer type?
#
# Data source:      HCUP National Inpatient Sample (NIS), 2023 core file
#                    (requires an HCUP Data Use Agreement; NIS is
#                    discharge-level, not patient-level, and cannot be
#                    redistributed -- only aggregated output belongs in
#                    this repo, per HCUP DUA terms)
#
# Population:        Age 18-50 at admission; principal diagnosis (I10_DX1)
#                    in C00-C97, excluding C44 (non-melanoma skin cancer)
# Exposure:           Rural vs. urban patient residence (PL_NCHS)
# Primary outcomes:   In-hospital infection/sepsis, VTE, AKI, in-hospital
#                    mortality, length of stay
# Treatment intensity: Major OR procedure, mechanical ventilation,
#                    blood/blood product transfusion, number of
#                    procedures, total charges
# Stratification:     Sex, race/ethnicity, cancer type (colorectal,
#                    breast, pancreatic, esophageal, ovarian/gynecologic,
#                    lung, melanoma, lymphoma, leukemia, other)
#
# IMPORTANT ASSUMPTIONS -- verify each against the 2023 NIS data
# elements documentation and your local extract before trusting output:
# - Cancer cohort is defined by PRINCIPAL diagnosis (I10_DX1) only, i.e.
#   "hospitalized primarily for cancer/cancer-related care." If you want
#   "hospitalized with a history of/comorbid cancer" instead, switch to
#   the any-listed-diagnosis version in Section 6b.
# - Rural/urban uses PL_NCHS (6-level 2013 NCHS urban-rural scheme:
#   1-4 = metro, 5-6 = micropolitan/noncore = rural). Some NIS vintages
#   name this variable PL_NCHS2006 -- check names(nis) in Section 3.
# - ICD-10-CM/PCS code lists for complications and procedures below are
#   built from standard code-block definitions but are NOT a validated
#   claims-based algorithm (e.g., not the AHRQ PSI or CDC Adult Sepsis
#   Event definitions). Treat as a reasonable starting point and refine
#   against a validated algorithm before publication.
# - Antineoplastic (chemotherapy) administration is NOT flagged from
#   ICD-10-PCS codes here -- coding varies too much by route/substance to
#   state a reliable prefix without your codebook in hand. If you need
#   it, add it in Section 6d once verified.
# ============================================================


# ----- 1. LOAD PACKAGES -----
# install.packages(c("tidyverse","survey","gtsummary","broom","data.table"))
library(tidyverse)
library(survey)
library(gtsummary)
library(broom)
library(data.table)

options(survey.lonely.psu = "adjust")


# ----- 2. LOAD DATA -----
# NIS is distributed as fixed-width ASCII with SAS/SPSS/Stata load
# programs, or as CSV via the HCUP SID/NIS tools depending on how you
# obtained it. This assumes you've already produced a flat CSV; update
# the path and swap in your own loader (e.g., a SAS-program-based reader)
# if you're working from the raw ASCII files.
#
# If the full NIS core file is too large to work with directly (multi-GB),
# run 00_prefilter_nis_2023_local.R locally first -- it streams the file
# via DuckDB, restricts to the 18-50/cancer cohort, and keeps only the
# columns this script needs, producing a much smaller CSV. Set
# PREFILTERED <- TRUE below if you're loading that output instead of the
# raw core file; it skips the redundant age/cancer restriction in
# Section 6a-6b since the pre-filter already applied it.
PREFILTERED <- TRUE
nis <- fread("~/Downloads/nis_2023_cancer_cohort_prefiltered.csv")

cat("Rows loaded:", nrow(nis), "\n")
cat("Columns:", ncol(nis), "\n")


# ----- 3. VERIFY RAW VARIABLES BEFORE CLEANING -----
# Confirm required variables exist and codes match expectations for
# your extract before proceeding -- variable names/coding have changed
# across NIS vintages.
required_vars <- c(
  "AGE", "FEMALE", "RACE", "PAY1", "ZIPINC_QRTL", "PL_NCHS",
  "LOS", "DIED", "TOTCHG", "NPR", "ORPROC", "ELECTIVE",
  "HOSP_REGION", "HOSP_TEACH", "HOSP_BEDSIZE",
  "APRDRG_Risk_Mortality", "APRDRG_Severity",
  "HOSP_NIS", "NIS_STRATUM", "DISCWT", "YEAR"
)
cat("\nMissing expected variables (check naming for your NIS vintage):\n")
print(setdiff(required_vars, names(nis)))

dx_cols <- grep("^I10_DX[0-9]+$", names(nis), value = TRUE)
pr_cols <- grep("^I10_PR[0-9]+$", names(nis), value = TRUE)
cat("\nDiagnosis columns found:", length(dx_cols), "\n")
cat("Procedure columns found:", length(pr_cols), "\n")

cat("\n--- PL_NCHS ---\n");  print(table(nis$PL_NCHS, useNA = "ifany"))
cat("\n--- RACE ---\n");     print(table(nis$RACE, useNA = "ifany"))
cat("\n--- FEMALE ---\n");   print(table(nis$FEMALE, useNA = "ifany"))
cat("\n--- DIED ---\n");     print(table(nis$DIED, useNA = "ifany"))
cat("\n--- YEAR ---\n");     print(table(nis$YEAR, useNA = "ifany"))
cat("\n--- Sample I10_DX1 codes ---\n")
print(head(sort(table(nis$I10_DX1), decreasing = TRUE), 15))


# ----- 4. HELPER: FLAG ANY DIAGNOSIS/PROCEDURE MATCHING A REGEX -----
# Vectorized across all dx/pr columns -- much faster on NIS-scale data
# (millions of rows) than row-wise apply().
flag_any_code <- function(df, cols, pattern) {
  hit_mat <- sapply(cols, function(cn) grepl(pattern, df[[cn]], perl = TRUE))
  rowSums(hit_mat, na.rm = TRUE) > 0
}


# ----- 5. ICD-10-CM/PCS CODE DEFINITIONS -----
# Codes are stored without decimal points in HCUP files (e.g., "C5091",
# "N170"), so prefix matching on the undotted category code is correct.

# --- 5a. Any cancer, C00-C97 excluding C44 (non-melanoma skin cancer) ---
cancer_prefixes <- sprintf("C%02d", c(0:43, 45:97))
cancer_regex    <- paste0("^(", paste(cancer_prefixes, collapse = "|"), ")")

# --- 5b. Cancer type sub-categories (used to classify the principal dx) ---
cancer_type_defs <- list(
  Colorectal              = sprintf("C%02d", 18:20),
  Breast                  = "C50",
  Pancreatic              = "C25",
  Esophageal              = "C15",
  `Ovarian/Gynecologic`   = sprintf("C%02d", 51:58),
  Lung                    = c("C33", "C34"),
  Melanoma                = "C43",
  Lymphoma                = c(sprintf("C%02d", 81:86), "C88"),
  Leukemia                = sprintf("C%02d", 91:95)
)
cancer_type_defs <- lapply(cancer_type_defs,
                            function(p) paste0("^(", paste(p, collapse = "|"), ")"))

# --- 5c. In-hospital complications ---
# Sepsis + selected serious infections (composite "infection/sepsis"
# outcome per the research question). Narrow to just the sepsis codes
# (A40|A41|R6520|R6521|T8144) if you want a stricter, sepsis-only outcome.
infection_sepsis_regex <- paste0(
  "^(A40|A41|R6520|R6521|T8144",              # sepsis / septic shock
  "|J1[2-8]",                                  # pneumonia
  "|N390",                                     # UTI
  "|L03",                                      # cellulitis
  "|K65[019]",                                 # peritonitis
  "|D70)"                                      # febrile neutropenia
)

# VTE: pulmonary embolism (I26) + deep vein thrombosis/other venous
# thrombosis (I80, I82). Note I80 nominally includes superficial
# thrombophlebitis (I80.0); narrow to "^(I26|I80[1-3]|I82)" for a
# stricter DVT/PE-only definition if superficial phlebitis inflates counts.
vte_regex <- "^(I26|I80|I82)"

# AKI
aki_regex <- "^N17"

# --- 5d. Treatment intensity procedures ---
# Mechanical ventilation (ICD-10-PCS 5A1935Z/5A1945Z/5A1955Z, all durations)
mech_vent_regex <- "^5A19"

# Blood/blood product transfusion (ICD-10-PCS Administration/Circulatory/
# Transfusion = section-bodysystem-operation "302"). Verify against the
# CMS ICD-10-PCS code tables for your data year before relying on this.
transfusion_regex <- "^302"


# ----- 6. COHORT CONSTRUCTION -----

# --- 6a-6b. Age/year restriction and principal-dx cancer cohort ---
# Skipped when PREFILTERED = TRUE (Section 2), since
# 00_prefilter_nis_2023_local.R already applied both restrictions before
# this file was written out. Re-applying here is harmless either way
# (both filters are idempotent) but the row-count logging below is only
# meaningful the first time the restriction is actually applied.
if (isTRUE(PREFILTERED)) {
  cohort <- nis
  cat("\nUsing pre-filtered cohort (age 18-50, principal-dx cancer already applied):",
      nrow(cohort), "\n")
} else {
  cohort <- nis %>%
    filter(YEAR == 2023, AGE >= 18, AGE <= 50)
  cat("\nAfter age/year restriction:", nrow(cohort), "\n")

  cohort <- cohort %>%
    filter(grepl(cancer_regex, I10_DX1, perl = TRUE))
  cat("After restricting to principal-dx cancer (C00-C97 excl. C44):",
      nrow(cohort), "\n")
}

# Alternative ("any-listed" cancer, i.e., hospitalized WITH a cancer
# diagnosis rather than primarily FOR cancer care) -- use as a sensitivity
# cohort in Section 15, not as the primary definition:
# cohort_any <- nis %>%
#   filter(YEAR == 2023, AGE >= 18, AGE <= 50) %>%
#   filter(flag_any_code(., dx_cols, cancer_regex))

# --- 6c. Classify cancer type from the principal diagnosis ---
classify_cancer_type <- function(dx1) {
  hits <- vapply(cancer_type_defs, function(rx) grepl(rx, dx1, perl = TRUE),
                  logical(length(dx1)))
  # hits is a length(dx1) x length(cancer_type_defs) matrix
  out <- apply(hits, 1, function(r) {
    nm <- names(cancer_type_defs)[r]
    if (length(nm) == 0) "Other" else nm[1]
  })
  out
}
cohort <- cohort %>%
  mutate(cancer_type = classify_cancer_type(I10_DX1),
         cancer_type = factor(cancer_type,
                               levels = c("Colorectal", "Breast", "Pancreatic",
                                          "Esophageal", "Ovarian/Gynecologic",
                                          "Lung", "Melanoma", "Lymphoma",
                                          "Leukemia", "Other")))

cat("\nCancer type distribution (unweighted):\n")
print(table(cohort$cancer_type, useNA = "always"))


# ----- 7. EXPOSURE: RURAL VS. URBAN -----
# PL_NCHS: 1=Large central metro, 2=Large fringe metro, 3=Medium metro,
# 4=Small metro, 5=Micropolitan, 6=Noncore (non-metro). 1-4 = urban,
# 5-6 = rural, per the standard NCHS urban-rural classification.
cohort <- cohort %>%
  mutate(
    rural = case_when(
      PL_NCHS %in% c(1, 2, 3, 4) ~ 0,
      PL_NCHS %in% c(5, 6)       ~ 1,
      TRUE ~ NA_real_
    ),
    rural_label = factor(if_else(rural == 1, "Rural", "Urban"),
                          levels = c("Urban", "Rural"))
  )


# ----- 8. OUTCOMES -----

# --- 8a. Complications (searched across ALL listed diagnoses, not just
#     principal, since complications are typically secondary diagnoses) ---
cohort <- cohort %>%
  mutate(
    infection_sepsis = as.numeric(flag_any_code(cohort, dx_cols, infection_sepsis_regex)),
    vte              = as.numeric(flag_any_code(cohort, dx_cols, vte_regex)),
    aki              = as.numeric(flag_any_code(cohort, dx_cols, aki_regex))
  )

# --- 8b. Mortality and length of stay ---
cohort <- cohort %>%
  mutate(
    died = as.numeric(DIED),
    los  = as.numeric(LOS)
  ) %>%
  filter(!is.na(los), los >= 0)   # drop invalid/missing LOS records

# --- 8c. Treatment intensity ---
cohort <- cohort %>%
  mutate(
    mech_vent   = as.numeric(flag_any_code(cohort, pr_cols, mech_vent_regex)),
    transfusion = as.numeric(flag_any_code(cohort, pr_cols, transfusion_regex)),
    orproc      = as.numeric(ORPROC),
    npr         = as.numeric(NPR),
    totchg      = as.numeric(TOTCHG)
  )


# ----- 9. COVARIATES -----
cohort <- cohort %>%
  mutate(
    sex = factor(if_else(FEMALE == 1, "Female", "Male"),
                 levels = c("Male", "Female")),

    # RACE: 1=White, 2=Black, 3=Hispanic, 4=Asian/Pacific Islander,
    # 5=Native American, 6=Other (uniform HCUP coding; missing = blank/NA)
    race_eth = case_when(
      RACE == 1 ~ "White",
      RACE == 2 ~ "Black",
      RACE == 3 ~ "Hispanic",
      RACE == 4 ~ "Asian/Pacific Islander",
      RACE == 5 ~ "Native American",
      RACE == 6 ~ "Other",
      TRUE ~ NA_character_
    ),
    race_eth = factor(race_eth,
                       levels = c("White", "Black", "Hispanic",
                                  "Asian/Pacific Islander",
                                  "Native American", "Other")),

    income_qrtl = factor(ZIPINC_QRTL, levels = 1:4,
                          labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)")),

    # PAY1: 1=Medicare, 2=Medicaid, 3=Private incl. HMO, 4=Self-pay,
    # 5=No charge, 6=Other
    payer = case_when(
      PAY1 == 1 ~ "Medicare",
      PAY1 == 2 ~ "Medicaid",
      PAY1 == 3 ~ "Private/HMO",
      PAY1 == 4 ~ "Self-pay",
      PAY1 %in% c(5, 6) ~ "Other/No charge",
      TRUE ~ NA_character_
    ),
    payer = factor(payer, levels = c("Private/HMO", "Medicare", "Medicaid",
                                      "Self-pay", "Other/No charge")),

    hosp_region = factor(HOSP_REGION, levels = 1:4,
                          labels = c("Northeast", "Midwest", "South", "West")),
    hosp_teach  = factor(HOSP_TEACH, levels = c(0, 1),
                          labels = c("Non-teaching", "Teaching")),
    elective    = factor(ELECTIVE, levels = c(0, 1),
                          labels = c("Non-elective", "Elective")),

    # 3M APR-DRG severity/mortality risk subclasses, supplied directly by
    # HCUP -- used here as the primary comorbidity/severity adjuster
    # instead of a re-derived Elixhauser index, since Elixhauser's
    # "solid tumor"/"metastatic cancer" categories are definitionally
    # present in this cohort and would need to be dropped to avoid
    # circularity. If you prefer Elixhauser for non-cancer comorbidities
    # (e.g., diabetes, renal disease, CHF) alongside APR-DRG severity,
    # compute it with the `comorbidity` package on dx_cols and exclude
    # the tumor/metastasis categories before adding it as a covariate.
    severity  = factor(APRDRG_Severity, levels = 1:4,
                        labels = c("Minor", "Moderate", "Major", "Extreme")),
    risk_mort = factor(APRDRG_Risk_Mortality, levels = 1:4,
                        labels = c("Minor", "Moderate", "Major", "Extreme"))
  )


# ----- 10. FEASIBILITY CHECK -----
cat("\n========== FEASIBILITY CHECK ==========\n")
cat("Final cohort N:", nrow(cohort), "\n")
cat("\nRural/urban distribution:\n"); print(table(cohort$rural_label, useNA = "always"))
cat("\nSex distribution:\n");         print(table(cohort$sex, useNA = "always"))
cat("\nRace/ethnicity distribution:\n"); print(table(cohort$race_eth, useNA = "always"))
cat("\nCancer type x rural/urban (unweighted):\n")
print(table(cohort$cancer_type, cohort$rural_label, useNA = "always"))

cat("\nMissingness summary:\n")
for (v in c("rural_label", "sex", "race_eth", "income_qrtl", "payer",
            "severity", "risk_mort", "los", "died")) {
  cat(sprintf("  %-15s missing: %d\n", v, sum(is.na(cohort[[v]]))))
}

# Drop records missing on exposure or key covariates before modeling
model_cohort <- cohort %>%
  filter(!is.na(rural_label), !is.na(sex), !is.na(race_eth),
         !is.na(income_qrtl), !is.na(payer), !is.na(severity),
         !is.na(risk_mort), !is.na(DISCWT), !is.na(HOSP_NIS),
         !is.na(NIS_STRATUM))

cat("\nN after dropping missing exposure/covariates:", nrow(model_cohort), "\n")


# ----- 11. SURVEY DESIGN -----
# NIS complex survey design: HOSP_NIS = hospital-level PSU, NIS_STRATUM =
# stratum, DISCWT = discharge weight (nationally representative).
svy_design <- svydesign(
  id      = ~HOSP_NIS,
  strata  = ~NIS_STRATUM,
  weights = ~DISCWT,
  data    = model_cohort,
  nest    = TRUE
)


# ----- 12. TABLE 1 -- WEIGHTED DESCRIPTIVES BY RURAL/URBAN -----
tbl1 <- model_cohort %>%
  select(rural_label, AGE, sex, race_eth, income_qrtl, payer, elective,
         hosp_region, hosp_teach, cancer_type, severity, risk_mort,
         infection_sepsis, vte, aki, died, los, orproc, mech_vent,
         transfusion, npr, totchg) %>%
  tbl_summary(
    by = rural_label,
    missing = "no",
    statistic = list(
      all_continuous()  ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    )
  ) %>%
  add_p(
    test = list(all_continuous() ~ "wilcox.test",
                all_categorical() ~ "chisq.test")
  ) %>%
  add_overall() %>%
  bold_labels()

print(tbl1)


# ----- 13. PRIMARY ANALYSIS -- OVERALL RURAL VS. URBAN -----
# Adjustment set used throughout: age, sex, race/ethnicity, income
# quartile, payer, hospital region, hospital teaching status, elective
# admission, cancer type, APR-DRG severity and risk of mortality.
adj_covars <- "AGE + sex + race_eth + income_qrtl + payer + hosp_region +
               hosp_teach + elective + cancer_type + severity + risk_mort"

binary_outcomes <- c("infection_sepsis", "vte", "aki", "died",
                      "orproc", "mech_vent", "transfusion")

fit_binary <- function(outcome, design, covars = NULL) {
  f_unadj <- as.formula(paste(outcome, "~ rural_label"))
  f_adj   <- as.formula(paste(outcome, "~ rural_label +", covars))
  m_unadj <- svyglm(f_unadj, design = design, family = quasibinomial())
  m_adj   <- svyglm(f_adj,   design = design, family = quasibinomial())
  list(
    unadj = tidy(m_unadj, exponentiate = TRUE, conf.int = TRUE) %>%
      filter(term == "rural_labelRural"),
    adj   = tidy(m_adj, exponentiate = TRUE, conf.int = TRUE) %>%
      filter(term == "rural_labelRural")
  )
}

cat("\n========== PRIMARY ANALYSIS: BINARY OUTCOMES (Rural vs. Urban) ==========\n")
primary_results <- list()
for (oc in binary_outcomes) {
  cat("\n---", oc, "---\n")
  res <- fit_binary(oc, svy_design, adj_covars)
  cat("Unadjusted OR:\n"); print(res$unadj)
  cat("Adjusted OR:\n");   print(res$adj)
  primary_results[[oc]] <- res
}

# Length of stay (Gamma/log link handles right skew; +0.5 offset for
# same-day discharges coded LOS = 0)
cat("\n--- Length of stay (Gamma, log link; ratio of geometric means) ---\n")
m_los_unadj <- svyglm(I(los + 0.5) ~ rural_label, design = svy_design, family = Gamma(link = "log"))
m_los_adj   <- svyglm(as.formula(paste("I(los + 0.5) ~ rural_label +", adj_covars)),
                       design = svy_design, family = Gamma(link = "log"))
cat("Unadjusted:\n"); print(tidy(m_los_unadj, exponentiate = TRUE, conf.int = TRUE) %>% filter(term == "rural_labelRural"))
cat("Adjusted:\n");   print(tidy(m_los_adj,   exponentiate = TRUE, conf.int = TRUE) %>% filter(term == "rural_labelRural"))

# Total charges (same approach as LOS)
cat("\n--- Total charges (Gamma, log link) ---\n")
m_chg_adj <- svyglm(as.formula(paste("I(totchg + 1) ~ rural_label +", adj_covars)),
                     design = svy_design, family = Gamma(link = "log"))
print(tidy(m_chg_adj, exponentiate = TRUE, conf.int = TRUE) %>% filter(term == "rural_labelRural"))

# Number of procedures (count outcome)
cat("\n--- Number of procedures (quasipoisson) ---\n")
m_npr_adj <- svyglm(as.formula(paste("npr ~ rural_label +", adj_covars)),
                     design = svy_design, family = quasipoisson())
print(tidy(m_npr_adj, exponentiate = TRUE, conf.int = TRUE) %>% filter(term == "rural_labelRural"))


# ----- 14. GENERIC STRATIFIED-ESTIMATE HELPER -----
# Returns the adjusted rural-vs-urban estimate (OR or, for LOS, ratio of
# geometric means) within one subgroup. `family` controls the model type;
# `covars` should omit whatever variable is being stratified on to avoid
# collinearity/separation within a stratum (e.g., drop cancer_type when
# stratifying by cancer_type).
stratified_estimate <- function(design, outcome, covars, family,
                                 is_los = FALSE, min_n = 30) {
  if (nrow(design$variables) < min_n) {
    return(tibble(term = "rural_labelRural", estimate = NA, conf.low = NA,
                  conf.high = NA, p.value = NA, n = nrow(design$variables)))
  }
  lhs <- if (is_los) "I(los + 0.5)" else outcome
  f <- as.formula(paste(lhs, "~ rural_label +", covars))
  m <- tryCatch(svyglm(f, design = design, family = family), error = function(e) NULL)
  if (is.null(m)) {
    return(tibble(term = "rural_labelRural", estimate = NA, conf.low = NA,
                  conf.high = NA, p.value = NA, n = nrow(design$variables)))
  }
  tidy(m, exponentiate = TRUE, conf.int = TRUE) %>%
    filter(term == "rural_labelRural") %>%
    mutate(n = nrow(design$variables))
}


# ----- 15. STRATIFIED ANALYSIS BY SEX -----
cat("\n========== STRATIFIED BY SEX ==========\n")
covars_no_sex <- "AGE + race_eth + income_qrtl + payer + hosp_region +
                  hosp_teach + elective + cancer_type + severity + risk_mort"

for (s in levels(model_cohort$sex)) {
  cat("\n---", s, "---\n")
  d <- subset(svy_design, sex == s)
  for (oc in binary_outcomes) {
    r <- stratified_estimate(d, oc, covars_no_sex, quasibinomial())
    cat(sprintf("  %-18s adj OR=%.2f (%.2f-%.2f), n=%d\n",
                oc, r$estimate, r$conf.low, r$conf.high, r$n))
  }
  r_los <- stratified_estimate(d, NA, covars_no_sex, Gamma(link = "log"), is_los = TRUE)
  cat(sprintf("  %-18s adj ratio=%.2f (%.2f-%.2f), n=%d\n",
              "los", r_los$estimate, r_los$conf.low, r_los$conf.high, r_los$n))
}


# ----- 16. STRATIFIED ANALYSIS BY RACE/ETHNICITY -----
cat("\n========== STRATIFIED BY RACE/ETHNICITY ==========\n")
covars_no_race <- "AGE + sex + income_qrtl + payer + hosp_region +
                   hosp_teach + elective + cancer_type + severity + risk_mort"

for (r_lvl in levels(model_cohort$race_eth)) {
  n_stratum <- sum(model_cohort$race_eth == r_lvl, na.rm = TRUE)
  cat("\n---", r_lvl, "(unweighted n =", n_stratum, ") ---\n")
  if (n_stratum < 50) {
    cat("  Too few observations for stable stratified estimates -- skipping.\n")
    next
  }
  d <- subset(svy_design, race_eth == r_lvl)
  for (oc in binary_outcomes) {
    r <- stratified_estimate(d, oc, covars_no_race, quasibinomial())
    cat(sprintf("  %-18s adj OR=%.2f (%.2f-%.2f), n=%d\n",
                oc, r$estimate, r$conf.low, r$conf.high, r$n))
  }
  r_los <- stratified_estimate(d, NA, covars_no_race, Gamma(link = "log"), is_los = TRUE)
  cat(sprintf("  %-18s adj ratio=%.2f (%.2f-%.2f), n=%d\n",
              "los", r_los$estimate, r_los$conf.low, r_los$conf.high, r_los$n))
}


# ----- 17. DOES THE DISPARITY VARY BY CANCER TYPE? -----

# --- 17a. Formal interaction test: rural_label x cancer_type ---
# regTermTest gives a design-based (Rao-Scott) test of the interaction
# term as a whole -- the right way to ask "does the rural/urban effect
# differ across cancer types" with survey-weighted data.
cat("\n========== EFFECT MODIFICATION BY CANCER TYPE (interaction test) ==========\n")
covars_no_cancer <- "AGE + sex + race_eth + income_qrtl + payer + hosp_region +
                     hosp_teach + elective + severity + risk_mort"

for (oc in binary_outcomes) {
  f_int <- as.formula(paste(oc, "~ rural_label * cancer_type +", covars_no_cancer))
  m_int <- tryCatch(svyglm(f_int, design = svy_design, family = quasibinomial()),
                     error = function(e) NULL)
  if (is.null(m_int)) { cat(oc, ": model failed to converge\n"); next }
  test <- regTermTest(m_int, ~rural_label:cancer_type)
  cat(sprintf("%-18s interaction p = %.4f\n", oc, test$p[1]))
}

f_int_los <- as.formula(paste("I(los + 0.5) ~ rural_label * cancer_type +", covars_no_cancer))
m_int_los <- svyglm(f_int_los, design = svy_design, family = Gamma(link = "log"))
test_los <- regTermTest(m_int_los, ~rural_label:cancer_type)
cat(sprintf("%-18s interaction p = %.4f\n", "los", test_los$p[1]))

# --- 17b. Stratified adjusted rural-vs-urban estimate within each
#     cancer type, for a forest-plot-style summary table ---
cat("\n========== STRATIFIED BY CANCER TYPE ==========\n")
cancer_type_results <- list()
for (ct in levels(model_cohort$cancer_type)) {
  n_stratum <- sum(model_cohort$cancer_type == ct, na.rm = TRUE)
  cat("\n---", ct, "(unweighted n =", n_stratum, ") ---\n")
  if (n_stratum < 50) {
    cat("  Too few observations for stable stratified estimates -- skipping.\n")
    next
  }
  d <- subset(svy_design, cancer_type == ct)
  row <- list(cancer_type = ct)
  for (oc in binary_outcomes) {
    r <- stratified_estimate(d, oc, covars_no_cancer, quasibinomial())
    cat(sprintf("  %-18s adj OR=%.2f (%.2f-%.2f), n=%d\n",
                oc, r$estimate, r$conf.low, r$conf.high, r$n))
    row[[oc]] <- r
  }
  r_los <- stratified_estimate(d, NA, covars_no_cancer, Gamma(link = "log"), is_los = TRUE)
  cat(sprintf("  %-18s adj ratio=%.2f (%.2f-%.2f), n=%d\n",
              "los", r_los$estimate, r_los$conf.low, r_los$conf.high, r_los$n))
  row[["los"]] <- r_los
  cancer_type_results[[ct]] <- row
}


# ----- 18. DESCRIPTIVE: RURAL/URBAN x SEX x RACE x CANCER TYPE -----
# Fully-crossed regression strata are likely underpowered for most
# cells; this gives weighted descriptive prevalence instead, which is
# usually more honest than an unstable adjusted estimate at this level
# of stratification. Inspect cell sizes before reporting any of these.
cat("\n========== DESCRIPTIVE: WEIGHTED OUTCOME PREVALENCE BY FULL STRATA ==========\n")
full_strata_summary <- svyby(
  ~infection_sepsis + vte + aki + died,
  ~rural_label + sex + race_eth + cancer_type,
  svy_design, svymean, na.rm = TRUE,
  keep.var = FALSE
)
cat("N cells (some may be sparse -- check counts before interpreting):\n")
print(nrow(full_strata_summary))
# Uncomment to inspect in full (can be a long table):
# print(full_strata_summary)


# ----- 19. SENSITIVITY ANALYSES -----
cat("\n========== SENSITIVITY ANALYSES ==========\n")

# --- 19a. Collapse micropolitan (PL_NCHS=5) into "urban" -- some studies
#     treat only noncore (PL_NCHS=6) as "rural" ---
model_cohort_alt <- model_cohort %>%
  mutate(rural_label_alt = factor(if_else(PL_NCHS == 6, "Rural", "Urban"),
                                   levels = c("Urban", "Rural")))
svy_alt <- svydesign(id = ~HOSP_NIS, strata = ~NIS_STRATUM, weights = ~DISCWT,
                      data = model_cohort_alt, nest = TRUE)
cat("\nAlternate rural definition (noncore only) -- mortality:\n")
m_alt <- svyglm(as.formula(paste("died ~ rural_label_alt +", adj_covars)),
                 design = svy_alt, family = quasibinomial())
print(tidy(m_alt, exponentiate = TRUE, conf.int = TRUE) %>% filter(term == "rural_label_altRural"))

# --- 19b. Exclude transferred-in patients (transfers can bias LOS and
#     severity upward independent of rural/urban status) ---
if ("TRAN_IN" %in% names(nis)) {
  cat("\nExcluding transfers-in -- mortality:\n")
  svy_notransfer <- subset(svy_design, TRAN_IN == 0)
  m_nt <- svyglm(as.formula(paste("died ~ rural_label +", adj_covars)),
                  design = svy_notransfer, family = quasibinomial())
  print(tidy(m_nt, exponentiate = TRUE, conf.int = TRUE) %>% filter(term == "rural_labelRural"))
} else {
  cat("\nTRAN_IN not found in this extract -- add it to test the transfer-exclusion sensitivity analysis.\n")
}

# --- 19c. Any-listed cancer diagnosis instead of principal-diagnosis-only
#     (uncomment once cohort_any from Section 6b is built) ---
# cohort_any <- cohort_any %>% mutate(... repeat Sections 7-10 ...)
# Compare N and effect estimates to the principal-dx cohort above.
