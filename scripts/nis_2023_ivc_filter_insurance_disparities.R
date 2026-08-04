# ============================================================
# NIS 2023 Core: Insurance-based disparities in IVC filter
# placement among hospitalized cancer patients with VTE
#
# Primary RQ: Among hospitalized cancer patients with VTE, are
# there insurance-based disparities in IVC filter placement?
# Do Medicaid, uninsured, and other-payer patients receive IVC
# filters at different rates than privately insured patients,
# and do disparities persist after adjusting for clinical
# severity, comorbidities, and cancer type?
#
# Secondary: (1) do disparities vary by cancer type; (2) do
# outcomes (mortality) differ by insurance x filter status;
# (3) what is the severity/bleeding-contraindication profile of
# filter recipients by insurance (appropriate use vs. over/
# underuse)?
#
# ARCHITECTURE: Streams the 4+GB fixed-width file in 200,000-
# line chunks (base R readLines()+substr(), no tidyverse/vroom
# in the hot loop) -- full tidyverse/vroom approaches were too
# slow/memory-heavy to reliably finish on ordinary laptop
# hardware against the real 6.74M-row file (see
# scripts/nis_2023_core_cancer_vte_explore.R for that history).
# Cohort-defining flags (cancer/VTE/prior-VTE-history) are
# computed for every row (required for correct survey
# subpopulation estimation -- see below); the more expensive
# per-row checks (cancer type, comorbidities, IVC filter) are
# only computed for the ~0.6% of rows that are actually in the
# cancer+VTE cohort, since re-running them on all 6.74M rows
# would be wasted work.
#
# SURVEY DESIGN NOTE: NIS is a complex survey sample (DISCWT
# weight, NIS_STRATUM strata, HOSP_NIS cluster/PSU). Building
# the survey design from the cancer+VTE cohort's rows alone
# (as if that subset were its own independent sample) understates
# standard errors. The statistically correct approach is
# "domain/subpopulation estimation": build svydesign() from the
# FULL sample, then subset() to the cohort of interest -- which
# is why this script retains weight/strata/cluster columns (and
# the cohort-defining flags) for every discharge record, not just
# cohort members.
#
# SCOPE NOTE -- RACE: not available. Checked NIS_2023_Core,
# NIS_2023_Hospital, and NIS_2023_Severity file structures; none
# contain a race/ethnicity variable in this extract. This
# analysis is insurance/income/rurality-focused, not race-based.
#
# SCOPE NOTE -- COMORBIDITIES: this uses a targeted panel of 8
# clinically relevant comorbidities (metastatic disease, heart
# failure, CKD, liver disease, coagulopathy, obesity, diabetes,
# COPD) derived from ICD-10-CM codes -- NOT the full 31-category
# AHRQ Elixhauser Comorbidity Software algorithm, which requires
# a much larger code list. Treat this as a proxy, not a validated
# comorbidity index.
#
# SCOPE NOTE -- SEVERITY: NIS Core has no APR-DRG severity/risk-
# of-mortality subclass. This uses proxies available in Core
# (number of diagnoses coded, elective vs. emergent admission, ED
# entry, transfer-in status) rather than true clinical severity.
# NIS_2023_Severity.ASC may contain real APR-DRG severity data --
# upgrade this once you have that file's SAS load program from
# HCUP-US (same place as the Core one), merging via KEY_NIS.
#
# SCOPE NOTE -- BLEEDING CONTRAINDICATION AS EXCLUSION VS.
# COVARIATE: bleeding-contraindication and prior-VTE-history are
# handled differently on purpose. Prior VTE history (Z86.71)
# EXCLUDES a record from the cohort whenever present, even if an
# active VTE code is also listed on the same record (i.e. a
# genuine recurrent VTE with noted history will still be
# excluded) -- this is a known limitation of claims-based VTE
# algorithms, not a bug. Bleeding contraindication is kept as a
# COVARIATE (not an exclusion), because secondary Q3 specifically
# asks whether filters are appropriately concentrated in
# contraindicated patients -- excluding those patients would make
# that question unanswerable.
#
# ICD-10 CODE FORMAT: NIS stores ICD-10-CM/PCS codes with NO
# decimal points (e.g. "C7800", not "C78.00"). All regexes below
# are written accordingly.
# ============================================================

suppressMessages({
  library(tidyverse)
  library(survey)
})

nis_file      <- "~/Downloads/NIS_2023/NIS_2023_Core.ASC"
checkpoint_rds <- "~/Downloads/NIS_2023/nis_2023_full_derived_checkpoint.rds"


# ----- 1. COLUMN LAYOUT (from HCUP's SASload_NIS_2023_Core.SAS) -----
full_spec <- tibble::tribble(
  ~var, ~start, ~end, ~type, ~informat,
  "HOSP_NIS", 1L, 5L, "numeric", "N5PF",
  "KEY_NIS", 6L, 15L, "numeric", "N10PF",
  "NIS_STRATUM", 16L, 19L, "numeric", "N4PF",
  "AGE", 20L, 22L, "numeric", "N3PF",
  "DIED", 29L, 30L, "numeric", "N2PF",
  "DISCWT", 31L, 41L, "numeric", "N11P7F",
  "ELECTIVE", 54L, 55L, "numeric", "N2PF",
  "FEMALE", 56L, 57L, "numeric", "N2PF",
  "HCUP_ED", 58L, 60L, "numeric", "N3PF",
  "I10_NDX", 351L, 352L, "numeric", "N2PF",
  "LOS", 533L, 537L, "numeric", "N5PF",
  "PAY1", 542L, 543L, "numeric", "N2PF",
  "TRAN_IN", 621L, 622L, "numeric", "N2PF",
  "TRAN_OUT", 623L, 624L, "numeric", "N2PF",
  "ZIPINC_QRTL", 629L, 630L, "numeric", "N2PF",
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
clean_missing <- function(x, informat) {
  x[x %in% hcup_missing_codes[[informat]]] <- NA
  x
}


# ----- 2. ICD-10-CM/PCS CODE DEFINITIONS (no decimal points) -----
cancer_regex      <- "^C"
vte_regex         <- "^(I26|I824)"          # PE (I26) + acute lower-extremity DVT (I82.4x)
prior_vte_regex   <- "^Z8671"                # personal history of VTE
bleeding_regex    <- "^(I61|K922|D62)"       # ICH, GI hemorrhage NOS, acute posthemorrhagic anemia
ivc_filter_regex  <- "^06H[034]"             # ICD-10-PCS: insertion of IVC filter

gyn_regex         <- "^(C54|C53|C56)"
breast_regex      <- "^C50"
lung_regex        <- "^C34"
pancreatic_regex  <- "^C25"
gi_regex          <- "^C(16|17|18|19|20|21|22)"
heme_regex        <- "^C9[1-5]"

metastatic_regex  <- "^(C77|C78|C79|C800)"
hf_regex          <- "^I50"
ckd_regex         <- "^N18"
liver_regex       <- "^K7[0-4]"
coag_regex        <- "^D6[5-9]"
obesity_regex     <- "^E66"
diabetes_regex    <- "^E1[0-4]"
copd_regex        <- "^J4[0-4]"

any_match <- function(lines, starts, ends, regex) {
  hit <- rep(FALSE, length(lines))
  for (i in seq_along(starts)) {
    codes <- trimws(substr(lines, starts[i], ends[i]))
    hit <- hit | grepl(regex, codes)
  }
  hit
}


# ----- 3. STREAM THE FILE AND DERIVE COHORT FLAGS -----
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

    # Required on ALL rows -- needed for correct full-sample subset() design
    scalar_df$has_cancer   <- any_match(lines, dx_starts, dx_ends, cancer_regex)
    scalar_df$has_vte      <- any_match(lines, dx_starts, dx_ends, vte_regex)
    scalar_df$prior_vte_hx <- any_match(lines, dx_starts, dx_ends, prior_vte_regex)
    cohort_mask <- scalar_df$has_cancer & scalar_df$has_vte & !scalar_df$prior_vte_hx

    # Expensive checks -- only run on this chunk's cohort rows
    scalar_df$ivc_filter      <- NA
    scalar_df$bleeding_contra <- NA
    scalar_df$cancer_type     <- NA_character_
    scalar_df$metastatic <- NA; scalar_df$heart_failure <- NA; scalar_df$ckd <- NA
    scalar_df$liver_disease <- NA; scalar_df$coagulopathy <- NA
    scalar_df$obesity <- NA; scalar_df$diabetes <- NA; scalar_df$copd <- NA

    if (any(cohort_mask)) {
      sub_lines <- lines[cohort_mask]
      scalar_df$ivc_filter[cohort_mask]      <- any_match(sub_lines, pr_starts, pr_ends, ivc_filter_regex)
      scalar_df$bleeding_contra[cohort_mask] <- any_match(sub_lines, dx_starts, dx_ends, bleeding_regex)
      scalar_df$cancer_type[cohort_mask] <- case_when(
        any_match(sub_lines, dx_starts, dx_ends, gyn_regex) ~ "Gynecologic",
        any_match(sub_lines, dx_starts, dx_ends, breast_regex) ~ "Breast",
        any_match(sub_lines, dx_starts, dx_ends, lung_regex) ~ "Lung",
        any_match(sub_lines, dx_starts, dx_ends, pancreatic_regex) ~ "Pancreatic",
        any_match(sub_lines, dx_starts, dx_ends, gi_regex) ~ "GI",
        any_match(sub_lines, dx_starts, dx_ends, heme_regex) ~ "Hematologic",
        TRUE ~ "Other"
      )
      scalar_df$metastatic[cohort_mask]    <- any_match(sub_lines, dx_starts, dx_ends, metastatic_regex)
      scalar_df$heart_failure[cohort_mask] <- any_match(sub_lines, dx_starts, dx_ends, hf_regex)
      scalar_df$ckd[cohort_mask]           <- any_match(sub_lines, dx_starts, dx_ends, ckd_regex)
      scalar_df$liver_disease[cohort_mask] <- any_match(sub_lines, dx_starts, dx_ends, liver_regex)
      scalar_df$coagulopathy[cohort_mask]  <- any_match(sub_lines, dx_starts, dx_ends, coag_regex)
      scalar_df$obesity[cohort_mask]       <- any_match(sub_lines, dx_starts, dx_ends, obesity_regex)
      scalar_df$diabetes[cohort_mask]      <- any_match(sub_lines, dx_starts, dx_ends, diabetes_regex)
      scalar_df$copd[cohort_mask]          <- any_match(sub_lines, dx_starts, dx_ends, copd_regex)
    }

    all_chunks[[chunk_i]] <- scalar_df
    cat("Chunk", chunk_i, "--", n, "rows,", sum(cohort_mask), "cohort rows so far this chunk\n")
  }
  close(con)

  full_derived <- bind_rows(all_chunks)
  cat("\nTotal rows scanned:", nrow(full_derived), "\n")
  cat("Total cancer+VTE cohort rows:", sum(full_derived$has_cancer & full_derived$has_vte & !full_derived$prior_vte_hx), "\n")

  dir.create(dirname(checkpoint_rds), showWarnings = FALSE, recursive = TRUE)
  saveRDS(full_derived, checkpoint_rds)
  cat("Saved checkpoint to", checkpoint_rds, "-- re-running this script will load it instead of re-scanning the file.\n")
}


# ----- 4. INSURANCE CATEGORIZATION (PAY1) -----
# HCUP PAY1: 1=Medicare, 2=Medicaid, 3=Private (incl. HMO), 4=Self-pay,
# 5=No charge, 6=Other. Private is the reference level per the RQ.
full_derived <- full_derived %>%
  mutate(
    insurance = case_when(
      PAY1 == 3 ~ "Private",
      PAY1 == 1 ~ "Medicare",
      PAY1 == 2 ~ "Medicaid",
      PAY1 == 4 ~ "Uninsured/Self-pay",
      PAY1 %in% c(5, 6) ~ "Other",
      TRUE ~ NA_character_
    ),
    insurance = factor(insurance,
                       levels = c("Private", "Medicare", "Medicaid",
                                  "Uninsured/Self-pay", "Other"))
  )


# ----- 5. SURVEY DESIGN + SUBPOPULATION SUBSET -----
svy_design <- svydesign(
  ids = ~HOSP_NIS, strata = ~NIS_STRATUM, weights = ~DISCWT,
  data = full_derived, nest = TRUE
)
svy_cohort <- subset(svy_design, has_cancer & has_vte & !prior_vte_hx)

cat("\n========== COHORT SIZE ==========\n")
cat("Unweighted cancer+VTE cohort n:", nrow(svy_cohort$variables), "\n")
cat("Weighted (national estimate) cohort n:", round(sum(weights(svy_cohort))), "\n")


# ----- 6. TABLE 1: DESCRIPTIVE CHARACTERISTICS BY INSURANCE -----
cat("\n========== TABLE 1: Cohort characteristics by insurance ==========\n")
cat("\nUnweighted n and weighted IVC filter rate by insurance:\n")
print(svyby(~ivc_filter, ~insurance, svy_cohort, svymean, na.rm = TRUE))

cat("\nWeighted mortality by insurance:\n")
print(svyby(~DIED, ~insurance, svy_cohort, svymean, na.rm = TRUE))

cat("\nWeighted IVC filter rate by cancer type:\n")
print(svyby(~ivc_filter, ~cancer_type, svy_cohort, svymean, na.rm = TRUE))

cat("\nWeighted IVC filter rate by bleeding contraindication status:\n")
print(svyby(~ivc_filter, ~bleeding_contra, svy_cohort, svymean, na.rm = TRUE))


# ----- 7. PRIMARY ANALYSIS: WHO GETS AN IVC FILTER? -----
cat("\n========== PRIMARY ANALYSIS: IVC filter placement ~ insurance ==========\n")

cat("\nUnadjusted:\n")
model_unadj <- svyglm(ivc_filter ~ insurance, design = svy_cohort, family = quasibinomial())
print(round(exp(cbind(OR = coef(model_unadj), confint(model_unadj))), 3))

cat("\nAdjusted for age, sex, cancer type, comorbidities, and severity proxies:\n")
model_adj <- svyglm(
  ivc_filter ~ insurance + AGE + FEMALE + cancer_type +
    metastatic + heart_failure + ckd + liver_disease + coagulopathy +
    obesity + diabetes + copd +
    ELECTIVE + HCUP_ED + I10_NDX + TRAN_IN,
  design = svy_cohort, family = quasibinomial()
)
print(round(exp(cbind(OR = coef(model_adj), confint(model_adj))), 3))


# ----- 8. SECONDARY Q1: DOES DISPARITY VARY BY CANCER TYPE? -----
cat("\n========== SECONDARY Q1: Insurance x cancer type interaction ==========\n")
model_interaction <- svyglm(
  ivc_filter ~ insurance * cancer_type + AGE + FEMALE,
  design = svy_cohort, family = quasibinomial()
)
print(round(exp(cbind(OR = coef(model_interaction), confint(model_interaction))), 3))


# ----- 9. SECONDARY Q2: MORTALITY BY INSURANCE x FILTER STATUS -----
cat("\n========== SECONDARY Q2: Mortality ~ filter x insurance ==========\n")
cat("\nWeighted mortality by insurance and filter status:\n")
print(svyby(~DIED, ~insurance + ivc_filter, svy_cohort, svymean, na.rm = TRUE))

model_mortality <- svyglm(
  DIED ~ ivc_filter * insurance + AGE + FEMALE + cancer_type +
    metastatic + heart_failure + ckd,
  design = svy_cohort, family = quasibinomial()
)
print(round(exp(cbind(OR = coef(model_mortality), confint(model_mortality))), 3))


# ----- 10. SECONDARY Q3: APPROPRIATENESS OF FILTER USE BY INSURANCE -----
cat("\n========== SECONDARY Q3: Filter placement by bleeding-contraindication x insurance ==========\n")
cat("(Appropriate use = filters concentrated in bleeding-contraindicated patients,\n")
cat(" similarly across insurance groups. Overuse = high filter rate WITHOUT contraindication.)\n\n")
print(svyby(~ivc_filter, ~insurance + bleeding_contra, svy_cohort, svymean, na.rm = TRUE))

model_appropriateness <- svyglm(
  ivc_filter ~ insurance * bleeding_contra + AGE + FEMALE + cancer_type,
  design = svy_cohort, family = quasibinomial()
)
print(round(exp(cbind(OR = coef(model_appropriateness), confint(model_appropriateness))), 3))

cat("\n========== DONE ==========\n")
