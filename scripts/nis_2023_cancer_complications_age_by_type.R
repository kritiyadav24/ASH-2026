# ============================================================
# NIS 2023 Core: Complication burden in cancer hospitalizations,
# AYA vs. older-onset, by cancer type
#
# RQ: Among adults hospitalized with a principal diagnosis of
# cancer, does complication burden (a composite of infection,
# sepsis, VTE, transfusion, or ICU-level care) differ between AYA
# (age 18-39) and older-onset (age 40-64) patients -- and does
# that age-related gap vary by cancer type?
#
# WHY THIS REPLACES THE BREAST-CANCER-ONLY VERSION: that script
# (nis_2023_breast_cancer_complications_age.R) came back
# underpowered -- only 293 unweighted AYA breast cancer patients,
# with single-digit event counts per complication (one outcome,
# surgical site infection, had ZERO AYA events, producing a
# degenerate/uninterpretable OR). Broadening to all cancer types
# gives a much larger cohort (the earlier AYA cancer scripts this
# session had ~9,861 unweighted patients), and a COMPOSITE
# complication outcome pools events for more stable estimates,
# at the cost of not being able to say which specific complication
# drives any difference.
#
# COHORT: Age 18-64, PRINCIPAL diagnosis (I10_DX1) is a malignant
# neoplasm ("C" code), EXCLUDING non-melanoma skin cancer (C44) --
# same cancer definition as every other cancer script in this repo.
# Split into AYA (18-39) and older-onset (40-64), same rationale
# as the breast-cancer script for capping at 64 (avoid mixing in
# elderly comorbidity/frailty effects).
#
# CANCER TYPE (10 categories, same classification used throughout
# this repo): Colorectal, Breast, Pancreatic, Esophageal,
# Ovarian/Gynecologic, Lung, Melanoma, Lymphoma, Leukemia, Other.
#
# PRIMARY OUTCOME: composite "any complication" -- surgical site/
# postprocedural infection (T81.4), sepsis (A40-A41, R65.2x), VTE
# (I26, I824), transfusion (ICD-10-PCS table 302), or mechanical
# ventilation (5A19-). All five code definitions reused from the
# breast-cancer script. A patient counts as having "any
# complication" if ANY of the five is present.
#
# SECONDARY: each of the five complications reported individually
# too, WITH cell counts shown -- some may still be sparse for
# rarer cancer types (Melanoma, Esophageal had very small AYA
# counts in earlier scripts, e.g. n=20 and n=40 respectively), so
# individual-complication results should be read against their
# cell counts, same caution as every cancer-type breakdown in this
# repo.
#
# ARCHITECTURE: same chunked base-R streaming approach as every
# other script in this repo. NEW checkpoint (broader age range,
# all cancer types, complication codes all in one scan).
#
# TWO IMPROVEMENTS ADDED after the first version of this script
# produced a solid primary finding (AYA OR 1.20 for any
# complication, cancer-type interaction p=0.46):
#   1. A cancer-type-ADJUSTED model (not just the interaction
#      test) -- controls for AYA and older-onset patients having a
#      different cancer-type mix to begin with, which the
#      interaction test alone doesn't rule out.
#   2. A metastatic-status mediation test, reusing the C77-C79
#      code from the AYA metastatic-at-presentation script --
#      tests directly whether "AYA patients present more advanced"
#      explains the complication gap, instead of only citing
#      literature for that mechanism.
#
# THIRD ADDITION after #1 substantially reversed the primary
# complication finding: applied the same unadjusted-vs-cancer-
# type-adjusted check to the mortality and LOS secondary outcomes
# (section 9) -- since cancer-type mix confounded the complication
# result, it could equally be confounding these, and hadn't been
# checked. Reuses the existing checkpoint (cancer_type already
# computed), so no new file scan needed for this addition.
# ============================================================

suppressMessages({
  library(tidyverse)
  library(survey)
})

nis_file       <- "~/Downloads/NIS_2023/NIS_2023_Core.ASC"
# NEW checkpoint filename -- the old one doesn't have has_metastasis (added
# below to test whether metastatic-at-presentation mediates the AYA
# complication gap), so reusing it would silently give NAs for that column.
checkpoint_rds <- "~/Downloads/NIS_2023/nis_2023_cancer_complications_by_type_v2_checkpoint.rds"


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
cancer_regex               <- "^C"
skin_cancer_exclude_regex  <- "^C44"

infection_regex   <- "^T814"
sepsis_regex      <- "^(A4[01]|R652)"
vte_regex         <- "^(I26|I824)"
transfusion_regex <- "^302"
mechvent_regex    <- "^5A19"
metastasis_regex  <- "^C7[789]"  # same code used in the AYA metastatic-at-presentation script

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
    age_in_range <- scalar_df$AGE >= 18 & scalar_df$AGE <= 64 & !is.na(scalar_df$AGE)

    scalar_df$cohort <- is_cancer_principal & age_in_range
    cohort_mask <- scalar_df$cohort

    # Expensive checks -- only for this chunk's cohort rows
    scalar_df$has_infection <- NA
    scalar_df$has_sepsis <- NA
    scalar_df$has_vte <- NA
    scalar_df$has_transfusion <- NA
    scalar_df$has_mechvent <- NA
    scalar_df$has_metastasis <- NA
    scalar_df$cancer_type <- NA_character_

    if (any(cohort_mask)) {
      sub_lines <- lines[cohort_mask]
      dx1_cohort <- dx1[cohort_mask]

      scalar_df$has_infection[cohort_mask]   <- any_match(sub_lines, dx_starts, dx_ends, infection_regex)
      scalar_df$has_sepsis[cohort_mask]      <- any_match(sub_lines, dx_starts, dx_ends, sepsis_regex)
      scalar_df$has_vte[cohort_mask]         <- any_match(sub_lines, dx_starts, dx_ends, vte_regex)
      scalar_df$has_transfusion[cohort_mask] <- any_match(sub_lines, pr_starts, pr_ends, transfusion_regex)
      scalar_df$has_mechvent[cohort_mask]    <- any_match(sub_lines, pr_starts, pr_ends, mechvent_regex)
      scalar_df$has_metastasis[cohort_mask]  <- any_match(sub_lines, dx_starts, dx_ends, metastasis_regex)

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
    }

    all_chunks[[chunk_i]] <- scalar_df
    cat("Chunk", chunk_i, "--", n, "rows,", sum(cohort_mask), "cohort rows so far this chunk\n")
  }
  close(con)

  full_derived <- bind_rows(all_chunks)
  cat("\nTotal rows scanned:", nrow(full_derived), "\n")
  cat("Total cancer cohort rows:", sum(full_derived$cohort), "\n")

  rm(all_chunks); gc()

  dir.create(dirname(checkpoint_rds), showWarnings = FALSE, recursive = TRUE)
  saveRDS(full_derived, checkpoint_rds)
  cat("Saved checkpoint to", checkpoint_rds, "\n")
}


# ----- 4. DERIVE AGE GROUP, CANCER TYPE FACTOR, AND COMPOSITE OUTCOME -----
full_derived <- full_derived %>%
  mutate(
    age_group = case_when(
      AGE >= 18 & AGE <= 39 ~ "AYA (18-39)",
      AGE >= 40 & AGE <= 64 ~ "Older-onset (40-64)",
      TRUE ~ NA_character_
    ),
    age_group = factor(age_group, levels = c("Older-onset (40-64)", "AYA (18-39)")),
    cancer_type = factor(cancer_type,
                          levels = c("Colorectal", "Breast", "Pancreatic", "Esophageal",
                                     "Ovarian/Gynecologic", "Lung", "Melanoma", "Lymphoma",
                                     "Leukemia", "Other")),
    any_complication = has_infection | has_sepsis | has_vte | has_transfusion | has_mechvent
  )


# ----- 5. SURVEY DESIGN + SUBPOPULATION SUBSET -----
svy_design_cc <- svydesign(
  ids = ~HOSP_NIS, strata = ~NIS_STRATUM, weights = ~DISCWT,
  data = full_derived, nest = TRUE
)
svy_cohort_cc <- subset(svy_design_cc, cohort)

cat("\n========== COHORT SIZE ==========\n")
cat("Unweighted cancer cohort n:", nrow(svy_cohort_cc$variables), "\n")
cat("Weighted (national estimate) cohort n:", round(sum(weights(svy_cohort_cc))), "\n")

cat("\nAge group distribution:\n")
print(table(svy_cohort_cc$variables$age_group, useNA = "always"))
cat("\nCancer type distribution:\n")
print(table(svy_cohort_cc$variables$cancer_type, useNA = "always"))


# ----- 6. PRIMARY: COMPOSITE COMPLICATION RATE BY AGE GROUP -----
cat("\n========== PRIMARY: any complication, by age group ==========\n")
print(svyby(~any_complication, ~age_group, svy_cohort_cc, svymean, na.rm = TRUE))

cell_counts_overall <- svy_cohort_cc$variables %>%
  filter(!is.na(age_group)) %>%
  count(age_group, any_complication)
cat("\nCell counts (age_group x any_complication):\n")
print(cell_counts_overall)

model_overall <- svyglm(any_complication ~ age_group, design = svy_cohort_cc, family = quasibinomial())
cat("\nUnadjusted OR (AYA vs. Older-onset, ref = Older-onset):\n")
print(round(exp(cbind(OR = coef(model_overall), confint(model_overall))), 3))
rm(model_overall); gc()


# ----- 6b. IMPROVEMENT 1: ADJUST FOR CANCER TYPE MIX -----
# The interaction test in section 7 checks whether the age gap VARIES by
# cancer type, but that doesn't control for AYA and older-onset patients
# having a different cancer-type MIX to begin with (e.g. some cancer types
# in this cohort skew younger, others older). Adding cancer_type as a
# covariate isolates the age effect from that mix effect.
cat("\n========== IMPROVEMENT 1: adjusted for cancer type mix ==========\n")
model_type_adj <- svyglm(any_complication ~ age_group + cancer_type,
                          design = svy_cohort_cc, family = quasibinomial())
cat("Adjusted OR (AYA vs. Older-onset, controlling for cancer type mix):\n")
type_adj_or <- round(exp(cbind(OR = coef(model_type_adj), confint(model_type_adj))), 3)
print(type_adj_or["age_groupAYA (18-39)", , drop = FALSE])
cat("\nCompare to the unadjusted OR above -- if similar, cancer-type mix is\n")
cat("NOT explaining the age gap; if it shrinks a lot, mix matters.\n")
rm(model_type_adj); gc()

# Same model plus sex (FEMALE) -- closes the previously-flagged
# no-sex-adjustment gap for the complication outcome specifically.
model_type_sex_adj <- svyglm(any_complication ~ age_group + cancer_type + FEMALE,
                              design = svy_cohort_cc, family = quasibinomial())
cat("\nSame model plus sex (FEMALE) -- does adding sex change the picture?\n")
type_sex_adj_or <- round(exp(cbind(OR = coef(model_type_sex_adj), confint(model_type_sex_adj))), 3)
print(type_sex_adj_or["age_groupAYA (18-39)", , drop = FALSE])
rm(model_type_sex_adj); gc()


# ----- 6c. IMPROVEMENT 2: DOES METASTATIC STATUS MEDIATE THE GAP? -----
# Tests one of the two proposed mechanisms directly (advanced disease at
# presentation) instead of only citing literature for it. If AYA patients
# present metastatic more often AND the complication OR shrinks once
# has_metastasis is added, that's real evidence for this pathway; if the
# OR barely moves, it points more toward the dose-intensity/physiologic-
# reserve explanation instead.
cat("\n========== IMPROVEMENT 2: does metastatic status mediate the gap? ==========\n")
cat("\nMetastatic-at-presentation rate by age group (context for the test below):\n")
print(svyby(~has_metastasis, ~age_group, svy_cohort_cc, svymean, na.rm = TRUE))

model_meta_adj <- svyglm(any_complication ~ age_group + has_metastasis,
                          design = svy_cohort_cc, family = quasibinomial())
cat("\nAdjusted OR (AYA vs. Older-onset, controlling for metastatic status):\n")
meta_adj_or <- round(exp(cbind(OR = coef(model_meta_adj), confint(model_meta_adj))), 3)
print(meta_adj_or)
cat("\nCompare age_groupAYA's OR here to the unadjusted OR above -- if it\n")
cat("drops substantially, metastatic-at-presentation explains a meaningful\n")
cat("share of the gap; if it barely moves, metastatic status does NOT\n")
cat("explain it (points toward the dose-intensity/physiologic-reserve\n")
cat("explanation instead).\n")
rm(model_meta_adj); gc()


# ----- 7. DOES THE AGE GAP VARY BY CANCER TYPE? -----
cat("\n========== Any complication rate by cancer type x age group ==========\n")

cell_counts_type <- svy_cohort_cc$variables %>%
  filter(!is.na(age_group), !is.na(cancer_type)) %>%
  count(cancer_type, age_group, any_complication) %>%
  pivot_wider(names_from = any_complication, values_from = n, values_fill = 0, names_prefix = "complication_")
cat("\nCell counts (cancer_type x age_group x any_complication) -- flag anything sparse before interpreting:\n")
print(cell_counts_type, n = 30)

model_interact <- svyglm(any_complication ~ age_group * cancer_type,
                          design = svy_cohort_cc, family = quasibinomial())
test_result <- regTermTest(model_interact, ~age_group:cancer_type)
cat("\nGlobal test: does the AYA-vs-older-onset complication gap vary by cancer type?\n")
cat("(valid even with sparse cells -- individual coefficients not reported)\n")
print(test_result)
rm(model_interact); gc()

cat("\nDescriptive: any-complication rate by age group within each cancer type\n")
cat("(compare against cell counts above before trusting any specific rate):\n")
print(svyby(~any_complication, ~cancer_type + age_group, svy_cohort_cc, svymean, na.rm = TRUE))


# ----- 8. SECONDARY: EACH INDIVIDUAL COMPLICATION BY AGE GROUP -----
analyze_complication <- function(outcome_name, outcome_label) {
  cat("\n========== SECONDARY: ", outcome_label, " by age group ==========\n")
  form_rate <- as.formula(paste0("~", outcome_name))
  print(svyby(form_rate, ~age_group, svy_cohort_cc, svymean, na.rm = TRUE))

  cell_counts <- svy_cohort_cc$variables %>%
    filter(!is.na(age_group)) %>%
    count(age_group, .data[[outcome_name]])
  cat("Cell counts:\n")
  print(cell_counts)
}

analyze_complication("has_infection", "Surgical site / postprocedural infection")
analyze_complication("has_sepsis", "Sepsis")
analyze_complication("has_vte", "VTE")
analyze_complication("has_transfusion", "Transfusion")
analyze_complication("has_mechvent", "Mechanical ventilation")


# ----- 9. SECONDARY: MORTALITY AND LOS BY AGE GROUP, UNADJUSTED AND -----
# ----- CANCER-TYPE-ADJUSTED (same check that reversed the primary   -----
# ----- complication finding -- applied here since we can't assume   -----
# ----- these secondary findings are free of the same confounding)   -----
cat("\n========== SECONDARY: mortality by age group ==========\n")
print(svyby(~DIED, ~age_group, svy_cohort_cc, svymean, na.rm = TRUE))

model_died_unadj <- svyglm(DIED ~ age_group, design = svy_cohort_cc, family = quasibinomial())
cat("\nUnadjusted OR (AYA vs. Older-onset, ref = Older-onset):\n")
print(round(exp(cbind(OR = coef(model_died_unadj), confint(model_died_unadj))), 3))
rm(model_died_unadj); gc()

model_died_adj <- svyglm(DIED ~ age_group + cancer_type, design = svy_cohort_cc, family = quasibinomial())
cat("\nCancer-type-ADJUSTED OR (AYA vs. Older-onset):\n")
died_adj_or <- round(exp(cbind(OR = coef(model_died_adj), confint(model_died_adj))), 3)
print(died_adj_or["age_groupAYA (18-39)", , drop = FALSE])
cat("Compare to the unadjusted OR above -- same logic as the complication check:\n")
cat("if similar, cancer-type mix does NOT explain the mortality gap; if it\n")
cat("shrinks toward/past 1, mix explains some or all of it.\n")
rm(model_died_adj); gc()

# ----- 9b. DOES METASTATIC STATUS EXPLAIN THE MORTALITY ADVANTAGE? -----
# Mortality is the one finding that survived cancer-type adjustment --
# this tests whether it's actually just "AYA patients present less
# metastatic" (a disease-severity explanation) rather than something
# about age itself. Also adds sex (FEMALE) as a covariate throughout,
# closing the previously-flagged "no sex adjustment anywhere" gap.
model_died_meta <- svyglm(DIED ~ age_group + has_metastasis + FEMALE,
                           design = svy_cohort_cc, family = quasibinomial())
cat("\nMetastatic-status-and-sex-ADJUSTED OR (AYA vs. Older-onset):\n")
died_meta_or <- round(exp(cbind(OR = coef(model_died_meta), confint(model_died_meta))), 3)
print(died_meta_or)
cat("Compare age_groupAYA's OR here to the unadjusted OR above -- if similar,\n")
cat("metastatic status/sex do NOT explain the mortality advantage.\n")
rm(model_died_meta); gc()

# Fully adjusted: cancer type + metastatic status + sex together
model_died_full <- svyglm(DIED ~ age_group + cancer_type + has_metastasis + FEMALE,
                           design = svy_cohort_cc, family = quasibinomial())
cat("\nFULLY-ADJUSTED OR (cancer type + metastatic status + sex, AYA vs. Older-onset):\n")
died_full_or <- round(exp(cbind(OR = coef(model_died_full), confint(model_died_full))), 3)
print(died_full_or["age_groupAYA (18-39)", , drop = FALSE])
cat("This is the strongest test: if the AYA mortality advantage survives ALL\n")
cat("three adjustments together, it's independent of cancer type, disease\n")
cat("severity at presentation, AND sex -- the most defensible version of\n")
cat("this finding.\n")
rm(model_died_full); gc()

cat("\n========== SECONDARY: length of stay by age group ==========\n")
print(svyby(~LOS, ~age_group, svy_cohort_cc, svymean, na.rm = TRUE))

model_los_unadj <- svyglm(LOS ~ age_group, design = svy_cohort_cc, family = gaussian())
cat("\nUnadjusted difference (AYA vs. Older-onset, ref = Older-onset):\n")
print(round(cbind(Estimate = coef(model_los_unadj), confint(model_los_unadj)), 3))
rm(model_los_unadj); gc()

model_los_adj <- svyglm(LOS ~ age_group + cancer_type, design = svy_cohort_cc, family = gaussian())
cat("\nCancer-type-ADJUSTED difference (AYA vs. Older-onset):\n")
los_adj_est <- round(cbind(Estimate = coef(model_los_adj), confint(model_los_adj)), 3)
print(los_adj_est["age_groupAYA (18-39)", , drop = FALSE])
rm(model_los_adj); gc()

cat("\n========== ALL ANALYSES COMPLETE ==========\n")
