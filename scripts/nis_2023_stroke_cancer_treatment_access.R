# ============================================================
# NIS 2023 Core: Ischemic stroke treatment access and outcomes,
# cancer vs. non-cancer, and by cancer type
#
# RQ: Among adults hospitalized with a principal diagnosis of
# ischemic stroke, does having a cancer diagnosis predict
# different treatment (IV thrombolysis, mechanical thrombectomy)
# and outcomes (mortality, LOS) compared to non-cancer stroke
# patients -- and does that gap vary by cancer type or metastatic
# status?
#
# WHY 2023 SPECIFICALLY: the existing comprehensive NIS study on
# this topic (Cancers, 2021) used 2015-2017 data -- BEFORE the
# DAWN and DEFUSE-3 trials (2018) extended mechanical thrombectomy
# eligibility from a 6-hour to up to a 24-hour window in select
# patients with favorable imaging, a genuinely practice-changing
# guideline update. 2023 tests whether the cancer-vs-non-cancer
# treatment gap persists even under this much more permissive
# eligibility window, which the older study could not test.
#
# COHORT: Age 18+, PRINCIPAL diagnosis (I10_DX1) is ischemic
# stroke, ICD-10-CM I63.x (cerebral infarction). Cancer status is
# determined from ANY listed diagnosis (not just principal, since
# stroke -- not cancer -- is the reason for THIS admission):
# malignant neoplasm ("C" code), excluding non-melanoma skin
# cancer (C44), same convention as every other cancer script in
# this repo.
#
# CANCER TYPE (10 categories, same classification used throughout
# this repo) and METASTATIC STATUS (C77-C79, same code validated
# in the AYA scripts) are both captured to test whether the
# treatment/outcome gap is uniform or concentrated in specific
# subgroups -- the existing 2015-2017 study found pancreatic and
# respiratory cancers drove most of the excess mortality, while
# prostate and breast showed none; worth checking if that pattern
# still holds in 2023.
#
# TREATMENT CODES (verified via web search, NOT guessed):
#   - IV thrombolysis: ICD-10-PCS 3E03317 (Introduction of Other
#     Thrombolytic into Peripheral Vein, Percutaneous Approach) --
#     the standard route for IV tPA in acute stroke.
#   - Mechanical thrombectomy: ICD-10-PCS 03CG3ZZ / 03CG3Z6
#     (Extirpation of Matter from Intracranial Artery, Percutaneous
#     Approach, with or without the bifurcation qualifier).
#
# OUTCOMES: in-hospital mortality, length of stay.
#
# ARCHITECTURE: same chunked base-R streaming approach as every
# other script in this repo.
# ============================================================

suppressMessages({
  library(tidyverse)
  library(survey)
})

nis_file       <- "~/Downloads/NIS_2023/NIS_2023_Core.ASC"
checkpoint_rds <- "~/Downloads/NIS_2023/nis_2023_stroke_cancer_checkpoint.rds"


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
stroke_regex               <- "^I63"
cancer_regex                <- "^C"
skin_cancer_exclude_regex   <- "^C44"
metastasis_regex            <- "^C7[789]"
thrombolysis_regex          <- "^3E03317"
thrombectomy_regex          <- "^03CG3"

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
    is_stroke_principal <- grepl(stroke_regex, dx1)
    age_in_range <- scalar_df$AGE >= 18 & !is.na(scalar_df$AGE)

    scalar_df$cohort <- is_stroke_principal & age_in_range
    cohort_mask <- scalar_df$cohort

    # Expensive checks -- only for this chunk's cohort rows
    scalar_df$has_cancer <- NA
    scalar_df$has_metastasis <- NA
    scalar_df$has_thrombolysis <- NA
    scalar_df$has_thrombectomy <- NA
    scalar_df$cancer_type <- NA_character_

    if (any(cohort_mask)) {
      sub_lines <- lines[cohort_mask]

      nonskin_cancer_hit <- rep(FALSE, sum(cohort_mask))
      for (i in seq_along(dx_starts)) {
        codes <- trimws(substr(sub_lines, dx_starts[i], dx_ends[i]))
        nonskin_cancer_hit <- nonskin_cancer_hit | (grepl(cancer_regex, codes) & !grepl(skin_cancer_exclude_regex, codes))
      }
      scalar_df$has_cancer[cohort_mask] <- nonskin_cancer_hit

      scalar_df$has_metastasis[cohort_mask]   <- any_match(sub_lines, dx_starts, dx_ends, metastasis_regex)
      scalar_df$has_thrombolysis[cohort_mask] <- any_match(sub_lines, pr_starts, pr_ends, thrombolysis_regex)
      scalar_df$has_thrombectomy[cohort_mask] <- any_match(sub_lines, pr_starts, pr_ends, thrombectomy_regex)

      # cancer type only meaningful for cancer-positive rows; classify
      # using whichever cancer code appears first across all dx fields
      cancer_type_chunk <- rep(NA_character_, sum(cohort_mask))
      for (i in seq_along(dx_starts)) {
        codes <- trimws(substr(sub_lines, dx_starts[i], dx_ends[i]))
        needs_classification <- is.na(cancer_type_chunk) & grepl(cancer_regex, codes) & !grepl(skin_cancer_exclude_regex, codes)
        cancer_type_chunk[needs_classification] <- case_when(
          grepl(colorectal_regex, codes[needs_classification]) ~ "Colorectal",
          grepl(breast_regex, codes[needs_classification]) ~ "Breast",
          grepl(pancreatic_regex, codes[needs_classification]) ~ "Pancreatic",
          grepl(esophageal_regex, codes[needs_classification]) ~ "Esophageal",
          grepl(gyn_regex, codes[needs_classification]) ~ "Ovarian/Gynecologic",
          grepl(lung_regex, codes[needs_classification]) ~ "Lung",
          grepl(melanoma_regex, codes[needs_classification]) ~ "Melanoma",
          grepl(lymphoma_regex, codes[needs_classification]) ~ "Lymphoma",
          grepl(leukemia_regex, codes[needs_classification]) ~ "Leukemia",
          TRUE ~ "Other"
        )
      }
      scalar_df$cancer_type[cohort_mask] <- cancer_type_chunk
    }

    all_chunks[[chunk_i]] <- scalar_df
    cat("Chunk", chunk_i, "--", n, "rows,", sum(cohort_mask), "cohort rows so far this chunk\n")
  }
  close(con)

  full_derived <- bind_rows(all_chunks)
  cat("\nTotal rows scanned:", nrow(full_derived), "\n")
  cat("Total stroke cohort rows:", sum(full_derived$cohort), "\n")

  rm(all_chunks); gc()

  dir.create(dirname(checkpoint_rds), showWarnings = FALSE, recursive = TRUE)
  saveRDS(full_derived, checkpoint_rds)
  cat("Saved checkpoint to", checkpoint_rds, "\n")
}


# ----- 4. DERIVE CANCER STATUS FACTOR -----
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


# ----- 5. SURVEY DESIGN + SUBPOPULATION SUBSET -----
svy_design_stroke <- svydesign(
  ids = ~HOSP_NIS, strata = ~NIS_STRATUM, weights = ~DISCWT,
  data = full_derived, nest = TRUE
)
svy_cohort_stroke <- subset(svy_design_stroke, cohort)

cat("\n========== COHORT SIZE ==========\n")
cat("Unweighted stroke cohort n:", nrow(svy_cohort_stroke$variables), "\n")
cat("Weighted (national estimate) cohort n:", round(sum(weights(svy_cohort_stroke))), "\n")

cat("\nCancer status distribution:\n")
print(table(svy_cohort_stroke$variables$cancer_status, useNA = "always"))
cat("\nCancer type distribution (among cancer patients):\n")
print(table(svy_cohort_stroke$variables$cancer_type, useNA = "always"))


# ----- 6. PRIMARY: TREATMENT ACCESS BY CANCER STATUS -----
cat("\n========== Thrombolysis rate by cancer status ==========\n")
print(svyby(~has_thrombolysis, ~cancer_status, svy_cohort_stroke, svymean, na.rm = TRUE))
model_lysis <- svyglm(has_thrombolysis ~ cancer_status + AGE + FEMALE,
                       design = svy_cohort_stroke, family = quasibinomial())
cat("\nAdjusted OR (Cancer vs. No cancer, ref = No cancer):\n")
print(round(exp(cbind(OR = coef(model_lysis), confint(model_lysis))), 3))
rm(model_lysis); gc()

cat("\n========== Thrombectomy rate by cancer status ==========\n")
print(svyby(~has_thrombectomy, ~cancer_status, svy_cohort_stroke, svymean, na.rm = TRUE))
model_thrombectomy <- svyglm(has_thrombectomy ~ cancer_status + AGE + FEMALE,
                              design = svy_cohort_stroke, family = quasibinomial())
cat("\nAdjusted OR (Cancer vs. No cancer, ref = No cancer):\n")
print(round(exp(cbind(OR = coef(model_thrombectomy), confint(model_thrombectomy))), 3))
rm(model_thrombectomy); gc()


# ----- 7. OUTCOMES BY CANCER STATUS -----
cat("\n========== Mortality by cancer status ==========\n")
print(svyby(~DIED, ~cancer_status, svy_cohort_stroke, svymean, na.rm = TRUE))
model_died <- svyglm(DIED ~ cancer_status + AGE + FEMALE, design = svy_cohort_stroke, family = quasibinomial())
cat("\nAdjusted OR (Cancer vs. No cancer, ref = No cancer):\n")
print(round(exp(cbind(OR = coef(model_died), confint(model_died))), 3))
rm(model_died); gc()

cat("\n========== Length of stay by cancer status ==========\n")
print(svyby(~LOS, ~cancer_status, svy_cohort_stroke, svymean, na.rm = TRUE))


# ----- 8. AMONG CANCER PATIENTS: DOES METASTATIC STATUS MATTER? -----
cat("\n========== Among cancer patients: mortality by metastatic status ==========\n")
svy_cohort_cancer_only <- subset(svy_design_stroke, cohort & has_cancer)
print(svyby(~DIED, ~has_metastasis, svy_cohort_cancer_only, svymean, na.rm = TRUE))
model_meta_died <- svyglm(DIED ~ has_metastasis + AGE + FEMALE,
                           design = svy_cohort_cancer_only, family = quasibinomial())
cat("\nAdjusted OR (Metastatic vs. Non-metastatic):\n")
print(round(exp(cbind(OR = coef(model_meta_died), confint(model_meta_died))), 3))
rm(model_meta_died); gc()

cat("\n========== Among cancer patients: thrombectomy rate by metastatic status ==========\n")
print(svyby(~has_thrombectomy, ~has_metastasis, svy_cohort_cancer_only, svymean, na.rm = TRUE))


# ----- 9. DOES MORTALITY VARY BY CANCER TYPE? -----
cat("\n========== Mortality by cancer type (among cancer patients) ==========\n")
cell_counts_type <- svy_cohort_cancer_only$variables %>%
  filter(!is.na(cancer_type)) %>%
  count(cancer_type, DIED)
cat("\nCell counts (cancer_type x DIED) -- flag anything sparse before interpreting:\n")
print(as.data.frame(cell_counts_type))  # as.data.frame() avoids a tibble print quirk seen in long-running sessions

cat("\nDescriptive: mortality rate by cancer type\n")
print(svyby(~DIED, ~cancer_type, svy_cohort_cancer_only, svymean, na.rm = TRUE))

cat("\n========== ALL ANALYSES COMPLETE ==========\n")
