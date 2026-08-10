# ============================================================
# NIS 2023 Core: Cardiac complications in AYA cancer patients, by
# rural vs. urban and by cancer type
#
# RQ: Among adolescent and young adult (AYA, age 18-39) cancer
# patients, does the rate of a cardiac complication (heart
# failure, arrhythmia, cardiomyopathy) present at this
# hospitalization differ by rural vs. urban patient location, and
# does that relationship vary by cancer type?
#
# WHY THIS QUESTION: a literature search this session found
# rurality-and-cardiovascular-risk studies in AYA cancer
# SURVIVORS (long-term, post-treatment, registry-based), but
# nothing specifically on acute cardiac-complication
# HOSPITALIZATIONS during/around cancer treatment, by rural/urban.
# This is a different question from the two prior AYA scripts
# (transfer-out, metastatic-at-presentation), both of which came
# back null -- this tests a different mechanism (cardiotoxicity-
# related complications rather than access-to-transfer or
# disease-stage-at-diagnosis).
#
# COHORT: Age 18-39, PRINCIPAL diagnosis (I10_DX1) is a malignant
# neoplasm ("C" code), EXCLUDING non-melanoma skin cancer (C44) --
# same definition as the other two AYA scripts.
#
# EXPOSURE: Rural vs. urban via PL_NCHS2 -- CONFIRMED mapping
# (HCUP User Support): 21 = Urban, 22 = Rural.
#
# OUTCOME: cardiac complication present ANYWHERE in this
# admission's diagnosis list (all 40 dx fields). ICD-10-CM codes:
# I50 (heart failure), I47-I49 (arrhythmias, including atrial
# fibrillation/flutter), I42 (cardiomyopathy). This is a general
# "cardiac complication" flag, NOT specific to chemotherapy-
# induced cardiotoxicity -- NIS Core cannot distinguish
# cardiotoxicity from a cardiac complication with any other cause,
# since there is no medication data to link a specific
# chemotherapy agent to the complication. Stated explicitly here
# so this isn't overclaimed as a pure cardiotoxicity measure.
#
# CANCER TYPE (10 categories, same classification used in the
# early-onset cancer script, derived from principal diagnosis):
# Colorectal, Breast, Pancreatic, Esophageal, Ovarian/Gynecologic,
# Lung, Melanoma, Lymphoma, Leukemia, Other.
#
# SECONDARY OUTCOMES: in-hospital mortality, length of stay.
#
# ARCHITECTURE: same chunked base-R streaming approach as every
# other script in this repo. NEW checkpoint file -- needs a
# cardiac-complication scan across all 40 dx fields plus cancer
# type classification, neither of which the earlier AYA
# checkpoints computed.
# ============================================================

suppressMessages({
  library(tidyverse)
  library(survey)
})

nis_file       <- "~/Downloads/NIS_2023/NIS_2023_Core.ASC"
checkpoint_rds <- "~/Downloads/NIS_2023/nis_2023_aya_cardiac_checkpoint.rds"


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
cardiac_complication_regex <- "^(I50|I4[789]|I42)"

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
    age_in_range <- scalar_df$AGE >= 18 & scalar_df$AGE <= 39 & !is.na(scalar_df$AGE)

    scalar_df$cohort <- is_cancer_principal & age_in_range
    cohort_mask <- scalar_df$cohort

    # Expensive checks -- only for this chunk's cohort rows
    scalar_df$has_cardiac <- NA
    scalar_df$cancer_type <- NA_character_

    if (any(cohort_mask)) {
      sub_lines <- lines[cohort_mask]
      dx1_cohort <- dx1[cohort_mask]

      scalar_df$has_cardiac[cohort_mask] <- any_match(sub_lines, dx_starts, dx_ends, cardiac_complication_regex)

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
  cat("Total AYA cancer cohort rows:", sum(full_derived$cohort), "\n")

  rm(all_chunks); gc()

  dir.create(dirname(checkpoint_rds), showWarnings = FALSE, recursive = TRUE)
  saveRDS(full_derived, checkpoint_rds)
  cat("Saved checkpoint to", checkpoint_rds, "\n")
}


# ----- 4. DERIVE rural/urban (CONFIRMED mapping) -----
full_derived <- full_derived %>%
  mutate(
    rural = case_when(
      PL_NCHS2 == 21 ~ "Urban",
      PL_NCHS2 == 22 ~ "Rural",
      TRUE ~ NA_character_
    ),
    rural = factor(rural, levels = c("Urban", "Rural")),
    cancer_type = factor(cancer_type,
                          levels = c("Colorectal", "Breast", "Pancreatic", "Esophageal",
                                     "Ovarian/Gynecologic", "Lung", "Melanoma", "Lymphoma",
                                     "Leukemia", "Other"))
  )


# ----- 5. SURVEY DESIGN + SUBPOPULATION SUBSET -----
svy_design_card <- svydesign(
  ids = ~HOSP_NIS, strata = ~NIS_STRATUM, weights = ~DISCWT,
  data = full_derived, nest = TRUE
)
svy_cohort_card <- subset(svy_design_card, cohort)

cat("\n========== COHORT SIZE ==========\n")
cat("Unweighted AYA cancer cohort n:", nrow(svy_cohort_card$variables), "\n")
cat("Weighted (national estimate) cohort n:", round(sum(weights(svy_cohort_card))), "\n")

cat("\nRural/urban distribution:\n")
print(table(svy_cohort_card$variables$rural, useNA = "always"))
cat("\nCancer type distribution:\n")
print(table(svy_cohort_card$variables$cancer_type, useNA = "always"))

cat("\nOverall weighted cardiac complication rate:\n")
print(svymean(~has_cardiac, svy_cohort_card, na.rm = TRUE))


# ----- 6. PRIMARY: CARDIAC COMPLICATION RATE BY RURAL/URBAN -----
cat("\n========== PRIMARY: cardiac complication rate by rural/urban ==========\n")
print(svyby(~has_cardiac, ~rural, svy_cohort_card, svymean, na.rm = TRUE))

cell_counts_rural <- svy_cohort_card$variables %>%
  filter(!is.na(rural)) %>%
  count(rural, has_cardiac)
cat("\nCell counts (rural x has_cardiac) -- flag anything sparse:\n")
print(cell_counts_rural)

model_card <- svyglm(has_cardiac ~ rural + AGE + FEMALE, design = svy_cohort_card, family = quasibinomial())
cat("\nAdjusted OR (Rural vs. Urban, ref = Urban):\n")
print(round(exp(cbind(OR = coef(model_card), confint(model_card))), 3))
rm(model_card); gc()


# ----- 7. DOES THE RURAL/URBAN RELATIONSHIP VARY BY CANCER TYPE? -----
cat("\n========== Cardiac complication rate by cancer type x rural/urban ==========\n")

cell_counts_type <- svy_cohort_card$variables %>%
  filter(!is.na(rural), !is.na(cancer_type)) %>%
  count(cancer_type, rural, has_cardiac) %>%
  pivot_wider(names_from = has_cardiac, values_from = n, values_fill = 0, names_prefix = "cardiac_")
cat("\nCell counts (cancer_type x rural x has_cardiac) -- flag anything sparse before interpreting:\n")
print(cell_counts_type, n = 30)

model_interact <- svyglm(has_cardiac ~ rural * cancer_type + AGE + FEMALE,
                          design = svy_cohort_card, family = quasibinomial())
test_result <- regTermTest(model_interact, ~rural:cancer_type)
cat("\nGlobal test: does the rural/urban cardiac-complication gap vary by cancer type?\n")
cat("(valid even with sparse cells -- individual coefficients not reported)\n")
print(test_result)
rm(model_interact); gc()

cat("\nDescriptive: cardiac complication rate by rural/urban within each cancer type\n")
cat("(compare against cell counts above before trusting any specific rate):\n")
print(svyby(~has_cardiac, ~cancer_type + rural, svy_cohort_card, svymean, na.rm = TRUE))


# ----- 8. SECONDARY: MORTALITY AND LOS BY RURAL/URBAN -----
cat("\n========== SECONDARY: mortality by rural/urban ==========\n")
print(svyby(~DIED, ~rural, svy_cohort_card, svymean, na.rm = TRUE))
model_mortality <- svyglm(DIED ~ rural + AGE + FEMALE, design = svy_cohort_card, family = quasibinomial())
print(round(exp(cbind(OR = coef(model_mortality), confint(model_mortality))), 3))
rm(model_mortality); gc()

cat("\n========== SECONDARY: length of stay by rural/urban ==========\n")
print(svyby(~LOS, ~rural, svy_cohort_card, svymean, na.rm = TRUE))
model_los <- svyglm(LOS ~ rural + AGE + FEMALE, design = svy_cohort_card, family = gaussian())
print(round(cbind(Estimate = coef(model_los), confint(model_los)), 3))
rm(model_los); gc()

cat("\n========== ALL ANALYSES COMPLETE ==========\n")
