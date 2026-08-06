# ============================================================
# NIS 2023 Core: Transfusion practices and outcomes across
# different adult patient populations
#
# RQ: Among ALL adult hospitalizations (not restricted to any one
# diagnosis), who receives a blood product transfusion, and is
# transfusion associated with different in-hospital mortality and
# length of stay -- and do either of these vary by age, sex,
# insurance, or neighborhood income?
#
# COHORT: All adults (age 18+), no diagnosis restriction -- this
# was a deliberate scope decision (confirmed with the user) to
# match "transfusion practices across different patient
# populations" literally, rather than picking an arbitrary set of
# "transfusion-relevant" diagnoses that would narrow
# generalizability and introduce a selection judgment call.
#
# EXPOSURE/OUTCOME OF INTEREST: transfusion of ANY blood product
# (not just RBCs), identified via ICD-10-PCS. Verified table
# structure (not guessed): Section 3 = Administration, Body
# System 0 = Circulatory, Root Operation 2 = Transfusion --
# so ANY code starting "302" is a transfusion procedure,
# regardless of body part (peripheral/central vein or artery,
# character 4), substance (whole blood/plasma/RBC/platelets,
# character 6), or autologous/nonautologous status (character 7).
# "Massive transfusion protocol" (e.g. >5 units/24h) is NOT
# identifiable here -- PCS codes mark that a transfusion happened,
# not how many units, and NIS Core lacks the hour-level procedure
# timing such a definition would need. This script therefore uses
# "any transfusion vs. none" only, per the same design decision
# already made with the user.
#
# TWO SEPARATE QUESTIONS, SAME STRUCTURE AS THE IVC-FILTER SCRIPT:
#   (1) PRACTICE: who gets transfused? (transfusion ~ demographics)
#   (2) OUTCOME: is transfusion associated with mortality/LOS?
#       (DIED/LOS ~ transfusion + demographics)
# IMPORTANT CAVEAT FOR (2): this is confounded by indication --
# patients get transfused because they are already bleeding/sicker,
# so a positive association between transfusion and mortality
# almost certainly reflects reverse causation/confounding, NOT
# transfusion causing harm. This script reports the association;
# it does not and cannot claim transfusion causes worse outcomes.
#
# STRATIFICATION: age group (18-44, 45-64, 65-79, 80+), sex,
# insurance (PAY1, same categorization validated in the IVC filter
# script), income quartile (ZIPINC_QRTL). Race is not available in
# this extract (consistent with every other project in this repo).
#
# ARCHITECTURE: same chunked base-R streaming approach as the
# other 2023 Core scripts. Because the cohort here is nearly the
# entire adult file (no diagnosis filter), the transfusion PCS
# scan runs on every row rather than a small cohort subset -- this
# is more expensive than earlier scripts' cohort-only scans, but
# the same chunked approach already proved it can handle a full
# 6.74M-row scan.
# ============================================================

suppressMessages({
  library(tidyverse)
  library(survey)
})

nis_file       <- "~/Downloads/NIS_2023/NIS_2023_Core.ASC"
checkpoint_rds <- "~/Downloads/NIS_2023/nis_2023_transfusion_checkpoint.rds"


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
  "PAY1", 542L, 543L, "numeric", "N2PF",
  "ZIPINC_QRTL", 629L, 630L, "numeric", "N2PF"
)
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


# ----- 2. ICD-10-PCS CODE DEFINITION (no decimal points) -----
transfusion_regex <- "^302"

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

    scalar_df$cohort <- scalar_df$AGE >= 18 & !is.na(scalar_df$AGE)
    cohort_mask <- scalar_df$cohort

    scalar_df$transfusion <- NA
    if (any(cohort_mask)) {
      sub_lines <- lines[cohort_mask]
      scalar_df$transfusion[cohort_mask] <- any_match(sub_lines, pr_starts, pr_ends, transfusion_regex)
    }

    all_chunks[[chunk_i]] <- scalar_df
    cat("Chunk", chunk_i, "--", n, "rows,", sum(cohort_mask), "cohort rows so far this chunk\n")
  }
  close(con)

  full_derived <- bind_rows(all_chunks)
  cat("\nTotal rows scanned:", nrow(full_derived), "\n")
  cat("Total adult cohort rows:", sum(full_derived$cohort), "\n")

  dir.create(dirname(checkpoint_rds), showWarnings = FALSE, recursive = TRUE)
  saveRDS(full_derived, checkpoint_rds)
  cat("Saved checkpoint to", checkpoint_rds, "\n")
}


# ----- 4. DERIVE AGE GROUP AND INSURANCE (same PAY1 mapping as the -----
# ----- IVC filter script) -----
full_derived <- full_derived %>%
  mutate(
    age_group = case_when(
      AGE >= 18 & AGE <= 44 ~ "18-44",
      AGE >= 45 & AGE <= 64 ~ "45-64",
      AGE >= 65 & AGE <= 79 ~ "65-79",
      AGE >= 80 ~ "80+",
      TRUE ~ NA_character_
    ),
    age_group = factor(age_group, levels = c("18-44", "45-64", "65-79", "80+")),
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
                                   "Uninsured/Self-pay", "Other")),
    income_quartile = factor(ZIPINC_QRTL, levels = 1:4,
                              labels = c("Q1 (lowest)", "Q2", "Q3", "Q4 (highest)"))
  )


# ----- 5. SURVEY DESIGN + SUBPOPULATION SUBSET -----
svy_design_tx <- svydesign(
  ids = ~HOSP_NIS, strata = ~NIS_STRATUM, weights = ~DISCWT,
  data = full_derived, nest = TRUE
)
svy_cohort_tx <- subset(svy_design_tx, cohort)

cat("\n========== COHORT SIZE ==========\n")
cat("Unweighted cohort n:", nrow(svy_cohort_tx$variables), "\n")
cat("Weighted (national estimate) cohort n:", round(sum(weights(svy_cohort_tx))), "\n")

cat("\nOverall weighted transfusion rate:\n")
print(svymean(~transfusion, svy_cohort_tx, na.rm = TRUE))


# ----- 6. PRACTICE: WHO GETS TRANSFUSED? -----
cat("\n========== PRACTICE: transfusion rate by patient population ==========\n")

cat("\nWeighted transfusion rate by age group:\n")
print(svyby(~transfusion, ~age_group, svy_cohort_tx, svymean, na.rm = TRUE))

cat("\nWeighted transfusion rate by sex:\n")
print(svyby(~transfusion, ~FEMALE, svy_cohort_tx, svymean, na.rm = TRUE))

cat("\nWeighted transfusion rate by insurance:\n")
print(svyby(~transfusion, ~insurance, svy_cohort_tx, svymean, na.rm = TRUE))

cat("\nWeighted transfusion rate by income quartile:\n")
print(svyby(~transfusion, ~income_quartile, svy_cohort_tx, svymean, na.rm = TRUE))

cat("\nAdjusted model -- transfusion ~ age_group + FEMALE + insurance + income_quartile:\n")
model_practice <- svyglm(
  transfusion ~ age_group + FEMALE + insurance + income_quartile,
  design = svy_cohort_tx, family = quasibinomial()
)
print(round(exp(cbind(OR = coef(model_practice), confint(model_practice))), 3))


# ----- 7. OUTCOME: IS TRANSFUSION ASSOCIATED WITH MORTALITY/LOS? -----
# Confounded-by-indication caveat (see header) applies to everything below.
cat("\n========== OUTCOME: mortality/LOS by transfusion status ==========\n")
cat("(NOTE: observational association only -- transfused patients are\n")
cat("sicker/actively bleeding by definition, so this does NOT show\n")
cat("whether transfusion helps or harms; see header comment.)\n\n")

cat("Weighted mortality by transfusion status:\n")
print(svyby(~DIED, ~transfusion, svy_cohort_tx, svymean, na.rm = TRUE))

cat("\nWeighted mean LOS by transfusion status:\n")
print(svyby(~LOS, ~transfusion, svy_cohort_tx, svymean, na.rm = TRUE))

cat("\nAdjusted mortality model -- DIED ~ transfusion + age_group + FEMALE + insurance + income_quartile:\n")
model_mortality <- svyglm(
  DIED ~ transfusion + age_group + FEMALE + insurance + income_quartile,
  design = svy_cohort_tx, family = quasibinomial()
)
print(round(exp(cbind(OR = coef(model_mortality), confint(model_mortality))), 3))

cat("\nAdjusted LOS model -- LOS ~ transfusion + age_group + FEMALE + insurance + income_quartile:\n")
model_los <- svyglm(
  LOS ~ transfusion + age_group + FEMALE + insurance + income_quartile,
  design = svy_cohort_tx, family = gaussian()
)
print(round(cbind(Estimate = coef(model_los), confint(model_los)), 3))

cat("\n========== ALL ANALYSES COMPLETE ==========\n")
