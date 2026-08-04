# ============================================================
# NIS 2023 IVC Filter Analysis: Three secondary findings
# supporting the mortality-disparity primary claim
#
# Primary claim (see abstract draft): among hospitalized cancer
# patients with VTE, uninsured and other-payer patients have
# significantly higher in-hospital mortality than privately
# insured patients, independent of IVC filter placement.
#
# Run this AFTER scripts/nis_2023_ivc_filter_insurance_disparities.R
# has finished in the same R session -- reuses svy_cohort and
# model_mortality already in memory.
#
# Produces, in ~/Desktop/NIS_2023_Tables/:
#   1. Bleeding-contraindication rate by insurance -- context for
#      interpreting appropriateness of filter use across groups.
#   2. IVC filter's association with mortality, WITHIN each
#      insurance group (not just the raw interaction term, which
#      is only interpretable relative to Private's filter effect).
#   3. Mortality by insurance within each cancer type, plus a
#      global test of whether the insurance-mortality association
#      varies by cancer type (same sparse-cell caution as the
#      filter-placement x cancer-type analysis -- death is a
#      rarer outcome, so cell counts are checked and reported
#      alongside descriptive rates rather than trusting individual
#      interaction coefficients).
# ============================================================

suppressMessages(library(tidyverse))

out_dir <- "~/Desktop/NIS_2023_Tables"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)


# ----- SECONDARY 1: Bleeding contraindication rate by insurance -----
bleeding_by_ins <- svyby(~bleeding_contra, ~insurance, svy_cohort, svymean, na.rm = TRUE)
secondary1 <- bleeding_by_ins %>%
  as_tibble() %>%
  transmute(
    Insurance = insurance,
    `Bleeding Contraindication Rate (95% CI)` = sprintf("%.1f%% (%.1f-%.1f%%)",
      bleeding_contraTRUE * 100,
      pmax(0, (bleeding_contraTRUE - 1.96 * se.bleeding_contraTRUE) * 100),
      pmin(100, (bleeding_contraTRUE + 1.96 * se.bleeding_contraTRUE) * 100))
  )
cat("========== SECONDARY 1: Bleeding contraindication rate by insurance ==========\n")
print(secondary1)
write.csv(secondary1, file.path(out_dir, "Secondary1_bleeding_by_insurance.csv"), row.names = FALSE)


# ----- SECONDARY 2: Does the IVC filter's mortality association differ by insurance? -----
co <- coef(model_mortality)
V <- vcov(model_mortality)

insurance_groups <- c("Private", "Medicare", "Medicaid", "Uninsured/Self-pay", "Other")
interaction_terms <- c(NA, "ivc_filterTRUE:insuranceMedicare", "ivc_filterTRUE:insuranceMedicaid",
                        "ivc_filterTRUE:insuranceUninsured/Self-pay", "ivc_filterTRUE:insuranceOther")

filter_effect_by_group <- function(interaction_term) {
  if (is.na(interaction_term)) {
    beta <- co["ivc_filterTRUE"]
    var_beta <- V["ivc_filterTRUE", "ivc_filterTRUE"]
  } else {
    beta <- co["ivc_filterTRUE"] + co[interaction_term]
    var_beta <- V["ivc_filterTRUE", "ivc_filterTRUE"] + V[interaction_term, interaction_term] +
      2 * V["ivc_filterTRUE", interaction_term]
  }
  se_beta <- sqrt(var_beta)
  tibble(OR = exp(beta), CI_low = exp(beta - 1.96 * se_beta), CI_high = exp(beta + 1.96 * se_beta))
}

secondary2 <- map2_dfr(insurance_groups, interaction_terms, function(g, term) {
  bind_cols(Insurance = g, filter_effect_by_group(term))
}) %>%
  mutate(`Filter Effect on Mortality, OR (95% CI)` = sprintf("%.2f (%.2f-%.2f)", OR, CI_low, CI_high)) %>%
  select(Insurance, `Filter Effect on Mortality, OR (95% CI)`)

cat("\n========== SECONDARY 2: Does the IVC filter's mortality association differ by insurance? ==========\n")
cat("(OR < 1 = filter associated with lower mortality within that insurance group)\n")
print(secondary2)
write.csv(secondary2, file.path(out_dir, "Secondary2_filter_effect_by_insurance.csv"), row.names = FALSE)


# ----- SECONDARY 3: Mortality disparity by cancer type x insurance -----
mortality_cell_counts <- svy_cohort$variables %>%
  filter(!is.na(insurance), !is.na(cancer_type)) %>%
  count(insurance, cancer_type, DIED) %>%
  pivot_wider(names_from = DIED, values_from = n, values_fill = 0, names_prefix = "died_")
cat("\n========== SECONDARY 3: Mortality disparity by cancer type ==========\n")
cat("\nCell counts (insurance x cancer_type x mortality) -- flag anything sparse before interpreting:\n")
print(mortality_cell_counts, n = 100)
write.csv(mortality_cell_counts, file.path(out_dir, "Secondary3_cell_counts.csv"), row.names = FALSE)

model_mortality_by_cancer <- svyglm(
  DIED ~ insurance * cancer_type + AGE + FEMALE + ivc_filter,
  design = svy_cohort, family = quasibinomial()
)
mortality_interaction_test <- regTermTest(model_mortality_by_cancer, ~insurance:cancer_type)
cat("\nGlobal test: does the insurance-mortality association vary by cancer type?\n")
cat("(valid even with sparse cells -- individual coefficients are not reported)\n")
print(mortality_interaction_test)

mortality_by_cancer_insurance <- svyby(~DIED, ~cancer_type + insurance, svy_cohort, svymean, na.rm = TRUE) %>%
  as_tibble() %>%
  transmute(cancer_type, insurance,
            `In-Hospital Mortality (95% CI)` = sprintf("%.1f%% (%.1f-%.1f%%)", DIED * 100,
              pmax(0, (DIED - 1.96 * se) * 100), pmin(100, (DIED + 1.96 * se) * 100))) %>%
  left_join(mortality_cell_counts %>% mutate(n_total = rowSums(select(., starts_with("died_")))) %>%
              select(insurance, cancer_type, n_total),
            by = c("cancer_type", "insurance")) %>%
  rename(`Cancer Type` = cancer_type, Insurance = insurance, `Unweighted n` = n_total) %>%
  arrange(`Cancer Type`, Insurance)

cat("\nDescriptive: mortality by insurance within each cancer type\n")
cat("(compare against cell counts above -- don't trust rates from cells with < ~10 deaths):\n")
print(mortality_by_cancer_insurance, n = 50)
write.csv(mortality_by_cancer_insurance, file.path(out_dir, "Secondary3_mortality_by_cancer_and_insurance.csv"), row.names = FALSE)

cat("\nAll 3 secondary analyses saved to", out_dir, "\n")
