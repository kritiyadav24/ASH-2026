# ============================================================
# NIS 2023 IVC Filter Analysis: Forest plot, Table 2, and
# stratified mortality analysis
#
# Run this AFTER scripts/nis_2023_ivc_filter_insurance_disparities.R
# has finished in the same R session -- it reuses svy_cohort and
# model_mortality (the adjusted mortality model) already in
# memory, rather than re-fitting anything or re-scanning the file.
#
# Produces, in ~/Desktop/NIS_2023_Tables/:
#   1. Forest_plot_mortality_by_insurance.png -- adjusted OR for
#      in-hospital mortality by insurance (vs. Private), from the
#      headline finding.
#   2. Table2_adjusted_mortality_model.csv -- the full adjusted
#      mortality model in clean manuscript format (labeled terms,
#      OR (95% CI), p-value).
#   3. Table3_stratified_mortality.csv -- weighted in-hospital
#      mortality (with 95% CI and unweighted n) stratified by
#      insurance x IVC filter status.
# ============================================================

suppressMessages(library(tidyverse))

out_dir <- "~/Desktop/NIS_2023_Tables"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)


# ----- 1. FOREST PLOT: adjusted mortality ORs by insurance -----
insurance_terms <- c("insuranceMedicare", "insuranceMedicaid",
                      "insuranceUninsured/Self-pay", "insuranceOther")
insurance_labels <- c("Medicare", "Medicaid", "Uninsured/Self-pay", "Other")

co <- coef(model_mortality)
ci <- confint(model_mortality)

forest_data <- tibble(
  Insurance = factor(insurance_labels, levels = rev(insurance_labels)),
  OR = exp(co[insurance_terms]),
  CI_low = exp(ci[insurance_terms, 1]),
  CI_high = exp(ci[insurance_terms, 2])
)
forest_data <- bind_rows(
  tibble(Insurance = factor("Private (ref)", levels = c(levels(forest_data$Insurance), "Private (ref)")),
         OR = 1, CI_low = NA, CI_high = NA),
  forest_data
) %>% mutate(Insurance = fct_relevel(Insurance, "Private (ref)", after = Inf))

forest_plot <- ggplot(forest_data, aes(x = OR, y = Insurance)) +
  geom_vline(xintercept = 1, linetype = "dashed", color = "grey50") +
  geom_errorbarh(aes(xmin = CI_low, xmax = CI_high), height = 0.15, na.rm = TRUE) +
  geom_point(size = 3, na.rm = TRUE) +
  scale_x_log10() +
  labs(
    title = "Adjusted In-Hospital Mortality by Insurance",
    subtitle = "Reference: Privately insured",
    x = "Adjusted Odds Ratio (log scale)", y = NULL
  ) +
  theme_minimal(base_size = 13) +
  theme(panel.grid.minor = element_blank())

ggsave(file.path(out_dir, "Forest_plot_mortality_by_insurance.png"),
       forest_plot, width = 7, height = 4, dpi = 300)
cat("Saved forest plot to", file.path(out_dir, "Forest_plot_mortality_by_insurance.png"), "\n")


# ----- 2. TABLE 2: full adjusted mortality model, clean format -----
term_labels <- c(
  "ivc_filterTRUE" = "IVC Filter Placed",
  "insuranceMedicare" = "Insurance: Medicare",
  "insuranceMedicaid" = "Insurance: Medicaid",
  "insuranceUninsured/Self-pay" = "Insurance: Uninsured/Self-pay",
  "insuranceOther" = "Insurance: Other",
  "AGE" = "Age (per year)",
  "FEMALE" = "Female Sex",
  "cancer_typeGI" = "Cancer Type: GI",
  "cancer_typeGynecologic" = "Cancer Type: Gynecologic",
  "cancer_typeHematologic" = "Cancer Type: Hematologic",
  "cancer_typeLung" = "Cancer Type: Lung",
  "cancer_typeOther" = "Cancer Type: Other",
  "cancer_typePancreatic" = "Cancer Type: Pancreatic",
  "metastaticTRUE" = "Metastatic Disease",
  "heart_failureTRUE" = "Heart Failure",
  "ckdTRUE" = "Chronic Kidney Disease",
  "ivc_filterTRUE:insuranceMedicare" = "Filter x Medicare",
  "ivc_filterTRUE:insuranceMedicaid" = "Filter x Medicaid",
  "ivc_filterTRUE:insuranceUninsured/Self-pay" = "Filter x Uninsured/Self-pay",
  "ivc_filterTRUE:insuranceOther" = "Filter x Other"
)

model_summary <- summary(model_mortality)$coefficients
table2 <- tibble(
  term = rownames(model_summary),
  OR = exp(co[term]),
  CI_low = exp(ci[term, 1]),
  CI_high = exp(ci[term, 2]),
  p_value = model_summary[, "Pr(>|t|)"]
) %>%
  mutate(
    Variable = ifelse(term %in% names(term_labels), term_labels[term], term),
    `Adjusted OR (95% CI)` = sprintf("%.2f (%.2f-%.2f)", OR, CI_low, CI_high),
    `p-value` = ifelse(p_value < 0.001, "<0.001", sprintf("%.3f", p_value))
  ) %>%
  select(Variable, `Adjusted OR (95% CI)`, `p-value`) %>%
  filter(Variable != "(Intercept)")

print(table2, n = 30)
write.csv(table2, file.path(out_dir, "Table2_adjusted_mortality_model.csv"), row.names = FALSE)
cat("Saved Table 2 to", file.path(out_dir, "Table2_adjusted_mortality_model.csv"), "\n")


# ----- 3. STRATIFIED MORTALITY BY INSURANCE + FILTER STATUS -----
strat_n <- svy_cohort$variables %>%
  filter(!is.na(insurance)) %>%
  count(insurance, ivc_filter, name = "Unweighted n")

strat_mortality <- svyby(~DIED, ~insurance + ivc_filter, svy_cohort, svymean, na.rm = TRUE) %>%
  as_tibble() %>%
  left_join(strat_n, by = c("insurance", "ivc_filter")) %>%
  transmute(
    Insurance = insurance,
    `IVC Filter` = ifelse(ivc_filter, "Yes", "No"),
    `Unweighted n`,
    `In-Hospital Mortality (95% CI)` = sprintf("%.1f%% (%.1f-%.1f%%)", DIED * 100,
      pmax(0, (DIED - 1.96 * se) * 100), pmin(100, (DIED + 1.96 * se) * 100))
  ) %>%
  arrange(Insurance, `IVC Filter`)

print(strat_mortality, n = 20)
write.csv(strat_mortality, file.path(out_dir, "Table3_stratified_mortality.csv"), row.names = FALSE)
cat("Saved stratified mortality table to", file.path(out_dir, "Table3_stratified_mortality.csv"), "\n")

cat("\nAll 3 deliverables saved to", out_dir, "\n")
