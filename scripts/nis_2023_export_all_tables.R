# ============================================================
# NIS 2023 IVC Filter Analysis: Export all results as clean CSVs
#
# Run this AFTER scripts/nis_2023_ivc_filter_insurance_disparities.R
# has finished in the same R session -- it reuses svy_cohort,
# cell_counts, and the fitted models (model_unadj, model_adj,
# model_interaction, model_mortality, model_appropriateness)
# rather than re-fitting anything or re-scanning the data file.
#
# Produces one CSV per result table (12 total) in
# ~/Desktop/NIS_2023_Tables/, each with weighted rates + 95% CIs
# formatted as plain percentages, or odds ratios + CIs for the
# regression models -- ready to open directly in Excel or paste
# into a manuscript/poster table.
# ============================================================

suppressMessages(library(tidyverse))

out_dir <- "~/Desktop/NIS_2023_Tables"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
save_table <- function(df, filename) {
  path <- file.path(out_dir, filename)
  write.csv(df, path, row.names = FALSE)
  cat("Saved:", path, "\n")
}

format_or_table <- function(model) {
  co <- coef(model)
  ci <- confint(model)
  tibble(
    Term = names(co),
    OR = round(exp(co), 3),
    `CI Lower` = round(exp(ci[, 1]), 3),
    `CI Upper` = round(exp(ci[, 2]), 3)
  )
}

# ---- Table 1a: filter rate + mortality by insurance ----
filter_by_ins <- svyby(~ivc_filter, ~insurance, svy_cohort, svymean, na.rm = TRUE)
mortality_by_ins <- svyby(~DIED, ~insurance, svy_cohort, svymean, na.rm = TRUE)
n_by_ins <- svy_cohort$variables %>% filter(!is.na(insurance)) %>% count(insurance, name = "Unweighted n")

table1_insurance <- n_by_ins %>%
  left_join(filter_by_ins %>% transmute(insurance,
              `IVC Filter Rate (95% CI)` = sprintf("%.1f%% (%.1f-%.1f%%)", ivc_filterTRUE*100,
                pmax(0,(ivc_filterTRUE-1.96*se.ivc_filterTRUE)*100), pmin(100,(ivc_filterTRUE+1.96*se.ivc_filterTRUE)*100))),
            by = "insurance") %>%
  left_join(mortality_by_ins %>% transmute(insurance,
              `In-Hospital Mortality (95% CI)` = sprintf("%.1f%% (%.1f-%.1f%%)", DIED*100,
                pmax(0,(DIED-1.96*se)*100), pmin(100,(DIED+1.96*se)*100))),
            by = "insurance") %>%
  rename(Insurance = insurance)
print(table1_insurance)
save_table(table1_insurance, "Table1_by_insurance.csv")

# ---- Table 1b: filter rate by cancer type ----
filter_by_cancer <- svyby(~ivc_filter, ~cancer_type, svy_cohort, svymean, na.rm = TRUE)
table1_cancer <- filter_by_cancer %>%
  transmute(`Cancer Type` = cancer_type,
            `IVC Filter Rate (95% CI)` = sprintf("%.1f%% (%.1f-%.1f%%)", ivc_filterTRUE*100,
              pmax(0,(ivc_filterTRUE-1.96*se.ivc_filterTRUE)*100), pmin(100,(ivc_filterTRUE+1.96*se.ivc_filterTRUE)*100)))
print(table1_cancer)
save_table(table1_cancer, "Table1_by_cancer_type.csv")

# ---- Table 1c: filter rate by bleeding contraindication ----
filter_by_bleed <- svyby(~ivc_filter, ~bleeding_contra, svy_cohort, svymean, na.rm = TRUE)
table1_bleeding <- filter_by_bleed %>%
  transmute(`Bleeding Contraindication` = bleeding_contra,
            `IVC Filter Rate (95% CI)` = sprintf("%.1f%% (%.1f-%.1f%%)", ivc_filterTRUE*100,
              pmax(0,(ivc_filterTRUE-1.96*se.ivc_filterTRUE)*100), pmin(100,(ivc_filterTRUE+1.96*se.ivc_filterTRUE)*100)))
print(table1_bleeding)
save_table(table1_bleeding, "Table1_by_bleeding_contraindication.csv")

# ---- Primary analysis: unadjusted + adjusted OR tables ----
table_primary_unadj <- format_or_table(model_unadj)
print(table_primary_unadj)
save_table(table_primary_unadj, "Primary_unadjusted_OR.csv")

table_primary_adj <- format_or_table(model_adj)
print(table_primary_adj)
save_table(table_primary_adj, "Primary_adjusted_OR.csv")

# ---- Secondary Q1: cell counts + global test + descriptive rates ----
print(cell_counts)
save_table(cell_counts, "Q1_cell_counts.csv")

q1_test <- regTermTest(model_interaction, ~insurance:cancer_type)
q1_test_table <- tibble(
  Test = "Insurance x Cancer Type interaction",
  F_statistic = round(q1_test$Ftest[1], 3),
  df1 = q1_test$df[1], df2 = q1_test$ddf,
  p_value = signif(q1_test$p[1], 3)
)
print(q1_test_table)
save_table(q1_test_table, "Q1_global_interaction_test.csv")

q1_descriptive_raw <- svyby(~ivc_filter, ~cancer_type + insurance, svy_cohort, svymean, na.rm = TRUE)
q1_descriptive <- q1_descriptive_raw %>%
  as_tibble() %>%
  transmute(cancer_type, insurance,
            `IVC Filter Rate (95% CI)` = sprintf("%.1f%% (%.1f-%.1f%%)", ivc_filterTRUE*100,
              pmax(0,(ivc_filterTRUE-1.96*se.ivc_filterTRUE)*100), pmin(100,(ivc_filterTRUE+1.96*se.ivc_filterTRUE)*100))) %>%
  left_join(cell_counts %>% transmute(cancer_type, insurance, `Unweighted n` = filter_FALSE + filter_TRUE, `n with filter` = filter_TRUE),
            by = c("cancer_type", "insurance")) %>%
  rename(`Cancer Type` = cancer_type, Insurance = insurance)
print(q1_descriptive, n = 50)
save_table(q1_descriptive, "Q1_descriptive_by_cancer_and_insurance.csv")

# ---- Secondary Q2: mortality descriptive + adjusted OR table ----
q2_descriptive_raw <- svyby(~DIED, ~insurance + ivc_filter, svy_cohort, svymean, na.rm = TRUE)
q2_descriptive <- q2_descriptive_raw %>%
  as_tibble() %>%
  transmute(Insurance = insurance, `IVC Filter` = ivc_filter,
            `In-Hospital Mortality (95% CI)` = sprintf("%.1f%% (%.1f-%.1f%%)", DIED*100,
              pmax(0,(DIED-1.96*se)*100), pmin(100,(DIED+1.96*se)*100)))
print(q2_descriptive)
save_table(q2_descriptive, "Q2_mortality_descriptive.csv")

table_q2_model <- format_or_table(model_mortality)
print(table_q2_model)
save_table(table_q2_model, "Q2_mortality_adjusted_OR.csv")

# ---- Secondary Q3: appropriateness descriptive + adjusted OR table ----
q3_descriptive_raw <- svyby(~ivc_filter, ~insurance + bleeding_contra, svy_cohort, svymean, na.rm = TRUE)
q3_descriptive <- q3_descriptive_raw %>%
  as_tibble() %>%
  transmute(Insurance = insurance, `Bleeding Contraindication` = bleeding_contra,
            `IVC Filter Rate (95% CI)` = sprintf("%.1f%% (%.1f-%.1f%%)", ivc_filterTRUE*100,
              pmax(0,(ivc_filterTRUE-1.96*se.ivc_filterTRUE)*100), pmin(100,(ivc_filterTRUE+1.96*se.ivc_filterTRUE)*100)))
print(q3_descriptive)
save_table(q3_descriptive, "Q3_appropriateness_descriptive.csv")

table_q3_model <- format_or_table(model_appropriateness)
print(table_q3_model)
save_table(table_q3_model, "Q3_appropriateness_adjusted_OR.csv")

cat("\nAll 12 tables saved to", out_dir, "\n")
