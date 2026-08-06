# ============================================================
# NIS 2023 Early-Onset Cancer: cancer-type-specific rural/urban
# gaps for the 3 outcomes with a significant rural x cancer-type
# interaction (sepsis, mortality, mechanical ventilation)
#
# CONTEXT: the pooled (main-effect) rural vs. urban comparisons for
# most outcomes were null. But the global interaction tests
# (regTermTest, printed inside analyze_binary_outcome()) were
# significant for sepsis, mortality, and mechanical ventilation --
# meaning the rural/urban gap is not uniform across cancer types.
# This script pulls out exactly where the gap lives for those 3
# outcomes, cancer type by cancer type, with unweighted cell counts
# shown alongside every rate so nothing gets over-interpreted from
# a handful of events.
#
# Run this AFTER scripts/nis_2023_early_onset_cancer_rural_urban.R
# has finished in the same R session -- reuses svy_cohort_eo
# already in memory (no re-scanning, no re-fitting anything here).
#
# Produces, in ~/Desktop/NIS_2023_Tables/:
#   CancerType_gap_sepsis.csv
#   CancerType_gap_mortality.csv
#   CancerType_gap_mechvent.csv
# Each row = one cancer type, sorted by |gap| descending, with a
# `reliable` flag (FALSE if either rural or urban n < 10 for that
# cancer type -- treat those rows as hypothesis-generating only).
# ============================================================

suppressMessages(library(tidyverse))

out_dir <- "~/Desktop/NIS_2023_Tables"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

build_gap_table <- function(outcome_name, outcome_label) {
  form_rate <- as.formula(paste0("~", outcome_name))

  rates <- svyby(form_rate, ~cancer_type + rural, svy_cohort_eo, svymean, na.rm = TRUE) %>%
    as_tibble()

  # svyby's column names differ for logical vs 0/1-numeric outcomes
  rate_col <- if (paste0(outcome_name, "TRUE") %in% names(rates)) paste0(outcome_name, "TRUE") else outcome_name
  se_col <- if (paste0("se.", outcome_name, "TRUE") %in% names(rates)) paste0("se.", outcome_name, "TRUE") else "se"

  counts <- svy_cohort_eo$variables %>%
    filter(!is.na(rural), !is.na(cancer_type)) %>%
    count(cancer_type, rural, name = "n")

  wide <- rates %>%
    select(cancer_type, rural, rate = all_of(rate_col)) %>%
    left_join(counts, by = c("cancer_type", "rural")) %>%
    pivot_wider(names_from = rural, values_from = c(rate, n))

  wide <- wide %>%
    mutate(
      gap_pct_points = round((rate_Rural - rate_Urban) * 100, 1),
      Urban_pct = sprintf("%.1f%% (n=%d)", rate_Urban * 100, n_Urban),
      Rural_pct = sprintf("%.1f%% (n=%d)", rate_Rural * 100, n_Rural),
      reliable = n_Rural >= 10 & n_Urban >= 10  # flag if either side has <10 for context
    ) %>%
    arrange(desc(abs(gap_pct_points))) %>%
    select(cancer_type, Urban_pct, Rural_pct, gap_pct_points, reliable)

  cat("\n========== ", outcome_label, " by cancer type (sorted by gap size) ==========\n")
  cat("(gap_pct_points = Rural rate minus Urban rate, in percentage points;\n")
  cat("reliable = FALSE means fewer than 10 patients on the rural or urban\n")
  cat("side for that cancer type -- treat as hypothesis-generating only)\n")
  print(wide, n = 20)
  wide
}

result_sepsis <- build_gap_table("sepsis", "Sepsis")
write.csv(result_sepsis, file.path(out_dir, "CancerType_gap_sepsis.csv"), row.names = FALSE)

result_died <- build_gap_table("DIED", "Mortality")
write.csv(result_died, file.path(out_dir, "CancerType_gap_mortality.csv"), row.names = FALSE)

result_vent <- build_gap_table("mech_vent", "Mechanical Ventilation")
write.csv(result_vent, file.path(out_dir, "CancerType_gap_mechvent.csv"), row.names = FALSE)

cat("\nAll 3 cancer-type gap tables saved to", out_dir, "\n")
