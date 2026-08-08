# ============================================================
# NIS 2023 Core: Provoked vs. unprovoked VTE across ALL general
# medicine admissions (not restricted to cancer)
#
# RQ: Among adults hospitalized with a principal diagnosis of VTE
# (PE or lower-extremity DVT), what proportion present as provoked
# vs. unprovoked, and do outcomes (mortality, LOS, IVC filter
# placement, bleeding complications) differ between the two groups?
#
# This is a well-established clinical distinction (drives
# anticoagulation duration decisions) rather than a novelty play --
# after extensive literature-gap checking this session found the
# "unstudied disease + disparity axis" space largely exhausted in
# NIS, this project deliberately reuses a known-important clinical
# framework and executes it carefully, rather than chasing novelty.
#
# COHORT: Age 18+, PRINCIPAL diagnosis (I10_DX1) is VTE -- same
# codes already validated in the IVC filter/insurance script:
# I26 (pulmonary embolism), I824 (acute lower-extremity DVT).
#
# PROVOKED VS. UNPROVOKED CLASSIFICATION -- IMPORTANT LIMITATION:
# true clinical "provoked VTE" classification often depends on
# events BEFORE this hospitalization (surgery 3 weeks ago, a long
# flight, recent immobilization at home) that NIS Core cannot see --
# it is a single-admission snapshot with no linkage to prior
# encounters. This script can only detect provoking factors
# DOCUMENTED WITHIN THIS SAME ADMISSION:
#   - Active cancer: secondary diagnosis is a malignancy ("C" code),
#     excluding C44 (non-melanoma skin cancer, not a recognized VTE
#     risk factor)
#   - Recent/concurrent major surgery: PCLASS_ORPROC == 1 during
#     THIS admission (postoperative VTE). NOTE: PCLASS_ORPROC's
#     exact value coding is a working assumption, not yet
#     HCUP-confirmed the way PL_NCHS2 now is -- Step 3 prints its
#     raw distribution; check it looks like a clean small integer
#     scheme before trusting this factor.
#   - Trauma/fracture: secondary diagnosis is a fracture (ICD-10-CM
#     S-chapter fracture codes across major body regions)
#   - Pregnancy/postpartum: secondary diagnosis in the O-chapter
#     (pregnancy, childbirth, puerperium)
# "Unprovoked" = none of the above present in this admission's
# record. This will systematically UNDER-classify true provoked
# cases whose provoking event isn't visible in this admission
# (e.g. surgery 2 weeks ago at a different visit) -- stated
# explicitly here and should be repeated in any write-up, not
# treated as a precise clinical determination.
#
# OUTCOMES: in-hospital mortality, length of stay, IVC filter
# placement rate (ICD-10-PCS 06H0-06H3, validated in the earlier
# script), and bleeding complications (I61 intracerebral hemorrhage,
# K922 GI hemorrhage NOS, D62 acute posthemorrhagic anemia -- same
# bleeding_contra proxy used in the IVC filter script).
#
# STRATIFICATION: age, sex. Insurance/income intentionally NOT
# included as a primary axis here (deliberate scope decision --
# insurance was already the focus of the IVC filter project).
#
# ARCHITECTURE: same chunked base-R streaming approach as every
# other script in this repo.
# ============================================================

suppressMessages({
  library(tidyverse)
  library(survey)
})

nis_file       <- "~/Downloads/NIS_2023/NIS_2023_Core.ASC"
checkpoint_rds <- "~/Downloads/NIS_2023/nis_2023_provoked_vte_checkpoint.rds"


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
  "PCLASS_ORPROC", 544L, 545L, "numeric", "N2PF"
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
vte_regex                 <- "^(I26|I824)"
cancer_regex               <- "^C"
skin_cancer_exclude_regex  <- "^C44"
fracture_regex             <- "^S(02|12|22|32|42|52|62|72|82|92)"
pregnancy_regex            <- "^O"
ivc_filter_regex           <- "^06H[034]"
bleeding_regex             <- "^(I61|K922|D62)"

any_match <- function(lines, starts, ends, regex) {
  hit <- rep(FALSE, length(lines))
  for (i in seq_along(starts)) {
    codes <- trimws(substr(lines, starts[i], ends[i]))
    hit <- hit | grepl(regex, codes)
  }
  hit
}


# ----- 3. STREAM THE FILE, WITH A DIAGNOSTIC CHECK FOR PCLASS_ORPROC -----
if (file.exists(checkpoint_rds)) {
  cat("Found existing checkpoint, loading instead of re-scanning:", checkpoint_rds, "\n")
  full_derived <- readRDS(checkpoint_rds)
} else {
  con <- file(nis_file, "r")
  chunk_size <- 200000
  all_chunks <- list()
  chunk_i <- 0
  diagnostic_printed <- FALSE

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

    if (!diagnostic_printed) {
      cat("\n========== DIAGNOSTIC: PCLASS_ORPROC raw value distribution (first chunk) ==========\n")
      print(table(scalar_df$PCLASS_ORPROC, useNA = "always"))
      cat("This script treats PCLASS_ORPROC == 1 as 'major OR procedure present'\n")
      cat("(used as the same-admission-surgery provoking factor below). If the\n")
      cat("distribution suggests a different scheme (e.g. multi-category 0/1/2/3),\n")
      cat("STOP and tell Claude before trusting the provoked/unprovoked results.\n")
      cat("=====================================================================\n\n")
      diagnostic_printed <- TRUE
    }

    dx1 <- trimws(substr(lines, dx_starts[1], dx_ends[1]))
    is_vte_principal <- grepl(vte_regex, dx1)
    age_in_range <- scalar_df$AGE >= 18 & !is.na(scalar_df$AGE)

    scalar_df$cohort <- is_vte_principal & age_in_range
    cohort_mask <- scalar_df$cohort

    # Expensive checks -- only for this chunk's cohort rows
    scalar_df$has_cancer <- NA
    scalar_df$has_fracture <- NA
    scalar_df$has_pregnancy <- NA
    scalar_df$ivc_filter <- NA
    scalar_df$bleeding_event <- NA

    if (any(cohort_mask)) {
      sub_lines <- lines[cohort_mask]

      # cancer check needs ALL dx fields (not just dx1, which is VTE here) --
      # "has_cancer" = at least one non-skin-cancer malignancy code present
      nonskin_cancer_hit <- rep(FALSE, sum(cohort_mask))
      for (i in seq_along(dx_starts)) {
        codes <- trimws(substr(sub_lines, dx_starts[i], dx_ends[i]))
        nonskin_cancer_hit <- nonskin_cancer_hit | (grepl(cancer_regex, codes) & !grepl(skin_cancer_exclude_regex, codes))
      }
      scalar_df$has_cancer[cohort_mask] <- nonskin_cancer_hit

      scalar_df$has_fracture[cohort_mask]   <- any_match(sub_lines, dx_starts, dx_ends, fracture_regex)
      scalar_df$has_pregnancy[cohort_mask]  <- any_match(sub_lines, dx_starts, dx_ends, pregnancy_regex)
      scalar_df$ivc_filter[cohort_mask]     <- any_match(sub_lines, pr_starts, pr_ends, ivc_filter_regex)
      scalar_df$bleeding_event[cohort_mask] <- any_match(sub_lines, dx_starts, dx_ends, bleeding_regex)
    }

    all_chunks[[chunk_i]] <- scalar_df
    cat("Chunk", chunk_i, "--", n, "rows,", sum(cohort_mask), "cohort rows so far this chunk\n")
  }
  close(con)

  full_derived <- bind_rows(all_chunks)
  cat("\nTotal rows scanned:", nrow(full_derived), "\n")
  cat("Total VTE cohort rows:", sum(full_derived$cohort), "\n")

  rm(all_chunks); gc()

  dir.create(dirname(checkpoint_rds), showWarnings = FALSE, recursive = TRUE)
  saveRDS(full_derived, checkpoint_rds)
  cat("Saved checkpoint to", checkpoint_rds, "\n")
}


# ----- 4. DERIVE provoked/unprovoked, major surgery flag -----
full_derived <- full_derived %>%
  mutate(
    major_surgery = PCLASS_ORPROC == 1,
    provoked = has_cancer | major_surgery | has_fracture | has_pregnancy,
    vte_status = case_when(
      cohort & provoked ~ "Provoked",
      cohort & !provoked ~ "Unprovoked",
      TRUE ~ NA_character_
    ),
    vte_status = factor(vte_status, levels = c("Unprovoked", "Provoked"))
  )

cat("\nProvoking factor breakdown among cohort (not mutually exclusive):\n")
full_derived %>%
  filter(cohort) %>%
  summarise(
    n = n(),
    pct_cancer = mean(has_cancer, na.rm = TRUE) * 100,
    pct_surgery = mean(major_surgery, na.rm = TRUE) * 100,
    pct_fracture = mean(has_fracture, na.rm = TRUE) * 100,
    pct_pregnancy = mean(has_pregnancy, na.rm = TRUE) * 100
  ) %>%
  print()


# ----- 5. SURVEY DESIGN + SUBPOPULATION SUBSET -----
svy_design_vte <- svydesign(
  ids = ~HOSP_NIS, strata = ~NIS_STRATUM, weights = ~DISCWT,
  data = full_derived, nest = TRUE
)
svy_cohort_vte <- subset(svy_design_vte, cohort)

cat("\n========== COHORT SIZE ==========\n")
cat("Unweighted VTE cohort n:", nrow(svy_cohort_vte$variables), "\n")
cat("Weighted (national estimate) cohort n:", round(sum(weights(svy_cohort_vte))), "\n")

cat("\nProvoked vs. unprovoked distribution:\n")
print(svymean(~vte_status, svy_cohort_vte, na.rm = TRUE))


# ----- 6. OUTCOMES BY PROVOKED/UNPROVOKED STATUS -----
cat("\n========== Mortality by VTE status ==========\n")
print(svyby(~DIED, ~vte_status, svy_cohort_vte, svymean, na.rm = TRUE))
model_mortality <- svyglm(DIED ~ vte_status + AGE + FEMALE, design = svy_cohort_vte, family = quasibinomial())
print(round(exp(cbind(OR = coef(model_mortality), confint(model_mortality))), 3))
rm(model_mortality); gc()

cat("\n========== Length of stay by VTE status ==========\n")
print(svyby(~LOS, ~vte_status, svy_cohort_vte, svymean, na.rm = TRUE))
model_los <- svyglm(LOS ~ vte_status + AGE + FEMALE, design = svy_cohort_vte, family = gaussian())
print(round(cbind(Estimate = coef(model_los), confint(model_los)), 3))
rm(model_los); gc()

cat("\n========== IVC filter placement rate by VTE status ==========\n")
print(svyby(~ivc_filter, ~vte_status, svy_cohort_vte, svymean, na.rm = TRUE))
model_filter <- svyglm(ivc_filter ~ vte_status + AGE + FEMALE, design = svy_cohort_vte, family = quasibinomial())
print(round(exp(cbind(OR = coef(model_filter), confint(model_filter))), 3))
rm(model_filter); gc()

cat("\n========== Bleeding complication rate by VTE status ==========\n")
print(svyby(~bleeding_event, ~vte_status, svy_cohort_vte, svymean, na.rm = TRUE))
model_bleed <- svyglm(bleeding_event ~ vte_status + AGE + FEMALE, design = svy_cohort_vte, family = quasibinomial())
print(round(exp(cbind(OR = coef(model_bleed), confint(model_bleed))), 3))
rm(model_bleed); gc()

cat("\n========== ALL ANALYSES COMPLETE ==========\n")
