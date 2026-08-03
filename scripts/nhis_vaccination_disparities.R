# ============================================================
# NHIS 2019-2024: Vaccination Disparities in Leukemia/Lymphoma Survivors
# Research Question: Among US adults with leukemia/lymphoma history,
# are there racial, socioeconomic, and geographic disparities in
# pneumococcal and influenza vaccination uptake?
#
# Primary outcome:   Pneumococcal vaccination ever (SHOTPNUEV)
# Secondary outcome: Influenza vaccination past 12 months (VACFLU12M)
# Primary exposure:  Race/ethnicity (RACENEW)
# Confounders:       Age, sex, insurance, poverty, education,
#                    usual source of care, region
# Sensitivity:       Rurality (URBRRL -- check the table(df$URBRRL, df$YEAR)
#                    output in Section 3 for your extract; it has been
#                    available for all of 2019-2024 in some extracts,
#                    despite older guidance suggesting 2019-2022 only)
#
# NOTES:
# - NHIS redesigned in 2019; restrict to 2019-2024 for comparability
# - Pool weights by dividing SAMPWEIGHT by 6 (number of years)
# - Drop 2020 longitudinal sample members to avoid double-counting
# - Use STRATA and PSU from IPUMS (already harmonized across years)
# ============================================================


# ----- 1. LOAD PACKAGES -----
# install.packages(c("ipumsr","tidyverse","survey","gtsummary","broom"))
library(ipumsr)
library(tidyverse)
library(survey)
library(gtsummary)
library(broom)


# ----- 2. LOAD DATA -----
# Ensure both .xml and .dat.gz are in the same folder
# Update filename to match your extract number
ddi <- read_ipums_ddi("~/Downloads/nhis_00004.xml")
df  <- read_ipums_micro(ddi)

# Verify years and variables
cat("Years in dataset:\n")
print(table(df$YEAR))
cat("\nVariables:\n")
print(names(df))


# ----- 3. VERIFY RAW CODES BEFORE CLEANING -----
# Run this section first to confirm all codes match expectations
cat("\n--- CNLEUKAG (last 10 codes) ---\n")
print(tail(table(df$CNLEUKAG), 10))

cat("\n--- CNLYMPAG (last 10 codes) ---\n")
print(tail(table(df$CNLYMPAG), 10))

cat("\n--- SHOTPNUEV ---\n"); print(table(df$SHOTPNUEV))
cat("\n--- VACFLU12M ---\n"); print(table(df$VACFLU12M))
cat("\n--- RACENEW ---\n");   print(table(df$RACENEW))
cat("\n--- HINOTCOV ---\n");  print(table(df$HINOTCOV))
cat("\n--- POVERTY ---\n");   print(table(df$POVERTY))
cat("\n--- EDUC ---\n");      print(table(df$EDUC))
cat("\n--- USUALPL ---\n");   print(table(df$USUALPL))
cat("\n--- REGION ---\n");    print(table(df$REGION))
cat("\n--- URBRRL ---\n");    print(table(df$URBRRL))
cat("\n--- SALNGPRTFLG (2020 longitudinal/partial sample flag) ---\n")
print(table(df$YEAR == 2020 & df$SALNGPRTFLG == 1))


# ----- 4. RESTRICT AND PREPARE POOLED DATASET -----

# Step 1: Restrict to 2019-2024 (post-redesign only)
df_pool <- df %>% filter(YEAR >= 2019)

# Step 2: Drop 2020 longitudinal sample members to avoid double-counting
# These are adults who completed both 2019 and 2020 interviews.
# NOTE: the flag for this is SALNGPRTFLG (not LONGWEIGHT, which is not
# an IPUMS NHIS variable). SALNGPRTFLG distinguishes 2020 sample adults
# in the longitudinal sample from those in the partial sample -- check
# its value labels in your codebook once added to the extract, and
# update the filter condition below to match the "longitudinal" code.
# This extract did not include SALNGPRTFLG, so this filter is a no-op
# placeholder; add SALNGPRTFLG to your IPUMS extract before running.
df_pool <- df_pool %>%
  filter(!(YEAR == 2020 & SALNGPRTFLG == 1))

# Step 3: Create pooled weight (divide by number of years = 6)
# This gives average annual population estimates rather than cumulative
df_pool <- df_pool %>%
  mutate(POOLWT = SAMPWEIGHT / 6)

cat("\nYears after restriction:\n")
print(table(df_pool$YEAR))
cat("\nRows dropped (2020 longitudinal members):",
    nrow(df %>% filter(YEAR >= 2019)) - nrow(df_pool), "\n")


# ----- 5. CLEAN AND RECODE -----
df_clean <- df_pool %>%

  # Restrict to sample adults 18+
  filter(AGE >= 18, ASTATFLG == 1) %>%

  # Convert AGE to numeric (removes haven_labelled class)
  mutate(AGE = as.numeric(AGE)) %>%

  # --- Heme survivor flag ---
  # CNLEUKAG/CNLYMPAG: 00-85 = valid age at dx (00 = diagnosed under
  # age 1); 96=NIU; 97-99=unknown. Use >= 0, not > 0, so diagnoses in
  # infancy aren't dropped.
  mutate(
    leuk_surv  = if_else(CNLEUKAG >= 0 & CNLEUKAG <= 85, 1, 0),
    lymph_surv = if_else(CNLYMPAG >= 0 & CNLYMPAG <= 85, 1, 0),
    heme_surv  = if_else(leuk_surv == 1 | lymph_surv == 1, 1, 0),
    heme_type  = case_when(
      leuk_surv == 1 & lymph_surv == 0 ~ "Leukemia only",
      leuk_surv == 0 & lymph_surv == 1 ~ "Lymphoma only",
      leuk_surv == 1 & lymph_surv == 1 ~ "Both",
      TRUE ~ "Neither"
    )
  ) %>%

  # --- Time since diagnosis (years) ---
  mutate(
    dx_age = case_when(
      leuk_surv == 1 ~ as.numeric(CNLEUKAG),
      lymph_surv == 1 ~ as.numeric(CNLYMPAG),
      TRUE ~ NA_real_
    ),
    years_since_dx = AGE - dx_age
  ) %>%

  # --- Primary outcome: Pneumococcal vaccination ---
  # SHOTPNUEV: 1=No, 2=Yes, 7=Refused, 8=Not ascertained, 9=Don't know
  # (verified against IPUMS NHIS codebook; note the value order is
  # No-then-Yes, opposite of what you might assume)
  mutate(
    pneumo_vax = case_when(
      SHOTPNUEV == 2 ~ 1,
      SHOTPNUEV == 1 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%

  # --- Secondary outcome: Influenza vaccination ---
  # VACFLU12M: 1=No, 2=Yes, 7=Refused, 8=Not ascertained, 9=Don't know
  mutate(
    flu_vax = case_when(
      VACFLU12M == 2 ~ 1,
      VACFLU12M == 1 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%

  # --- Race/ethnicity ---
  # RACENEW: 100=White, 200=Black, 300=AIAN, 400=Asian
  # 510/530/541/542=Other/Multiracial; 997-999=Unknown
  mutate(
    race_eth = case_when(
      RACENEW == 100 ~ "White",
      RACENEW == 200 ~ "Black",
      RACENEW == 300 ~ "AIAN",
      RACENEW == 400 ~ "Asian",
      RACENEW %in% c(500, 510, 520, 530, 540, 541, 542) ~ "Other/Multiracial",
      TRUE ~ NA_character_
    ),
    race_eth = factor(race_eth,
                      levels = c("White", "Black", "Asian",
                                 "AIAN", "Other/Multiracial"))
  ) %>%

  # --- Sex ---
  # SEX: 1=Male, 2=Female
  mutate(
    sex = case_when(
      SEX == 1 ~ "Male",
      SEX == 2 ~ "Female",
      TRUE ~ NA_character_
    ),
    sex = factor(sex, levels = c("Male", "Female"))
  ) %>%

  # --- Insurance ---
  # HINOTCOV: 1=No/has coverage (insured), 2=Yes/no coverage (uninsured),
  # 9=Unknown. HINOTCOV is phrased as "has NO coverage", so its Yes/No
  # values are the reverse of "insured" -- verified against codebook.
  mutate(
    insured = case_when(
      HINOTCOV == 1 ~ 1,
      HINOTCOV == 2 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%

  # --- Poverty ratio ---
  # Detailed NHIS subcategory codes:
  # 11-14=<100% FPL; 21-25=100-199%; 31-35=200-399%; 36-38=400%+
  # 98-99=Unknown
  mutate(
    poverty_cat = case_when(
      POVERTY %in% c(11, 12, 13, 14) ~ "<100% FPL",
      POVERTY %in% c(21, 22, 23, 24, 25) ~ "100-199% FPL",
      POVERTY %in% c(31, 32, 33, 34, 35) ~ "200-399% FPL",
      POVERTY %in% c(36, 37, 38) ~ "400%+ FPL",
      TRUE ~ NA_character_
    ),
    poverty_cat = factor(poverty_cat,
                         levels = c("400%+ FPL", "200-399% FPL",
                                    "100-199% FPL", "<100% FPL"))
  ) %>%

  # --- Education ---
  # Codes confirmed from IPUMS codebook (2019-2024). Includes both the
  # aggregate codes (100/200/300/500) and detailed sub-codes, since a
  # given record uses one or the other depending on year/source:
  # 100-116=Less than HS; 200-202=HS/GED; 300-303=Some college
  # 400=Bachelor's; 500/510/520/521/522=Graduate/Professional/Doctoral
  # 997-999=Unknown
  mutate(
    educ_cat = case_when(
      EDUC %in% c(100, 101, 102, 103, 104, 105, 106, 107, 108,
                  109, 110, 111, 112, 113, 114, 115, 116) ~ "Less than HS",
      EDUC %in% c(200, 201, 202) ~ "HS/GED",
      EDUC %in% c(300, 301, 302, 303) ~ "Some college",
      EDUC == 400 ~ "Bachelor's degree",
      EDUC %in% c(500, 510, 520, 521, 522) ~ "Graduate degree",
      TRUE ~ NA_character_
    ),
    educ_cat = factor(educ_cat,
                      levels = c("Graduate degree", "Bachelor's degree",
                                 "Some college", "HS/GED", "Less than HS"))
  ) %>%

  # --- Usual source of care ---
  # USUALPL: 0=NIU, 1=No place, 2=Yes one place, 3=Yes more than one
  # place, 7-9=Unknown (verified against codebook; original assumed
  # 1/2 were both "Yes" and 3 was "No" -- it's the opposite)
  mutate(
    usual_care = case_when(
      USUALPL %in% c(2, 3) ~ 1,
      USUALPL == 1 ~ 0,
      TRUE ~ NA_real_
    )
  ) %>%

  # --- Geographic region ---
  # REGION: 1=Northeast, 2=Midwest, 3=South, 4=West; 8/9=Missing
  mutate(
    region = case_when(
      REGION == 1 ~ "Northeast",
      REGION == 2 ~ "Midwest",
      REGION == 3 ~ "South",
      REGION == 4 ~ "West",
      TRUE ~ NA_character_
    ),
    region = factor(region,
                    levels = c("Northeast", "Midwest", "South", "West"))
  ) %>%

  # --- Rurality ---
  # URBRRL: 1=Large central metro, 2=Large fringe metro,
  #         3=Medium/small metro, 4=Non-metropolitan
  # Verify year coverage for your extract via Section 3's
  # table(df$URBRRL, df$YEAR) output before assuming any years are
  # missing -- some extracts have it for all of 2019-2024.
  mutate(
    rural = case_when(
      URBRRL %in% c(1, 2) ~ 0,
      URBRRL %in% c(3, 4) ~ 1,
      TRUE ~ NA_real_
    ),
    rural_label = case_when(
      rural == 0 ~ "Urban",
      rural == 1 ~ "Rural",
      TRUE ~ NA_character_
    ),
    rural_label = factor(rural_label, levels = c("Urban", "Rural"))
  )


# ----- 6. FEASIBILITY CHECK -----
cat("\n========== FEASIBILITY CHECK ==========\n")
df_heme <- df_clean %>% filter(heme_surv == 1)

cat("Leukemia survivors:", sum(df_clean$leuk_surv, na.rm = TRUE), "\n")
cat("Lymphoma survivors:", sum(df_clean$lymph_surv, na.rm = TRUE), "\n")
cat("Total heme survivors:", nrow(df_heme), "\n")
cat("Pneumo vax Yes:", sum(df_heme$pneumo_vax == 1, na.rm = TRUE), "\n")
cat("Pneumo vax No:", sum(df_heme$pneumo_vax == 0, na.rm = TRUE), "\n")
cat("Flu vax Yes:", sum(df_heme$flu_vax == 1, na.rm = TRUE), "\n")
cat("Flu vax No:", sum(df_heme$flu_vax == 0, na.rm = TRUE), "\n")

cat("\nRace distribution:\n")
print(table(df_heme$race_eth, useNA = "always"))

cat("\nMissing data summary:\n")
cat("pneumo_vax missing:", sum(is.na(df_heme$pneumo_vax)), "\n")
cat("flu_vax missing:", sum(is.na(df_heme$flu_vax)), "\n")
cat("race_eth missing:", sum(is.na(df_heme$race_eth)), "\n")
cat("poverty_cat missing:", sum(is.na(df_heme$poverty_cat)), "\n")
cat("educ_cat missing:", sum(is.na(df_heme$educ_cat)), "\n")
cat("insured missing:", sum(is.na(df_heme$insured)), "\n")
cat("usual_care missing:", sum(is.na(df_heme$usual_care)), "\n")


# ----- 7. SURVEY DESIGN -----
# Using POOLWT (SAMPWEIGHT / 6) for pooled 2019-2024 estimates
# STRATA and PSU are IPUMS-harmonized across years
svy_design <- svydesign(
  id      = ~PSU,
  strata  = ~STRATA,
  weights = ~POOLWT,
  data    = df_clean,
  nest    = TRUE
)

# Subset to heme survivors
svy_heme <- subset(svy_design, heme_surv == 1)

# Subset for rurality sensitivity. Restrict to years where URBRRL is
# non-missing for your extract -- check Section 3's URBRRL-by-YEAR
# table first; don't assume 2019-2022 only.
svy_rural <- subset(svy_design, heme_surv == 1 & !is.na(rural))

# Subgroups
svy_leuk  <- subset(svy_design, leuk_surv == 1)
svy_lymph <- subset(svy_design, lymph_surv == 1)


# ----- 8. TABLE 1 — WEIGHTED DESCRIPTIVES BY RACE -----
tbl1 <- df_clean %>%
  filter(heme_surv == 1) %>%
  mutate(AGE = as.numeric(AGE)) %>%
  select(race_eth, AGE, sex, poverty_cat, educ_cat, insured,
         usual_care, region, rural_label, pneumo_vax, flu_vax,
         heme_type, years_since_dx) %>%
  tbl_summary(
    by = race_eth,
    missing = "no",
    label = list(
      AGE            ~ "Age, years",
      sex            ~ "Sex",
      poverty_cat    ~ "Poverty level",
      educ_cat       ~ "Education",
      insured        ~ "Has health insurance",
      usual_care     ~ "Has usual source of care",
      region         ~ "Census region",
      rural_label    ~ "Rurality (years with non-missing URBRRL)",
      pneumo_vax     ~ "Pneumococcal vaccination, ever",
      flu_vax        ~ "Influenza vaccination, past 12 months",
      heme_type      ~ "Cancer type",
      years_since_dx ~ "Years since diagnosis"
    ),
    statistic = list(
      all_continuous()  ~ "{median} ({p25}, {p75})",
      all_categorical() ~ "{n} ({p}%)"
    )
  ) %>%
  add_p(
    test = list(
      all_continuous()  ~ "kruskal.test",
      all_categorical() ~ "fisher.test"
    ),
    test.args = all_tests("fisher.test") ~
      list(simulate.p.value = TRUE)
  ) %>%
  add_overall() %>%
  bold_labels()

print(tbl1)


# ----- 9. WEIGHTED VACCINATION PREVALENCE -----
cat("\n========== WEIGHTED VACCINATION PREVALENCE ==========\n")

cat("\nOverall pneumococcal vaccination prevalence:\n")
print(svymean(~pneumo_vax, svy_heme, na.rm = TRUE))

cat("\nOverall flu vaccination prevalence:\n")
print(svymean(~flu_vax, svy_heme, na.rm = TRUE))

cat("\nPneumococcal by race:\n")
print(svyby(~pneumo_vax, ~race_eth, svy_heme, svymean, na.rm = TRUE))

cat("\nFlu by race:\n")
print(svyby(~flu_vax, ~race_eth, svy_heme, svymean, na.rm = TRUE))

cat("\nPneumococcal by insurance:\n")
print(svyby(~pneumo_vax, ~insured, svy_heme, svymean, na.rm = TRUE))

cat("\nPneumococcal by region:\n")
print(svyby(~pneumo_vax, ~region, svy_heme, svymean, na.rm = TRUE))


# ----- 10. PRIMARY ANALYSIS — PNEUMOCOCCAL VACCINE -----
cat("\n========== PRIMARY ANALYSIS: PNEUMOCOCCAL VACCINE ==========\n")

# Unadjusted
model_pneumo_unadj <- svyglm(
  pneumo_vax ~ race_eth,
  design = svy_heme,
  family = quasibinomial()
)
cat("\nUnadjusted ORs:\n")
print(round(exp(cbind(OR = coef(model_pneumo_unadj),
                      confint(model_pneumo_unadj))), 3))

# Fully adjusted
model_pneumo_adj <- svyglm(
  pneumo_vax ~ race_eth + AGE + sex + insured + poverty_cat +
               educ_cat + usual_care + region,
  design = svy_heme,
  family = quasibinomial()
)
cat("\nAdjusted ORs:\n")
print(round(exp(cbind(OR = coef(model_pneumo_adj),
                      confint(model_pneumo_adj))), 3))


# ----- 11. SECONDARY ANALYSIS — INFLUENZA VACCINE -----
cat("\n========== SECONDARY ANALYSIS: INFLUENZA VACCINE ==========\n")

# Unadjusted
model_flu_unadj <- svyglm(
  flu_vax ~ race_eth,
  design = svy_heme,
  family = quasibinomial()
)
cat("\nUnadjusted ORs:\n")
print(round(exp(cbind(OR = coef(model_flu_unadj),
                      confint(model_flu_unadj))), 3))

# Fully adjusted
model_flu_adj <- svyglm(
  flu_vax ~ race_eth + AGE + sex + insured + poverty_cat +
            educ_cat + usual_care + region,
  design = svy_heme,
  family = quasibinomial()
)
cat("\nAdjusted ORs:\n")
print(round(exp(cbind(OR = coef(model_flu_adj),
                      confint(model_flu_adj))), 3))


# ----- 12. SENSITIVITY ANALYSIS — RURALITY -----
cat("\n========== SENSITIVITY: RURALITY ==========\n")

cat("N for rurality analysis:",
    nrow(subset(df_clean, heme_surv == 1 & !is.na(rural))), "\n")

model_pneumo_rural <- svyglm(
  pneumo_vax ~ race_eth + AGE + sex + insured + poverty_cat +
               educ_cat + usual_care + region + rural,
  design = svy_rural,
  family = quasibinomial()
)
cat("\nAdjusted ORs — Pneumococcal with rurality:\n")
print(round(exp(cbind(OR = coef(model_pneumo_rural),
                      confint(model_pneumo_rural))), 3))

model_flu_rural <- svyglm(
  flu_vax ~ race_eth + AGE + sex + insured + poverty_cat +
            educ_cat + usual_care + region + rural,
  design = svy_rural,
  family = quasibinomial()
)
cat("\nAdjusted ORs — Flu with rurality:\n")
print(round(exp(cbind(OR = coef(model_flu_rural),
                      confint(model_flu_rural))), 3))


# ----- 13. SUBGROUP ANALYSES -----
cat("\n========== SUBGROUP ANALYSES ==========\n")

# Leukemia only
cat("\nLeukemia only (n =", sum(df_clean$leuk_surv, na.rm=TRUE), "):\n")
model_leuk <- svyglm(
  pneumo_vax ~ race_eth + AGE + sex + insured + poverty_cat +
               educ_cat + usual_care + region,
  design = svy_leuk,
  family = quasibinomial()
)
print(round(exp(cbind(OR = coef(model_leuk),
                      confint(model_leuk))), 3))

# Lymphoma only
cat("\nLymphoma only (n =", sum(df_clean$lymph_surv, na.rm=TRUE), "):\n")
model_lymph <- svyglm(
  pneumo_vax ~ race_eth + AGE + sex + insured + poverty_cat +
               educ_cat + usual_care + region,
  design = svy_lymph,
  family = quasibinomial()
)
print(round(exp(cbind(OR = coef(model_lymph),
                      confint(model_lymph))), 3))


# ----- 14. TREND ANALYSIS BY YEAR -----
cat("\n========== VACCINATION TRENDS BY YEAR ==========\n")

pneumo_trend <- svyby(~pneumo_vax, ~YEAR, svy_heme,
                      svymean, na.rm = TRUE)
flu_trend    <- svyby(~flu_vax,    ~YEAR, svy_heme,
                      svymean, na.rm = TRUE)

cat("\nPneumococcal vaccination by year:\n")
print(round(pneumo_trend, 3))

cat("\nFlu vaccination by year:\n")
print(round(flu_trend, 3))
