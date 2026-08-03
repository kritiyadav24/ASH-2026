# ASH 2026: Vaccination Disparities in Leukemia/Lymphoma Survivors

Analysis of racial, socioeconomic, and geographic disparities in pneumococcal
and influenza vaccination uptake among US adults with a history of leukemia
or lymphoma, using pooled 2019-2024 National Health Interview Survey (NHIS)
data via IPUMS.

## Contents

- `scripts/nhis_vaccination_disparities.R` — full analysis pipeline: data
  loading, cleaning/recoding, survey-weighted descriptive tables, and
  primary/secondary/sensitivity/subgroup regression models.

## Data

Requires an IPUMS NHIS extract (DDI `.xml` + `.dat.gz`) covering 2019-2024,
including the variables referenced in the script (`CNLEUKAG`, `CNLYMPAG`,
`SHOTPNUEV`, `VACFLU12M`, `RACENEW`, `HINOTCOV`, `POVERTY`, `EDUC`,
`USUALPL`, `REGION`, `URBRRL`, `SAMPWEIGHT`, `STRATA`, `PSU`, `LONGWEIGHT`).
Update the file path in the script's data-loading section to point to your
local extract.

## Requirements

R packages: `ipumsr`, `tidyverse`, `survey`, `gtsummary`, `broom`.
