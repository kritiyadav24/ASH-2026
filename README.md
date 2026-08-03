# ASH 2026: Vaccination Disparities in Leukemia/Lymphoma Survivors

Analysis of racial, socioeconomic, and geographic disparities in pneumococcal
and influenza vaccination uptake among US adults with a history of leukemia
or lymphoma, using pooled 2019-2024 National Health Interview Survey (NHIS)
data via IPUMS.

## Contents

- `scripts/nhis_vaccination_disparities.R` — full analysis pipeline: data
  loading, cleaning/recoding, survey-weighted descriptive tables, and
  primary/secondary/sensitivity/subgroup regression models.
- `results/pooled_2019_2024_results.txt` — output from an actual run of
  the script against a real IPUMS NHIS extract (2019-2024, N=1,056 heme
  survivors). Raw microdata itself is not included in this repo per
  IPUMS redistribution terms — only the aggregated statistical output.

## Data

Requires an IPUMS NHIS extract (DDI `.xml` + `.dat.gz`) covering 2019-2024,
including the variables referenced in the script (`CNLEUKAG`, `CNLYMPAG`,
`SHOTPNUEV`, `VACFLU12M`, `RACENEW`, `HINOTCOV`, `POVERTY`, `EDUC`,
`USUALPL`, `REGION`, `URBRRL`, `SAMPWEIGHT`, `STRATA`, `PSU`, `SALNGPRTFLG`).
Update the file path in the script's data-loading section to point to your
local extract.

## Requirements

R packages: `ipumsr`, `tidyverse`, `survey`, `gtsummary`, `broom`.

## Known issues fixed (verified against the IPUMS NHIS codebook and a real extract)

An earlier version of this script had several variables coded backwards
or mislabeled. These have been corrected in the current script:

- **`SHOTPNUEV`** (primary outcome, pneumococcal vaccination): codebook is
  1=No, 2=Yes — the script previously had this reversed.
- **`VACFLU12M`** (secondary outcome, flu vaccination): codebook is 1=No,
  2=Yes — also previously reversed.
- **`HINOTCOV`** (insurance): this variable is phrased "has NO coverage",
  so 1=No/has coverage (insured), 2=Yes/no coverage (uninsured) — the
  `insured` recode had this backwards.
- **`USUALPL`** (usual source of care): 1=No place, 2=Yes one place,
  3=Yes more than one place — the script previously grouped 1 & 2 as
  "yes" and 3 as "no", the reverse of the actual codes.
- **`LONGWEIGHT`** does not exist as an IPUMS NHIS variable. The correct
  variable for identifying 2020 longitudinal sample members is
  **`SALNGPRTFLG`**, which must be added to your IPUMS extract. Without
  it, 2020 longitudinal members cannot be dropped and pooled 2020
  estimates may double-count some respondents.
- **`URBRRL`** (rurality): don't assume it's missing for 2023-2024 —
  check `table(df$URBRRL, df$YEAR)` for your own extract; it was present
  for all of 2019-2024 in the extract used to generate the results here.
- `RACENEW` "Other/Multiracial" and `EDUC` category buckets were widened
  to include a couple of legacy/aggregate codes that were previously
  omitted (didn't change results for this extract, but avoids silently
  dropping records in extracts that do include those codes).

Because these were sign/direction errors on binary predictors and the
primary/secondary outcomes, results from the original script would have
been substantively wrong (e.g., showing insurance as *protective against*
vaccination would have appeared reversed). See `results/pooled_2019_2024_results.txt`
for corrected output.

## Caveats on the corrected results

- Racial subgroups other than White and Black are very small (Asian
  n=18, AIAN n=6, Other/Multiracial n=15 unweighted), producing wide,
  sometimes degenerate confidence intervals (e.g., one leukemia-only
  subgroup estimate has an OR in the hundreds of thousands from
  near-complete separation). Treat AIAN/Other/Multiracial estimates,
  especially in subgroup models, as too underpowered to interpret.
- 2020 longitudinal sample members are not excluded in this run because
  `SALNGPRTFLG` wasn't in the source extract (see above).
