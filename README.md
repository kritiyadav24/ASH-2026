# ASH 2026: Vaccination Disparities in Leukemia/Lymphoma Survivors

Analysis of racial, socioeconomic, and geographic disparities in pneumococcal
and influenza vaccination uptake among US adults with a history of leukemia
or lymphoma, using pooled 2019-2024 National Health Interview Survey (NHIS)
data via IPUMS.

## Key finding

**Lymphoma survivors have significantly lower vaccination uptake than
leukemia survivors**, even after adjusting for race, age, sex, insurance,
poverty, education, usual source of care, and region:
pneumococcal adjusted OR 0.62 (95% CI 0.41–0.92); flu adjusted OR 0.63
(95% CI 0.43–0.91). This is one of the best-powered comparisons in the
analysis (346 leukemia-only vs. 706 lymphoma-only survivors), noticeably
larger than any racial subgroup available here. See "Key findings" in
`results/pooled_2019_2024_results.txt` for the full ranked list,
including why insurance status remains the single largest driver
overall and why the racial-disparity comparisons are underpowered.

## Contents

- `scripts/nhis_vaccination_disparities.R` — full analysis pipeline: data
  loading, cleaning/recoding, survey-weighted descriptive tables, and
  primary/secondary/sensitivity/subgroup regression models, including a
  direct leukemia-vs-lymphoma comparison (Section 14).
- `results/pooled_2019_2024_results.txt` — output from an actual run of
  the script against a real IPUMS NHIS extract (2019-2024, N=1,056 heme
  survivors), led by a ranked "Key findings" summary. Raw microdata
  itself is not included in this repo per IPUMS redistribution terms —
  only the aggregated statistical output.

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

## NIS 2023 Core: Cancer + VTE coding (data loading, in progress)

A second, separate project exploring cancer-associated venous
thromboembolism (VTE) coding using the HCUP Nationwide Inpatient Sample
(NIS) 2023 Core file. Currently at the data-loading/exploration stage --
no research question or analysis pipeline has been built yet.

- `scripts/nis_2023_core_cancer_vte_explore.R` -- **the script that
  actually completed against the real 6.74M-row file.** Pure base R
  (no tidyverse/vroom), reads the fixed-width file in 200,000-line
  chunks with `readLines()`+`substr()`, so memory stays small and
  progress prints continuously. See `results/nis_2023_core_cancer_vte_explore_results.txt`
  for real output from running it.
- `scripts/nis_2023_core_load.R` and `nis_2023_core_standalone.R` --
  earlier tidyverse/vroom-based versions. Correct, but were too slow
  (`read_fwf()`) or too memory-heavy (`vroom` materializing all
  columns) to reliably finish on ordinary laptop hardware against the
  full file -- kept for reference on the fixed-width column layout
  and missing-value-sentinel handling, which the chunked version
  re-derives inline from the same HCUP-verified positions.
- `data_specs/SASload_NIS_2023_Core.SAS` -- HCUP's own load program for
  NIS 2023 Core, used as the source of truth for byte-level column
  positions (see below).

### Real run results

Against the actual NIS 2023 Core extract (6,743,716 discharge records):

- 587,844 records (8.72%) had any cancer diagnosis (ICD-10-CM "C" code)
- 203,314 records (3.01%) had any VTE diagnosis (I26/I80/I82)
- 43,665 records had both -- i.e. **7.43% of cancer discharges also had
  a VTE diagnosis**, in line with published cancer-associated VTE
  prevalence ranges (a sanity check on the coding logic, not a
  validated cohort)
- 21,711 records had an IVC filter placement procedure

These are unweighted discharge counts, not weighted national estimates
-- `DISCWT` was not loaded/applied. See the results file for full
caveats (any-listed-diagnosis vs. incident VTE, no pharmacy data, etc.).

### Primary analysis: insurance-based disparities in IVC filter placement

`scripts/nis_2023_ivc_filter_insurance_disparities.R` -- the actual
research-question pipeline, built on the chunked streaming approach
above. Research question: among hospitalized cancer patients with VTE,
are there insurance-based disparities in IVC filter placement (Medicaid/
uninsured/other-payer vs. privately insured), and do they persist after
adjusting for clinical severity, comorbidities, and cancer type?
Secondary: does the disparity vary by cancer type; do outcomes differ by
insurance x filter status; are filters concentrated appropriately in
bleeding-contraindicated patients across insurance groups?

Race was dropped from the original research question after checking:
`NIS_2023_Core`, `NIS_2023_Hospital`, and `NIS_2023_Severity` file
structures were inspected and none contain a race/ethnicity variable in
this extract.

Key design points (see the script's header comments for full detail):
- **Survey design correctness**: builds `svydesign()` from the *full*
  6.74M-row sample (weight/strata/cluster retained for every row) and
  uses `subset()` to reach the cancer+VTE cohort, rather than building
  the design from the cohort alone -- the latter understates standard
  errors for subpopulation estimates.
- **Performance**: only the cheap cohort-defining checks (cancer/VTE/
  prior-VTE-history) run on all 6.74M rows per chunk; the expensive
  checks (cancer type, comorbidities, IVC filter, bleeding
  contraindication) only run on the ~0.6% of rows that are actually in
  the cohort. Saves a checkpoint (`.rds`) after the file scan so
  re-running the script to fix a downstream modeling issue doesn't
  require re-scanning the whole file.
- **Bleeding contraindication is a covariate, not an exclusion** --
  an earlier draft excluded these patients from the cohort entirely,
  which would have made the appropriateness-of-use secondary question
  unanswerable.
- **Prior VTE history (Z86.71) is an exclusion**, even when an active
  VTE code is also present on the same record (i.e. a genuine recurrent
  VTE with noted history is still excluded) -- a known limitation of
  claims-based VTE cohort algorithms, not a bug.
- **Comorbidities** are a targeted 8-category panel (metastatic disease,
  heart failure, CKD, liver disease, coagulopathy, obesity, diabetes,
  COPD), not the full AHRQ Elixhauser Comorbidity Software algorithm.
- **Severity** uses Core-available proxies (diagnosis count, elective/
  emergent admission, ED entry, transfer-in status), not true APR-DRG
  severity -- upgradable later if `NIS_2023_Severity`'s own SAS load
  program is obtained from HCUP-US and merged in via `KEY_NIS`.

### Known issue caught before running against real data

An earlier draft of the loading script used `data.table::fread()`,
assuming `NIS_2023_Core.ASC` was delimited text. It is not: HCUP NIS
Core files are fixed-width ASCII with no delimiter between fields --
every discharge record is one 643-byte line, and each variable occupies
a specific byte range documented only in HCUP's SAS/SPSS/Stata load
program. `fread()` on this file wouldn't error; it would just silently
misalign every column past the first few.

The corrected script parses the exact byte positions straight out of
`data_specs/SASload_NIS_2023_Core.SAS` (rather than hand-transcribing
~125 start/end positions, which is exactly the kind of manual-
transcription error this repo has caught before) and validates the
parse -- widths sum to the declared `LRECL`, no gaps/overlaps between
columns, no duplicate variable names -- before trusting it to read the
real file. It also correctly recodes HCUP's numeric missing-value
sentinels (e.g. `-99`/`-88`/`-66` for a 3-digit field) to `NA`, and
fails loudly if it meets an informat it doesn't have a sentinel mapping
for, rather than silently leaving garbage negative values in place.

### Scope note: anticoagulation is not observable in NIS

NIS is hospital discharge/administrative claims data -- it has no
pharmacy or medication-administration file. Anticoagulant receipt
cannot be directly observed. The closest available proxy is an IVC
filter placement procedure code (ICD-10-PCS `06H0-`/`06H3-`/`06H4-`),
which signals a decision not to (or inability to) anticoagulate, not
evidence of anticoagulation itself. Any research question built on
this data needs to be framed around what's actually codeable here
(diagnosis and procedure codes), not medication receipt.

### Requirements

R packages: `tidyverse`.

### Data

Requires an HCUP NIS 2023 Core ASCII extract (`NIS_2023_Core.ASC`,
purchased via the HCUP Central Distributor under a Data Use Agreement)
plus its accompanying load program/file specifications (included here
under `data_specs/`, and also public at
https://hcup-us.ahrq.gov/db/nation/sasloadprog.jsp regardless of
whether you have SAS installed). Update the `nis_file` path in the
script to point to your local extract. Raw microdata itself is not
included in this repo per HCUP's Data Use Agreement.
