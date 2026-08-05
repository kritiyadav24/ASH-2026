# ASH 2026 Abstract Draft

**Title:** Insurance-Based Disparities in In-Hospital Mortality Among Cancer Patients with Venous Thromboembolism: A National Analysis

**Background:** Cancer-associated VTE carries substantial morbidity and mortality. Insurance status is a known driver of healthcare disparities, but its association with outcomes in this population, independent of treatment patterns, is unclear.

**Methods:** Using the 2023 National Inpatient Sample, we identified hospitalizations for adults with a cancer diagnosis and acute VTE (pulmonary embolism or lower-extremity DVT). IVC filter placement was identified by ICD-10-PCS code. Survey-weighted logistic regression estimated the association between insurance status (Private as reference) and in-hospital mortality, adjusting for age, sex, cancer type, and key comorbidities. Secondary analyses examined bleeding-contraindication rates by insurance, the filter-mortality association within each insurance group, and whether the insurance-mortality association varied by cancer type.

**Results:** Among 129,265 weighted hospitalizations (25,853 unweighted), IVC filter placement did not differ significantly by insurance for Medicaid or Uninsured/Self-pay patients; a small, marginally significant reduction was observed for the "Other" payer category (OR 0.73, 95% CI 0.54–0.996). In-hospital mortality was significantly higher for uninsured/self-pay (OR 1.67, 95% CI 1.26–2.20) and other-payer (OR 1.58, 95% CI 1.26–1.99) patients versus privately insured patients. Bleeding-contraindication rates were similar across insurance groups (13.5–16.9%), and filter placement was appropriately concentrated in contraindicated patients regardless of payer (OR 4.25, 95% CI 3.50–5.16) — arguing against differential undertreatment as the driver. The filter-mortality association did not reach significance within any single insurance stratum. The insurance-mortality association varied significantly by cancer type (p=0.004), most robustly among patients with other/less common cancer types (11.1% vs. 18.6% mortality, Private vs. Uninsured/Self-pay).

**Conclusion:** Among hospitalized cancer patients with VTE, uninsured and other-payer patients experience significantly higher in-hospital mortality than privately insured patients, independent of IVC filter use or filter appropriateness. This disparity varies by cancer type and likely reflects factors beyond procedural treatment — delayed presentation, disease severity at diagnosis, or post-discharge access to care — warranting further investigation.

---

## Fact-check log

Every claim above was individually verified against real script output (not synthetic/placeholder data) before finalizing:

| Claim | Source | Verified value |
|---|---|---|
| Cohort size | `COHORT SIZE` output | 129,265 weighted / 25,853 unweighted |
| Filter placement by insurance (adjusted) | Primary analysis adjusted model | Medicare 0.945 (0.830–1.075), Medicaid 0.917 (0.770–1.093), Uninsured/Self-pay 1.114 (0.811–1.530), Other 0.734 (0.541–0.996) |
| Mortality by insurance (adjusted) | Secondary Q2 model (`model_mortality`) | Uninsured/Self-pay 1.666 (1.264–2.195), Other 1.580 (1.255–1.989) |
| Bleeding contraindication rate by insurance | Secondary 1 script output | Range 13.5% (Uninsured/Self-pay) – 16.9% (Other) |
| Filter placement appropriateness | Secondary Q3 model | `bleeding_contraTRUE` OR 4.246 (3.497–5.156) |
| Filter-mortality effect within insurance strata | Secondary 2 script output | All 5 groups NS (CIs cross 1): Private 1.05, Medicare 0.91, Medicaid 0.91, Uninsured/Self-pay 0.50, Other 0.47 |
| Insurance x cancer type interaction on mortality | Secondary 3 global test (`regTermTest`) | F=1.938 on 24, 2290 df, p=0.0041068 |
| "Other" cancer type mortality gap | Secondary 3 descriptive table | Private 11.1% (n=2787), Uninsured/Self-pay 18.6% (n=199, 37 events) -- chosen as the best-powered specific comparison |

One error was caught and corrected during this audit: an earlier draft stated filter placement "did not differ significantly by insurance" without qualification, which omitted the marginally significant "Other" payer reduction (OR 0.73, 95% CI 0.54-0.996) visible in the adjusted primary model. The Results section above now states this accurately.

### Known limitations (not in the abstract, for internal reference)
- Race is not available in this NIS 2023 extract (checked Core, Hospital, and Severity file structures -- none contain it).
- Comorbidities are a targeted 8-category panel, not the full AHRQ Elixhauser algorithm.
- Clinical severity uses Core-available proxies (diagnosis count, elective/emergent admission, ED entry, transfer-in status), not true APR-DRG severity.
- Breast and Hematologic cancer-type-specific mortality comparisons for Uninsured/Self-pay and Other insurance are too underpowered (6 and 2 deaths respectively) to cite specifically, despite large point estimates in the raw descriptive table.
- Pancreatic cancer's mortality gap (14.1% vs. 33.3%, Private vs. Uninsured/Self-pay) is suggestive but based on only 10 deaths in the uninsured group -- described as "suggestive" rather than confirmed, and intentionally not included in the abstract's headline claims.
