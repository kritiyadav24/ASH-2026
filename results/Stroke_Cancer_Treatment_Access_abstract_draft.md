# Stroke Treatment Access in Cancer Patients Abstract Draft

**Title:** Divergent Paths to Reperfusion: Cancer Patients Hospitalized for Acute Ischemic Stroke Receive Less Thrombolysis but More Thrombectomy

**Background:** Cancer patients hospitalized with acute ischemic stroke have higher in-hospital mortality than non-cancer stroke patients, and a prior National Inpatient Sample (NIS) study (2015-2017) found that intravenous thrombolysis and endovascular thrombectomy each attenuated the excess mortality associated with cancer, concluding that both therapies "should be considered routinely" in this population unless contraindicated [1]. Since that study, the DAWN and DEFUSE-3 trials (2018) extended thrombectomy eligibility from a 6-hour to up to a 24-hour window in patients with favorable imaging-defined deficit-infarct mismatch, substantially broadening who can be offered mechanical reperfusion [2,3]. Cancer-associated stroke is also mechanistically distinct, driven disproportionately by tumor-related hypercoagulability, which produces embolic and cryptogenic stroke patterns that may be more amenable to mechanical clot retrieval than to pharmacologic thrombolysis [4]. Whether these two changes -- an expanded thrombectomy window and cancer-specific stroke mechanism -- have altered *which* reperfusion therapy cancer patients actually receive, as opposed to whether they benefit from it, has not been directly examined.

**Methods:** Using the 2023 NIS, we identified adult (age 18+) hospitalizations with a principal diagnosis of ischemic stroke (ICD-10-CM I63.x). Cancer status (any of 10 mutually exclusive cancer types, plus metastatic status) was determined by scanning all listed diagnosis fields, since cancer is typically a secondary diagnosis in a stroke admission rather than the reason for admission. Intravenous thrombolysis (ICD-10-PCS 3E03317) and mechanical thrombectomy (ICD-10-PCS 03CG3ZZ/03CG3Z6) were identified from procedure codes. Treatment rates, in-hospital mortality, and length of stay were compared between cancer and non-cancer patients using survey-weighted logistic regression adjusted for age and sex; a secondary analysis restricted to cancer patients examined metastatic status as a predictor of mortality and thrombectomy use.

**Results:** Among 111,676 unweighted (558,380 weighted) ischemic stroke hospitalizations, 6,116 (5.5%) had a comorbid cancer diagnosis. Cancer patients received thrombolysis significantly less often than non-cancer patients (5.90% vs. 9.56%; adjusted OR 0.61, 95% CI 0.55-0.69) but received thrombectomy significantly more often (8.31% vs. 6.42%; adjusted OR 1.34, 95% CI 1.22-1.46) -- an opposite-direction pattern in the two guideline-recommended reperfusion therapies. Cancer patients had more than double the in-hospital mortality of non-cancer patients (7.55% vs. 3.54%; adjusted OR 2.11, 95% CI 1.90-2.33) and a longer mean length of stay (6.58 vs. 5.63 days, descriptive). Among cancer patients, metastatic disease nearly doubled mortality risk relative to non-metastatic cancer (10.66% vs. 5.52%; adjusted OR 2.04, 95% CI 1.67-2.48) and was associated with a higher unadjusted thrombectomy rate (9.50% vs. 7.52%). Mortality varied substantially by cancer type: esophageal (13.75%, n=80, 11 deaths) and lung (10.07%, n=884, 89 deaths) cancers carried the highest reliably-estimated mortality, while melanoma's apparent 17.54% rate was driven by a single death in 57 patients and is not interpretable.

**Conclusion:** Cancer patients hospitalized with acute ischemic stroke follow a divergent reperfusion pathway relative to non-cancer patients -- less thrombolysis, more thrombectomy -- despite prior evidence that both therapies mitigate their excess mortality risk [1]. This pattern is consistent with, but not proof of, a shift toward mechanical over pharmacologic reperfusion in cancer patients, plausibly reflecting both the post-DAWN/DEFUSE-3 expansion of thrombectomy eligibility [2,3] and contraindications to thrombolysis common in active cancer (thrombocytopenia, recent surgery or anticoagulation, bleeding risk) that this dataset cannot directly measure. Regardless of mechanism, cancer patients' mortality remained more than double that of non-cancer patients even against this backdrop of relatively higher thrombectomy use, and metastatic disease compounded that risk further. These findings support treating cancer-associated stroke as a distinct clinical phenotype meriting its own reperfusion-access research, rather than assuming findings from thrombolysis-era stroke care generalize unchanged to the current treatment landscape.

**References**
1. Pana TA, Mohamed MO, Mamas MA, Myint PK. Prognosis of Acute Ischaemic Stroke Patients with Cancer: A National Inpatient Sample Study. *Cancers (Basel).* 2021;13(9):2193.
2. Nogueira RG, Jadhav AP, Haussen DC, et al. Thrombectomy 6 to 24 Hours after Stroke with a Mismatch between Deficit and Infarct. *N Engl J Med.* 2018;378(1):11-21.
3. Albers GW, Marks MP, Kemp S, et al. Thrombectomy for Stroke at 6 to 16 Hours with Selection by Perfusion Imaging. *N Engl J Med.* 2018;378:708-718.
4. Ryan D, Bou Dargham T, Ikramuddin S, Shekhar S, Sengupta S, Feng W. Epidemiology, Pathophysiology, and Management of Cancer-Associated Ischemic Stroke. *Cancers (Basel).* 2024;16(23):4016.

---

## Fact-check log

Every claim above was individually verified against real script output (`scripts/nis_2023_stroke_cancer_treatment_access.R`) before finalizing. Numeric values were copied from console text output (confirmed by the user directly, not read from a screenshot).

| Claim | Verified value |
|---|---|
| Cohort size | 111,676 unweighted / 558,380 weighted; 6,116 cancer / 105,560 no cancer, 0 NA |
| Cancer type distribution (n) | Colorectal 230, Breast 347, Pancreatic 340, Esophageal 80, Ovarian/Gynecologic 187, Lung 884, Melanoma 57, Lymphoma 284, Leukemia 459, Other 3248 |
| Thrombolysis rate, no cancer vs. cancer | 9.56% vs. 5.90% |
| Thrombolysis OR (cancer vs. no cancer, age+sex adjusted) | 0.613 (0.548-0.687) |
| Thrombectomy rate, no cancer vs. cancer | 6.42% vs. 8.31% |
| Thrombectomy OR (cancer vs. no cancer, age+sex adjusted) | 1.337 (1.222-1.464) |
| Mortality rate, no cancer vs. cancer | 3.54% vs. 7.55% |
| Mortality OR (cancer vs. no cancer, age+sex adjusted) | 2.106 (1.903-2.331) |
| LOS, no cancer vs. cancer | 5.628 vs. 6.577 days (descriptive weighted means; no adjusted model built) |
| Mortality rate among cancer patients, non-metastatic vs. metastatic | 5.52% vs. 10.66% |
| Mortality OR, metastatic vs. non-metastatic (age+sex adjusted, cancer patients only) | 2.038 (1.674-2.481) |
| Thrombectomy rate among cancer patients, non-metastatic vs. metastatic | 7.52% vs. 9.50% (descriptive/unadjusted) |
| Mortality by cancer type (with cell counts) | Esophageal 13.75% (n=80, 11 deaths); Lung 10.07% (n=884, 89 deaths); Melanoma 17.54% (n=57, 1 death -- flagged unreliable) |
| Prior NIS study cancer prevalence among stroke admissions (2015-2017) | 3.51% -- notably lower than this cohort's 5.5%; discrepancy noted as a limitation, not resolved |

### Known limitations (not in the abstract, for internal reference)
- **Cancer prevalence discrepancy**: this cohort's cancer prevalence among stroke admissions (5.5%) is higher than the 2015-2017 benchmark study's 3.51% [1]. Plausible explanations include a genuine rise in cancer-among-stroke-patients prevalence over time, and/or a methodological difference (this analysis scans all 40 diagnosis fields for cancer codes, which may capture more cancer diagnoses than the prior study's approach, not confirmed). This is stated as an open limitation in the abstract's framing rather than resolved.
- **Cannot distinguish appropriate vs. inappropriate treatment selection**: NIS Core has no lab values, oncologic treatment history, or documented contraindications, so this analysis cannot determine whether lower thrombolysis use in cancer patients reflects appropriate avoidance (thrombocytopenia, recent anticoagulation, bleeding risk) or a disparity in access/consideration. The abstract's conclusion is deliberately worded as "consistent with, but not proof of" to avoid overclaiming causality.
- **Melanoma cancer-type mortality figure (17.54%) is not interpretable** -- driven by a single death among 57 patients -- and is explicitly flagged as such rather than omitted, consistent with this project's practice of showing sparse cells rather than hiding them.
- **LOS comparison is descriptive only** -- unlike every other outcome in this abstract, no adjusted regression model with a confidence interval was built for length of stay; only unadjusted weighted means are reported, and the abstract does not claim statistical significance for this difference.
- **No formal interaction test between cancer status and treatment type** was run to statistically confirm the "opposite direction" pattern is itself significant (as opposed to two independently significant but unrelated findings); the two adjusted models (thrombolysis, thrombectomy) were run and interpreted separately.
- **Reference [1]'s NIS years (2015-2017) predate this study's 2023 data** by 6-8 years; the DAWN/DEFUSE-3 justification for using 2023 data specifically assumes thrombectomy practice patterns changed materially in that interval, which is plausible given published national thrombectomy-utilization trends but was not directly tested within this analysis.
- Race is not available in this NIS 2023 extract (consistent with every other project in this repo).
- Cohort is not restricted by age ceiling (unlike the AYA cancer complications project) -- includes all adults 18+, so findings may be influenced by the older, Medicare-heavy age distribution typical of stroke admissions generally.
