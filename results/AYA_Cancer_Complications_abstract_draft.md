# AYA Cancer Complications vs. Older-Onset Abstract Draft

**Title:** What Looks Like an Age Effect Is a Cancer-Type Effect -- Except for Survival: In-Hospital Outcomes in Young vs. Older-Onset Cancer Hospitalizations

**Background:** Adolescent and young adult (AYA, conventionally defined by the National Cancer Institute as age 15-39, here 18-39 given this dataset's adult-only scope) cancer patients are a recognized distinct population, differing from older-onset patients in cancer type distribution, tumor biology, and disease presentation [1]. Because the specific cancers diagnosed in young adults differ substantially from those in older adults, naive age-group comparisons of hospitalization outcomes risk confounding by cancer type -- an apparent age-related difference could reflect age itself, or simply which cancers happen to be more common in each age group.

**Methods:** Using the 2023 National Inpatient Sample, we identified adult hospitalizations (age 18-64) with a principal diagnosis of a malignant neoplasm, excluding non-melanoma skin cancer, and classified patients as AYA (18-39) or older-onset (40-64). A composite complication outcome (surgical site/postprocedural infection, sepsis, venous thromboembolism, blood transfusion, or mechanical ventilation), length of stay, and in-hospital mortality were each compared between age groups using survey-weighted regression, first unadjusted, then progressively adjusted for cancer type (10 categories), metastatic status at presentation, and sex.

**Results:** Among 82,532 unweighted (412,660 weighted) hospitalizations (9,861 AYA; 72,671 older-onset), unadjusted comparisons suggested AYA patients had higher odds of any complication (19.4% vs. 16.7%; OR 1.20, 95% CI 1.13-1.28) and longer length of stay (8.4 vs. 7.0 days; +1.45 days, 95% CI 1.15-1.74). Both findings did not survive cancer-type adjustment: the complication OR reversed to 0.88 (95% CI 0.83-0.94), and the LOS difference became non-significant (-0.01 days, 95% CI -0.24 to 0.23), indicating both were driven by AYA and older-onset patients having a different underlying mix of cancer types, not by age itself. In contrast, AYA patients' lower in-hospital mortality (3.0% vs. 3.7%; unadjusted OR 0.80, 95% CI 0.71-0.90) remained significant and stable across every adjustment: cancer-type-adjusted OR 0.77 (0.68-0.87); metastatic-status-and-sex-adjusted OR 0.86 (0.76-0.97); fully adjusted (all three together) OR 0.77 (0.68-0.87). Metastatic disease at presentation was, as expected, a strong independent predictor of mortality (OR 3.13, 95% CI 2.88-3.39) but did not explain the age-related mortality gap.

**Conclusion:** An apparent AYA disadvantage in complications and length of stay was fully explained by differences in cancer type distribution between age groups; once compared within the same cancer type, both effects vanished or reversed. In contrast, AYA patients' lower in-hospital mortality was robust and independent of cancer type, disease severity, and sex, consistent with age-related decline in physiologic reserve limiting older patients' capacity to survive an acute hospitalization [2]. This finding should be scoped narrowly: it describes survival through a single acute hospitalization, not long-term cancer-specific prognosis. Multiple studies have found young age to be an independent *adverse* prognostic factor for long-term recurrence-free and overall survival in breast cancer specifically, even after adjusting for stage [3] -- a pattern this analysis cannot address, since NIS captures only in-hospital outcomes. The two findings are not necessarily in conflict: young patients may be more likely to survive an acute hospitalization while simultaneously having more biologically aggressive disease over a longer time horizon.

**References**
1. Miller KD, Fidler-Benaoudia M, Keegan TH, Hipp HS, Jemal A, Siegel RL. Cancer statistics for adolescents and young adults, 2020. *CA Cancer J Clin.* 2020.
2. Chelluri L. Critical Illness in the Elderly: Review of Pathophysiology of Aging and Outcome of Intensive Care. *J Intensive Care Med.* 2001.
3. Young age: an independent risk factor for disease-free survival in women with operable breast cancer. *BMC Cancer.* (Full author list, volume, and page numbers not independently confirmed beyond the indexed title/journal -- see limitations below.)

---

## Fact-check log

Every claim above was individually verified against real script output (`scripts/nis_2023_cancer_complications_age_by_type.R`) before finalizing. All numeric values below were copied from R console text output directly (not read from a screenshot), including a re-verification of the complication adjusted OR that had earlier been read imprecisely from a photo.

| Claim | Verified value |
|---|---|
| Cohort size | 82,532 unweighted / 412,660 weighted; 9,861 AYA / 72,671 older-onset |
| Any-complication rate, Older-onset vs. AYA | 16.67% vs. 19.38% |
| Complication OR, unadjusted | 1.202 (1.133-1.275) |
| Complication OR, cancer-type-adjusted | 0.882 (0.826-0.940) |
| Complication OR, cancer-type + sex adjusted | 0.882 (0.827-0.941) -- confirms sex doesn't change the picture |
| Cancer-type interaction test (on unadjusted-style model) | F=0.9691667 on 9 and 2473 df, p=0.46356 -- NOT significant |
| Metastatic-status-adjusted complication OR | 1.203 (1.134-1.276) -- barely moved from unadjusted, metastatic status does not mediate |
| has_metastasis as predictor of any_complication | OR 1.010 (0.934-1.093) -- NOT significant |
| Metastatic rate, Older-onset vs. AYA | 44.0% vs. 37.6% |
| LOS, Older-onset vs. AYA (unadjusted) | 6.98 days vs. 8.42 days; difference +1.447 (1.154-1.739) |
| LOS difference, cancer-type-adjusted | -0.007 (-0.239 to 0.225) -- NOT significant |
| Mortality, Older-onset vs. AYA (unadjusted rate) | 3.67% vs. 2.95% |
| Mortality OR, unadjusted | 0.798 (0.705-0.903) |
| Mortality OR, cancer-type-adjusted | 0.766 (0.675-0.870) |
| Mortality OR, metastatic-status + sex adjusted (no cancer type) | 0.859 (0.759-0.973) |
| Mortality OR, fully adjusted (cancer type + metastatic status + sex) | 0.771 (0.680-0.874) |
| has_metastasis as predictor of mortality | OR 3.126 (2.883-3.390) |
| FEMALE as predictor of mortality | OR 0.882 (0.817-0.952) |

### Known limitations (not in the abstract, for internal reference)
- Reference [3] is cited by title and journal only -- full author list, volume, and page numbers were not independently confirmed beyond what search results provided. The underlying claim (young age as an independent adverse prognostic factor in breast cancer, replicated across multiple studies) was verified as real and repeatedly replicated, but this specific citation's full bibliographic detail should be confirmed against the actual paper before formal submission.
- The mortality finding describes in-hospital survival during a single acute hospitalization, NOT long-term cancer-specific survival, recurrence, or overall prognosis -- NIS Core has no follow-up data beyond a single admission. This scope limitation is stated explicitly in the Conclusion and should not be relaxed in any future version of this abstract.
- The "any complication" composite outcome cannot say which specific complication (if any) still shows an age effect after cancer-type adjustment -- individual-complication adjusted models were not built.
- Race is not available in this NIS 2023 extract (consistent with every other project in this repo).
- Cohort capped at age 64 -- findings do not generalize to cancer patients 65+.
- The interaction test (p=0.46) was run on the unadjusted-style unadjusted model structure (age_group * cancer_type) and answers a different question than the cancer-type-ADJUSTED main-effect models -- both are reported because they answer genuinely different questions (does the gap vary by type vs. does adjusting for type change the overall gap), not because one supersedes the other.
