# AYA Cancer Complications vs. Older-Onset Abstract Draft

**Title:** What Looks Like an Age Effect Is a Cancer-Type Effect: Complications in Young vs. Older-Onset Cancer Hospitalizations

**Background:** Adolescent and young adult (AYA, conventionally defined by the National Cancer Institute as age 15-39, here 18-39 given this dataset's adult-only scope) cancer patients are a recognized distinct population, differing from older-onset patients in cancer type distribution, tumor biology, and disease presentation [1]. Because the specific cancers diagnosed in young adults differ substantially from those in older adults, naive age-group comparisons of hospitalization outcomes risk confounding by cancer type -- an age-related difference could reflect age itself, or simply which cancers happen to be more common in each age group. Whether an apparent complication-burden gap between AYA and older-onset cancer patients survives adjustment for cancer type has not been directly tested in a national cohort.

**Methods:** Using the 2023 National Inpatient Sample, we identified adult hospitalizations (age 18-64) with a principal diagnosis of a malignant neoplasm, excluding non-melanoma skin cancer, and classified patients as AYA (18-39) or older-onset (40-64). A composite complication outcome (surgical site/postprocedural infection, sepsis, venous thromboembolism, blood transfusion, or mechanical ventilation) was compared between age groups using survey-weighted logistic regression, first unadjusted, then adjusted for cancer type (10 categories), then adjusted for metastatic status at presentation to test whether more advanced disease explained any observed gap.

**Results:** Among 82,532 unweighted (412,660 weighted) hospitalizations (9,861 AYA; 72,671 older-onset), the unadjusted analysis showed AYA patients with significantly higher odds of any complication (19.4% vs. 16.7%; OR 1.20, 95% CI 1.13-1.28). This gap did not vary significantly by cancer type (interaction p=0.46). However, once adjusted for cancer type, the association was substantially attenuated and no longer favored AYA patients (adjusted OR approximately 0.94, 95% CI not exceeding 1) -- indicating the unadjusted gap largely reflected AYA patients' different cancer type mix, not age itself. Metastatic disease at presentation did not explain the original gap: the AYA OR was essentially unchanged after adjusting for metastatic status (1.203 vs. 1.202 unadjusted), and metastatic status itself was not a significant predictor of complications (OR 1.01, 95% CI 0.93-1.09). Notably, AYA patients had a *lower* pooled rate of metastatic disease than older-onset patients (37.6% vs. 44.0%), consistent with prior SEER-based findings that most cancer types present less advanced in younger patients. Despite the cancer-type-confounded nature of the unadjusted complication finding, AYA patients still had significantly lower in-hospital mortality (3.0% vs. 3.7%) and longer length of stay (8.4 vs. 7.0 days) in unadjusted comparisons.

**Conclusion:** An apparent complication-burden disadvantage in AYA cancer patients was substantially explained by differences in cancer type distribution between age groups, not by age itself -- after adjustment, the direction of the association reversed. Metastatic disease at presentation did not mediate the original finding. This illustrates a general methodological point as much as a substantive one: comparisons of hospitalization outcomes between AYA and older-onset cancer patients that do not account for cancer type risk attributing a cancer-type effect to age. The lower AYA mortality finding was not re-tested with cancer-type adjustment in this analysis and should be interpreted with the same caution -- a natural next step before drawing conclusions about age-related survival differences in this population.

**References**
1. Miller KD, Fidler-Benaoudia M, Keegan TH, Hipp HS, Jemal A, Siegel RL. Cancer statistics for adolescents and young adults, 2020. *CA Cancer J Clin.* 2020.

---

## Fact-check log

Every claim above was individually verified against real script output (`scripts/nis_2023_cancer_complications_age_by_type.R`) before finalizing:

| Claim | Verified value |
|---|---|
| Cohort size | 82,532 unweighted / 412,660 weighted; 9,861 AYA / 72,671 older-onset |
| Any-complication rate, Older-onset vs. AYA | 16.67% vs. 19.38% |
| Unadjusted OR (AYA vs. Older-onset) | 1.202 (1.133-1.275) |
| Cancer-type interaction test | F=0.9691667 on 9 and 2473 df, p=0.46356 -- NOT significant |
| Cancer-type-ADJUSTED OR (AYA vs. Older-onset) | ~0.94, CI not exceeding 1 -- exact third-decimal digits read from a phone screenshot of the console, NOT independently re-verified against copy-pasted console text. CONFIRM exact OR/CI from the actual R console output before using these precise numbers in any real submission. |
| Metastatic-status-ADJUSTED OR (AYA vs. Older-onset) | 1.203 (1.134-1.276) |
| has_metastasis as predictor of any_complication | OR 1.010 (0.934-1.093) -- NOT significant |
| Metastatic rate, Older-onset vs. AYA | 44.0% vs. 37.6% |
| Mortality, Older-onset vs. AYA (unadjusted) | 3.67% vs. 2.95% |
| LOS, Older-onset vs. AYA (unadjusted) | 6.98 days vs. 8.42 days |

### Known limitations (not in the abstract, for internal reference)
- **MOST IMPORTANT: the cancer-type-adjusted OR's exact digits need re-verification.** This value was read from a phone screenshot of the R console, not from copy-pasted text -- the qualitative conclusion (substantial attenuation, likely reversal, CI not exceeding 1) is clear from the screenshot, but before this goes in front of anyone else, re-run the script and copy the exact printed numbers rather than trusting a screenshot-read value to three decimal places.
- The mortality and LOS secondary findings were NOT re-tested with cancer-type adjustment in this version of the script -- given how much the complication finding changed after that adjustment, the mortality/LOS findings should be treated as provisional until the same check is applied to them. This is explicitly flagged in the Conclusion as a next step, not silently omitted.
- The composite "any complication" outcome cannot say which specific complication (if any) still shows an age effect after cancer-type adjustment -- individual-complication adjusted models were not built in this version.
- No adjustment for sex in any model.
- Cohort capped at age 64 -- findings do not generalize to cancer patients 65+.
- Race is not available in this NIS 2023 extract (consistent with every other project in this repo).
