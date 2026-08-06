# Complicated Index Hospitalization Abstract Draft

**Title:** Divergent Severity Across Common Medicine Admissions: A National Comparison of Mortality, ICU-Level Care, and Length of Stay

**Background:** Pneumonia, heart failure (CHF) exacerbation, sepsis, COPD exacerbation, and diabetic ketoacidosis (DKA) are among the most common reasons for adult medicine hospitalization, but their relative severity is rarely compared directly within a single national cohort using consistent outcome definitions.

**Methods:** Using the 2023 National Inpatient Sample, we identified adult (age ≥18) hospitalizations with a principal diagnosis of pneumonia, CHF, sepsis, COPD exacerbation, or DKA. Survey-weighted logistic regression estimated adjusted odds of in-hospital mortality and mechanical ventilation (an ICU-level-care proxy), and linear regression estimated adjusted differences in length of stay (LOS), each model referenced to pneumonia and adjusted for age and sex.

**Results:** Among 730,437 unweighted (3,652,185 weighted) hospitalizations, mortality ranged from 0.5% (DKA) to 8.7% (sepsis). Relative to pneumonia, sepsis carried markedly higher adjusted odds of mortality (OR 4.27, 95% CI 4.09–4.46) and mechanical ventilation (OR 4.23, 95% CI 4.05–4.41), plus 2.55 additional days of LOS (95% CI 2.49–2.62). CHF was associated with modestly higher mortality (OR 1.69, 95% CI 1.54–1.84) and longer LOS (+1.33 days, 95% CI 1.04–1.63), but lower odds of mechanical ventilation (OR 0.82, 95% CI 0.74–0.92). COPD and DKA were both associated with significantly lower mortality (OR 0.46 and 0.41, respectively), shorter LOS (–0.79 and –1.25 days), and lower or unchanged mechanical ventilation odds (COPD OR 0.95, 95% CI 0.88–1.02, not significant; DKA OR 0.56, 95% CI 0.52–0.61) compared with pneumonia.

**Conclusion:** Among five common causes of adult medicine hospitalization, sepsis is disproportionately associated with death, ICU-level care, and prolonged stay, while COPD exacerbation and DKA are comparatively low-risk despite frequently being grouped together with pneumonia and CHF as routine "floor" admissions. These national estimates may help calibrate expectations for resource allocation and risk communication across common admission types, and establish a baseline cohort for further work examining hospital-level drivers of variation in these outcomes.

---

## Fact-check log

Every claim above was individually verified against real script output (`scripts/nis_2023_complicated_hospitalization.R`) before finalizing:

| Claim | Verified value |
|---|---|
| Cohort size | 730,437 unweighted / 3,652,185 weighted |
| Mortality range | DKA 0.54% (lowest) – Sepsis 8.72% (highest) |
| Mortality OR, Sepsis vs. Pneumonia | 4.271 (4.087–4.464) |
| Mortality OR, CHF vs. Pneumonia | 1.685 (1.542–1.840) |
| Mortality OR, COPD vs. Pneumonia | 0.456 (0.414–0.502) |
| Mortality OR, DKA vs. Pneumonia | 0.412 (0.360–0.472) |
| Mech vent OR, Sepsis vs. Pneumonia | 4.226 (4.046–4.413) |
| Mech vent OR, CHF vs. Pneumonia | 0.822 (0.736–0.918) |
| Mech vent OR, COPD vs. Pneumonia | 0.947 (0.884–1.015) -- NOT significant, CI crosses 1 |
| Mech vent OR, DKA vs. Pneumonia | 0.560 (0.515–0.609) |
| LOS difference, Sepsis vs. Pneumonia | +2.554 days (2.488–2.619) |
| LOS difference, CHF vs. Pneumonia | +1.334 days (1.042–1.626) |
| LOS difference, COPD vs. Pneumonia | -0.792 days (-0.846 to -0.738) |
| LOS difference, DKA vs. Pneumonia | -1.252 days (-1.330 to -1.175) |

All 12 comparisons are statistically significant except one: COPD's mechanical ventilation OR (0.947, 95% CI 0.884–1.015). The Results/Conclusion above state this explicitly rather than implying uniform significance.

### Known limitations (not in the abstract, for internal reference)
- Cohort defined by principal diagnosis only -- excludes patients where one of these conditions was a secondary complication of a different admitting diagnosis.
- Mechanical ventilation proxy captures invasive ventilation only (ICD-10-PCS 5A19-); noninvasive ventilation (BiPAP) is not captured, so this likely undercounts true ICU-level care, especially for CHF and COPD where NIV is a common initial strategy -- this may partly explain CHF's lower-than-expected mech vent odds relative to pneumonia.
- No lab values (e.g., lactate, BNP) or vital signs are available in NIS Core, so severity adjustment is necessarily limited to age, sex, and diagnosis category -- not a full severity index.
- Race is not available in this NIS 2023 extract (consistent with the other two projects in this repo).
- Adjusted models compare each diagnosis to pneumonia as reference; pairwise comparisons between non-pneumonia diagnoses (e.g., sepsis vs. CHF) were not directly modeled but can be inferred from the reported estimates if needed.
