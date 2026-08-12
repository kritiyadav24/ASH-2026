# Systematic Review & Meta-Analysis Protocol

**Working title:** Antithrombotic Therapy for Secondary Stroke Prevention in Patients with Active Cancer: An Updated Systematic Review and Meta-Analysis with Metastatic-Status Subgroup Analysis

**Status:** Draft protocol, not yet registered. Written for a PGY-3 resident scholarly project -- this topic already has 2-3 network meta-analyses published in 2025 (see "Relationship to existing literature" below), so this is explicitly framed as an *update with a differentiating subgroup angle*, not a claim of first-in-field novelty.

---

## 1. Rationale

Cancer-associated ischemic stroke carries elevated recurrence and mortality risk, and the optimal antithrombotic strategy for secondary prevention (anticoagulation vs. antiplatelet vs. no therapy) remains genuinely unsettled in guidelines. The most comprehensive existing synthesis (Systematic review and network meta-analysis, *European Journal of Clinical Pharmacology*, 2025; PubMed ID 40332574 -- full author list not yet independently confirmed, verify before citing in any final manuscript) searched PubMed/Embase/Scopus through March 2, 2025 and pooled only **11 studies (4 RCTs, 6 retrospective cohorts, 1 case series), n=1,319 total** -- a small evidence base for a network meta-analysis with multiple treatment arms. That review found antiplatelets ranked highest for reducing stroke recurrence (RR 0.44, 95% CI 0.20-0.96) followed by LMWH (RR 0.50, 95% CI 0.26-0.96), both superior to no treatment, with subgroup analyses by D-dimer level and stroke territory but no confirmed metastatic-status stratification.

Two things justify a fresh, focused effort rather than treating this as closed:
1. **The evidence base is still thin and actively growing.** New cohorts in this space are being published on a rolling basis (e.g., a 2026 piece on the "thrombectomy dilemma" in cancer-associated stroke, a 2025 "CanStroke Protocol" paper) -- an updated search will likely capture additional eligible studies the March 2025 cutoff missed.
2. **Metastatic status is a clinically obvious effect-modifier that does not appear to have been formally stratified** in the existing network meta-analysis. Bleeding risk, thrombotic risk, and life expectancy all differ substantially between metastatic and non-metastatic cancer, so pooling them together risks obscuring a clinically important interaction -- directly analogous to the cancer-type confounding pattern already demonstrated in this repo's AYA cancer complications project.

---

## 2. PICO Question

- **Population:** Adults (18+) with active cancer (any type) who experienced an index ischemic stroke or transient ischemic attack (TIA) and require secondary prevention.
- **Intervention:** Therapeutic anticoagulation (low-molecular-weight heparin, vitamin K antagonist, or direct oral anticoagulant).
- **Comparator:** Antiplatelet therapy, or no antithrombotic therapy, or a different anticoagulant class (all pairwise comparisons feed a network meta-analysis if the evidence base supports it).
- **Outcomes:**
  - *Primary:* Recurrent ischemic stroke, TIA, or systemic thromboembolism.
  - *Secondary:* Major bleeding (ISTH criteria if reported), intracranial hemorrhage specifically, all-cause mortality, functional outcome (modified Rankin Scale) where reported.
  - *Prespecified subgroup/effect-modifier analysis:* metastatic vs. non-metastatic status at time of stroke; cancer type (solid vs. hematologic, and by organ system where feasible); D-dimer level (replicating the existing review's approach for comparability).

---

## 3. Eligibility Criteria

**Include:**
- Randomized controlled trials, prospective cohort studies, retrospective cohort studies, and case-control studies.
- Adult patients (18+) with active cancer (on treatment, or diagnosed within a prespecified lookback window -- to be finalized, likely 6 months, matching common cancer-associated-thrombosis study conventions) and a confirmed ischemic stroke or TIA.
- Studies reporting extractable comparative data on at least one prespecified outcome across at least two antithrombotic strategies (or one strategy vs. no treatment).
- English language (note as a limitation; do not attempt translation given resident-level resource constraints).

**Exclude:**
- Pediatric populations.
- Primary stroke prevention studies (no prior stroke/TIA at enrollment).
- Studies of hemorrhagic stroke only.
- Case reports and case series with fewer than 10 patients (a higher bar than the existing 2025 review, which included a case series -- tightening this improves internal validity at some cost to sample size; worth discussing with a mentor before finalizing).
- Studies without extractable outcome data by treatment arm.

---

## 4. Search Strategy

**Databases:** MEDLINE (via PubMed), Embase, Scopus, Cochrane CENTRAL. Cochrane CENTRAL is added relative to the existing 2025 review, which used PubMed/Embase/Scopus only.

**Date range:** Inception to date of search (no lower bound -- this is a secondary-prevention drug-therapy question, not tied to a specific guideline-change year the way the stroke-treatment-access NIS project was tied to DAWN/DEFUSE-3).

**Draft search concept (to be refined into full Boolean strings per database with a librarian's help before execution):**
`(cancer OR neoplasm* OR malignan* OR tumor OR tumour) AND (stroke OR "cerebral infarction" OR "transient ischemic attack" OR TIA OR "cerebrovascular accident") AND (anticoagul* OR antiplatelet* OR warfarin OR heparin OR "low molecular weight heparin" OR LMWH OR DOAC OR "direct oral anticoagulant" OR apixaban OR rivaroxaban OR edoxaban OR dabigatran OR aspirin OR clopidogrel) AND (secondary prevention OR recurren*)`

**Supplementary:** manual reference-list screening of the existing 2025 network meta-analysis and its cited primary studies, plus forward citation search (studies citing it) to catch anything published since its March 2025 cutoff.

---

## 5. Study Selection & Data Extraction

- Two independent reviewers screen titles/abstracts, then full texts, against the eligibility criteria above; conflicts resolved by discussion or a third reviewer. (Note: as a single resident, you will need a co-reviewer -- a co-resident, medical student, or your research mentor -- for this step; dual independent screening is a PRISMA/methodological expectation, not optional.)
- Standardized extraction form: study design, country, sample size, cancer type distribution, metastatic status (n and %), stroke type/territory, antithrombotic regimen and dosing, comparator, follow-up duration, outcome event counts by arm, D-dimer reporting.

---

## 6. Risk of Bias Assessment

- RCTs: Cochrane Risk of Bias 2 (RoB2) tool.
- Cohort and case-control studies: Newcastle-Ottawa Scale (NOS).

---

## 7. Statistical Analysis Plan

- Random-effects pairwise meta-analysis (restricted maximum likelihood, REML) for each direct comparison with sufficient studies.
- Network meta-analysis (frequentist, via the `netmeta` R package) if the evidence base supports a connected network with adequate loops -- mirrors the existing review's approach, enabling direct comparability of results.
- Heterogeneity assessed via I² and prediction intervals, not just the p-value for Q, consistent with this repo's established discipline of not over-trusting a single heterogeneity statistic.
- Prespecified subgroup analysis / meta-regression by metastatic status as the primary differentiating analysis; sensitivity analysis excluding case-control/retrospective studies to assess robustness of RCT-only estimates.
- Publication bias: funnel plot and Egger's test if ≥10 studies contribute to a given comparison (below that threshold, underpowered and will be noted as such rather than reported).
- Software: R (`meta`, `metafor`, `netmeta` packages) -- consistent with the R environment already set up for the NIS projects in this repo.

---

## 8. Reporting

PRISMA 2020 and PRISMA-NMA (if a network meta-analysis is feasible) reporting guidelines.

---

## 9. Registration

**Not yet done -- this is the immediate next step before starting the literature search.** Register the protocol on PROSPERO (international prospective register of systematic reviews) prior to running the search, which is expected by most journals and prevents duplicate effort. PROSPERO registration requires: title, review question, eligibility criteria, search strategy, and named reviewers -- all drafted above and ready to adapt into PROSPERO's submission form.

---

## 10. Relationship to Existing Literature (for internal reference, not for the manuscript introduction verbatim)

Confirmed via search (not exhaustive full-text review -- verify directly before finalizing manuscript citations):
- Systematic review and network meta-analysis, *European Journal of Clinical Pharmacology*, 2025 (PMID 40332574) -- PubMed/Embase/Scopus to March 2, 2025; 11 studies, n=1,319; antiplatelets and LMWH both beat no treatment; subgroups by D-dimer and stroke territory.
- A related abstract, "Secondary Prevention for Ischemic Stroke in Patients with Cancer: A Systematic Review and Network Meta-analysis," *Neurology* (P3-5.024) -- likely a conference-abstract version of related or overlapping work; needs direct comparison to the EJCP paper to determine if it's the same study group or an independent effort.
- "Anticoagulant versus antiplatelet treatment for secondary stroke prevention in patients with active cancer" (PMC12479245) -- another 2025 entry; scope not yet fully characterized, needs full-text review during the actual search phase.
- A 2025 preprint, "Anticoagulation versus Antiplatelet Therapy After Cancer-Associated Ischemic Stroke: A Net-Benefit Threshold Analysis" (Research Square) -- preprint status, not yet peer-reviewed; a different analytic framework (net-benefit threshold rather than pooled meta-analysis).

**Action item before committing further time:** pull full text of all of the above during the literature search phase to precisely characterize overlap, rather than relying on search-snippet summaries as done here for initial scoping.
