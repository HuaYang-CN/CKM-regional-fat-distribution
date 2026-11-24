# Regional fat distribution and advanced CKM

Analysis code for *Regional Fat Distribution and Advanced Cardiovascular–Kidney–Metabolic Syndrome in Chinese and US Adults*, prepared as an Article for Nutrients. This repository contains version **1.1.0** of the study code. Its predecessor is the published [GitHub v1.0.2 release](https://github.com/HuaYang-CN/CKM-regional-fat-distribution/releases/tag/v1.0.2), archived at [Zenodo](https://doi.org/10.5281/zenodo.23183495).

## What changed

The current workflow reconstructs the 2011–2018 NHANES sample from official CDC data, applies the manuscript’s PREVENT-based staging rules, and scores standardized regional adiposity inputs using the fixed Chinese loading matrix. In the 2017–2018 alcohol questionnaire, a lifetime abstainer (ALQ111 = 2) whose ALQ121 item was skipped is classified as having had no alcohol in the preceding 12 months. This restores eligible survey participants to the alcohol-complete domain. The historical v1.0.2 table and analysis scripts alone do not reproduce the current alcohol sensitivity estimates.

## Run the public analysis

Run from the repository root with Rscript on PATH. Use a dedicated directory for official public input and a new directory for results. On a first run allow the required official downloads. An offline repeat requires the complete, checksum-matched CDC cache.

~~~sh
Rscript scripts/public_nhanes.R /path/to/nhanes-cache /path/to/new-results
Rscript scripts/public_nhanes.R /path/to/nhanes-cache /path/to/another-new-results --offline
~~~

The input inventory is in [the CDC file manifest](manifests/public_cdc_inputs.tsv). The fixed coefficients are in [the loading matrix](manifests/public_fixed_loadings.csv). The main survey model uses PC1, PC2, sex and current smoking. United States effects are per survey-weighted standard deviation. Alcohol and age are added separately. Treat stages 3–4 versus stage 2 as a survey domain and retain the original score scaling. Consult [Reproducibility](documentation/REPRODUCIBILITY.md) for required packages, actual test coverage and execution boundaries.

**Execution check.** Starting from 72 verified official public files, the raw-data test reproduced 2,688 participants (179 advanced), the 2,262 and 1,445 alcohol domains, and the primary, age-adjusted and stage-restricted PC2 odds ratios. Additional modules and restricted analyses have different verification coverage as documented below.

## Clinical data and publication displays

The Chinese cross-sectional and repeated-measures models require authorized inputs. Their upstream cohort assembly is not available. Requests and conditions are described in [Data access](documentation/DATA_ACCESS.md). Earlier figure numbers reflect analytical rather than manuscript order. The [output map](manifests/output_to_manuscript_map.tsv) connects source results with the 4 main figures, 2 main tables, 12 supplementary figures and 27 supplementary tables. The clinical time-series graphics require restricted inputs. The final Figure S10 renderer takes aggregate curves, deciles, calibration coefficients and sample counts.

The [analysis manifest](manifests/analysis_manifest.tsv) identifies current sources and older scripts. Read the [workflow limitations](documentation/PUBLIC_WORKFLOW_LIMITATIONS.md) before interpreting any historical output. The 1.0.2 GitHub and Zenodo records refer to the earlier implementation.

## Citation and license

Software creator and license attribution are retained from the upstream repository. See [CITATION.cff](CITATION.cff) and the [MIT license](LICENSE).
