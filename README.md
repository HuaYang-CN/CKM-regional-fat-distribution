# Fixed-loading transport of a regional adiposity pattern across cohorts with concurrent cardiovascular-kidney-metabolic associations

## Purpose and manuscript relationship
Software accompanying the final manuscript titled above. This release preserves the retained method-locked analysis and construction sources. It supports reproducibility review without changing results, samples, models, figures or tables.

## Repository structure
`R/` contains the two public construction scripts and four final refactored sources; `scripts/` contains read-only preflight and final display construction; `manifests/` records inputs and analysis order; `sessionInfo/` contains two historical run records; `documentation/` describes governance, schemas, attribution and limitations. `00_config.R` configures public construction; final analyses source `R/00_CKM_refactored_config.R`. Generated `data/`, `restricted_data/` and `results/` are excluded.

## Data sources and public / controlled-access boundary
- **A. Public NHANES:** 2011-2018 public-use components from [CDC/NCHS](https://wwwn.cdc.gov/nchs/nhanes/). 07a acquires/harmonizes them; 07b creates the revised staged analysis input. Raw/intermediate files are regenerated, not redistributed.
- **B. Chinese cohorts:** code is provided; participant-level Chinese clinical data are available only through controlled access. They are not reproducible from this repository alone.
- **C. Derived manuscript outputs included:** none. Locked manuscripts, tables, figures, master results and participant-level QA are not bundled.
- **D. Restricted exclusions:** Chinese source spreadsheets/records, participant scores/IDs, master RDS, private logs and credentials.

## Software environment
The retained final analysis and table/figure execution records report **R 4.5.1 (2025-06-13 ucrt)**. See `sessionInfo/analysis_sessionInfo.txt` and `sessionInfo/tables_figures_sessionInfo.txt`, which are authoritative historical records; only local input-path text has been redacted. Verified versions include survey 4.4-2, rms 8.1-0, pROC 1.19.0.1, caret 7.0-1, recipes 1.3.1, gbm 2.2.2, randomForest 4.7-1.2, glmnet 4.1-10 and fastshap 0.1.1. Package sources/licenses remain separate. No dependency lockfile or complete environment container is claimed. Construction also uses preventr, nhanesA, haven and tidyverse components; see source package lists. Original scripts may install missing packages when run; preflight installs nothing.

## Reproduction workflow and main script order
Run direct commands from the repository root with Rscript on your PATH. Start with the read-only check:
```text
Rscript scripts/00_reproducibility_preflight.R
```
Then follow the public construction chain. The complete analysis sequence below is for authorized users with the controlled inputs. Details of inputs, outputs and manuscript mapping are in `manifests/analysis_manifest.tsv`.

## NHANES public-data workflow
```text
Rscript R/07a_nhanes_download_harmonize.R
Rscript R/07b_nhanes_revised_prevent_ckm_qc.R
```
07a downloads public source components and produces `data/public/nhanes/data_processed/NHANES_2011_2018_harmonized_all.rds`; 07b consumes that file and produces `NHANES_2011_2018_external_validation_ready_REVISED.rds`, staged data and QC summaries. Internet access, packages and disk space are required. Large downloads were not performed in Stage10B.

**Final-analysis boundary:** the retained final monolithic analysis executes China sections first and uses a Chinese PCA reference for transport. It cannot be run from public NHANES data alone. The acquisition/construction chain is public-data reproducible; independent execution of all final NHANES analyses is not currently provided. Read `documentation/PUBLIC_WORKFLOW_LIMITATIONS.md` before claiming complete public-data reproduction.

## Chinese restricted-data workflow
Restricted input not distributed. Authorized users provide `restricted_data/china/cross_filtered.xlsx` and `restricted_data/china/suifang.xlsx`, or set `CKM_CHINA_CROSS_FILE` and `CKM_CHINA_LONG_FILE` to authorized paths. Schemas and governance are in `documentation/RESTRICTED_INPUTS.md`. Do not add real records to schemas.

The final config defaults to `results/` and the 07b output above. Optional path overrides: `CKM_REPO_ROOT`, `CKM_OUTPUT_ROOT`, `CKM_NHANES_READY_FILE`, `CKM_NHANES_RAW_DIR`. Preserve all scientific settings. Then:
```text
Rscript R/01_CKM_refactored_analysis_v7_methodlock.R
Rscript R/02_CKM_tables_figures_highimpact_v19_reconciled.R
Rscript R/03_CKM_supplementary_tables_S1_S26_v8_reconciled.R
Rscript scripts/Stage7_5_Final_Figure_Supplement_Production.R
```
The last script defaults to the master under `results/01_locked_analysis_objects/`; if using another output root, set `STAGE75_MASTER_RDS` to the authorized master file. Production writes into `results/stage7_5_production/`. Analysis/production can generate participant-level objects and QA CSVs: keep them controlled and out of public archives.

## Expected outputs and manuscript mapping
The final main analysis saves `CKM_refactored_master_results.rds` and tidy/audit records. The table/figure and Supplement scripts consume saved results; supplementary tables cover 1-26. Final display construction supports Figure 1 (study architecture/Chinese loading contrast), Figure 2 (Chinese concurrent associations), Figure 3 (NHANES replication/transport) and Figure 4 (selected repeated measures), plus Supplementary Figures 1 and 13. Generated output names can retain historical labels; the final map is the analysis manifest. Frozen publication artifacts are not distributed or regenerated here.

## License
MIT; see LICENSE. Existing 2026 HuaYang-CN copyright notice preserved. Dependencies retain their licenses; attribution is described in `documentation/CREATORS_AND_LICENSE.md`.

## Citation
Use CITATION.cff for this software version 1.0.2 and cite the associated manuscript. No manuscript DOI or new Zenodo DOI has been assigned in this candidate. Hua Yang is the software creator, explicitly confirmed by the author for this release; manuscript author/contribution roles are unchanged.

## Data Availability
Public NHANES components can be retrieved using the provided code. Chinese inputs require investigator and applicable institutional review. No automatic access entitlement, unrestricted redistribution or identifiable source-record sharing is implied. See the approved controlled-access terms in `documentation/RESTRICTED_INPUTS.md`.

## Code Availability
The [existing public repository](https://github.com/HuaYang-CN/CKM-regional-fat-distribution) and historical v1.0.0 release were verified during preparation. This 1.0.2 candidate is a new correction release and must not overwrite v1.0.0. Publication status and exact archive identifiers are recorded separately in the Stage10B fact record. An older DOI must not be represented as archiving this candidate without a content/version match.

## Limitations of reproducibility
Only syntax, provenance, portability and read-only preflight were checked here; model fitting and scientific reproduction were not rerun. The final workflow needs controlled Chinese inputs; no NHANES-only final execution wrapper is supplied. Chinese upstream age/cohort-construction code was not retained. Two historical sessions document the original environment; installed packages today may differ. Prepared older split analyses and the absent graphical abstract are excluded. No fully reproducible-from-repository-alone claim is made.

## Contact
Corresponding author: Jiajun Zhao, jjzhao@sdu.edu.cn; [ORCID](https://orcid.org/0000-0002-8697-4791).
