# Regional Fat-Distribution Phenotypes and CKM Severity

Analysis code accompanying the manuscript:

**Regional Fat-Distribution Phenotypes and Cardiovascular-Kidney-Metabolic Syndrome
Severity: Cross-Sectional, Longitudinal, and Cross-Platform Replication Analyses**

This repository contains the R code used for the Chinese derivation analyses, secondary
internal model-performance analyses, exploratory repeated-measures analyses, NHANES
2011–2018 external cross-platform replication, fixed-loading transportability, sensitivity
analyses, age-adjustment sensitivity, and figure generation.

## Repository principles

- No individual-level Chinese clinical data are included.
- No local Windows paths or investigator-specific computer paths are required.
- NHANES data are public and can be downloaded by the included code.
- Generated results and downloaded raw data are excluded from Git by `.gitignore`.
- The final revised PREVENT/CKM staging is produced by `07b_nhanes_revised_prevent_ckm_qc.R`.
- Age-adjusted models are diagnostic sensitivity analyses; they do not replace the
  prespecified primary models.

## Directory structure

```text
.
├── 00_config.R
├── run_all.R
├── R/
│   ├── 00_install_packages.R
│   ├── 01_derivation_pca_collinearity.R
│   ├── 02_association_rcs.R
│   ├── 03_internal_nested_cv.R
│   ├── 04_internal_secondary_metrics.R
│   ├── 05_internal_posthoc_SHAP.R
│   ├── 06_internal_longitudinal.R
│   ├── 07a_nhanes_download_harmonize.R
│   ├── 07b_nhanes_revised_prevent_ckm_qc.R
│   ├── 08_nhanes_external_replication.R
│   ├── 09_nhanes_sensitivity_final_outputs.R
│   ├── 10_age_sensitivity.R
│   ├── 11_study_flow.R
│   ├── 12_graphical_abstract.R
│   └── 99_capture_session_info.R
├── data/
│   ├── private/china/
│   └── public/nhanes/
└── results/                  # generated; not committed
```

## Software

The manuscript reports analyses in **R 4.4.0**.

Install packages once from the repository root:

```bash
Rscript R/00_install_packages.R
```

The repository uses common CRAN packages including `survey`, `preventr`, `nhanesA`,
`caret`, `gbm`, `glmnet`, `fastshap`, `shapviz`, `rms`, `pROC`, and `ggplot2`.

## Reproducing the Chinese analyses

The Chinese participant-level datasets are not public. Authorized users should place
deidentified files in:

```text
data/private/china/cross_filtered.xlsx
data/private/china/suifang.xlsx
```

See `data/private/china/README.md` and the header-only schema files.

Then run:

```bash
Rscript run_all.R china
```

Locked checks used in the final analysis include a derivation cohort of **n=955** and a
distinct repeated-measures cohort of **n=100** (37 mild-to-severe progressors and 63
stable-severe participants).

## Reproducing the public NHANES analyses

Internet access is required for the first step.

```bash
Rscript run_all.R nhanes
```

Or run the stages individually:

```bash
Rscript R/07a_nhanes_download_harmonize.R
Rscript R/07b_nhanes_revised_prevent_ckm_qc.R
Rscript R/08_nhanes_external_replication.R
Rscript R/09_nhanes_sensitivity_final_outputs.R
```

The locked final external cohort contains **2,254** adults aged 30–59 years with **157**
advanced CKM events (Stages 3–4). Survey-weighted analyses use the combined fasting
subsample weight and NHANES PSU/strata variables.

Official source:
https://wwwn.cdc.gov/nchs/nhanes/

## Complete workflow

After the two authorized Chinese input files are present:

```bash
Rscript run_all.R all
```

This runs the Chinese and NHANES pipelines and then the age-sensitivity analysis.

Nested cross-validation and bootstrap analyses can be computationally intensive.

## Analysis-to-manuscript map

| Script | Main purpose |
|---|---|
| `01_derivation_pca_collinearity.R` | PCA phenotype derivation, collinearity, Figure 1 |
| `02_association_rcs.R` | Logistic associations and restricted cubic splines, Figure 2 |
| `03_internal_nested_cv.R` | Strict nested CV and feature-set comparison |
| `04_internal_secondary_metrics.R` | NRI/IDI, calibration, Brier, DCA |
| `05_internal_posthoc_SHAP.R` | Post-hoc GBM SHAP interpretation |
| `06_internal_longitudinal.R` | Repeated-measures PC1/PC2 analyses, final Main Figure 3 |
| `08_nhanes_external_replication.R` | NHANES PCA/association replication |
| `09_nhanes_sensitivity_final_outputs.R` | Fixed China-loading transfer, alcohol, LOCO and final Main Figure 4 |
| `10_age_sensitivity.R` | Age adjustment and PC2×age interaction; Supplementary Figure S12 |
| `11_study_flow.R` | Supplementary Figure S1 |
| `12_graphical_abstract.R` | Editable vector graphical abstract |

Some internal output filenames retain historical stage labels from the locked analysis
workflow. The manuscript mapping above, rather than historical filenames, defines the final
figure role.

## Data availability

Chinese cohort data are not publicly deposited because of participant privacy and
institutional data-governance requirements. Deidentified data may be available from the
corresponding author upon reasonable request, subject to institutional review and any
required data-use agreement.

NHANES 2011–2018 data are publicly available from the National Center for Health
Statistics.

## Code availability statement after public release

A manuscript-ready version is provided in `CODE_AVAILABILITY_TEXT.txt`. After creating a
GitHub release and Zenodo archive, replace the placeholders with the real repository URL and
DOI.

## Reproducibility notes

- Run scripts from the repository root.
- If running from another directory, set `CKM_REPO_ROOT` to the repository path.
- `R/99_capture_session_info.R` writes the actual package/session environment used locally.
- Fixed random seeds are retained in the analysis scripts.
- Do not upload private Chinese clinical data to this repository.
