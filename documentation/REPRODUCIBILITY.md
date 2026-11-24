# Reproducing the public NHANES analysis

## Environment

Use R 4.5.1 with **preventr 0.12.0** for the PREVENT implementation used here and **survey 4.4-2** for the complex-sample regressions. The construction also uses haven, dplyr (at least 1.1.4), tidyr, purrr, stringr, tibble and readr. The public entry point checks for its packages. The historical environments for clinical results and publications are recorded in sessionInfo/. These are recorded versions rather than a platform-independent lockfile. Users can install a required archived R package version in a dedicated library using CRAN or its official archive before running the analysis.

## Commands

From the repository root, use Rscript on PATH and replace the example locations with writable directories. Keep each results directory new.

~~~sh
Rscript scripts/public_nhanes.R /work/nhanes-cache /work/ckm-run-1
Rscript scripts/public_nhanes.R /work/nhanes-cache /work/ckm-run-2 --offline
python scripts/public_verify.py /work/ckm-run-2 /work/verification.tsv
~~~

The first command retrieves the official CDC XPT releases missing from the cache and checks the locked input sizes and checksums. An existing file whose fingerprint differs causes a stop. The second command requires all files locally. CDC may revise a public release. If its checksum changes, investigate the version and acquisition record before analysis. Use a new verification report path.

## What the commands calculate

1. Harmonize cycle-specific public data, build survey-design identifiers and fasting weights, apply the eligibility and staging functions, and construct the seven adiposity inputs. See manifests/public_cdc_inputs.tsv for the specific component releases.
2. Apply the Chinese loading coefficients to the within-NHANES standardized inputs, calculate the independent NHANES scores, and scale scores for survey effects by their survey-weighted standard deviations. The first component represents overall adiposity in the study.
3. Fit the primary, age-adjusted, stage-restricted and alcohol-complete models in the documented survey designs; also fit the locally derived score and leave-one-cycle-out analyses. Compute the aggregate outputs first. The separate Python comparison reads documented reference values solely to check observed results.

Results are written as flow.tsv (selection counts), stage_counts.tsv (stage distribution), models.tsv (counts, effect estimates, uncertainty, p values and design information), diagnostics.tsv (score/harmonization checks) and sessionInfo.txt. Inspect the design and residual degrees of freedom when matching archive table rows. The full-sample survey and the alcohol-complete models have different denominators.

## Restricted and display analyses

For the clinical analysis see [Data access](DATA_ACCESS.md), [Restricted inputs](RESTRICTED_INPUTS.md), the restricted runner RUN_ANALYSES.R, and [the publication-stage README](../publication/README.md). The public survey command does not require these clinical inputs. Current supplement numbering is documented in [the output map](../manifests/output_to_manuscript_map.tsv), including the limited reconstruction evidence for Table S27.

The final S10 drawing script was exercised on the retained aggregate curves, deciles and calibration coefficients. Its raster matched the current Word figure pixel for pixel when rendered with the same fonts. No clinical participant-level fitting was repeated in that graphics check.

## Validation performed on the release candidate

All 72 official CDC files were freshly acquired and checked against the input manifest. The raw reconstruction, PREVENT-based staging, complete-case selection, fixed-score application, local PCA and survey regressions were run using only public NHANES inputs and aggregate loadings. The tested command used the verified cache in offline mode. The resulting sample contained 2,688 participants with 179 advanced cases. Stage counts from 0 through 4 were 235, 741, 1,533, 18 and 161.

Thirteen survey models were fitted in the public run. Fixed-score PC2 odds ratios were 0.56 (95% CI 0.44–0.72) for the primary association, 0.63 (0.49–0.83) with age adjustment, and 0.75 (0.58–0.97) for stages 3–4 versus stage 2. Effects use one full-survey weighted standard deviation. The alcohol-complete analysis included 2,262 participants and its stage 2–4 restriction included 1,445, each with 150 advanced cases. The 2017–2018 cycle contributed 51 eligible respondents with ALQ111 = 2 and a skipped ALQ121 in the final analytic sample. Only the tested public modules are covered by these results. The full clinical pipeline, NHANES secondary apparent-performance displays, all interaction/spline analyses and every manuscript table were not executed by the public command.
