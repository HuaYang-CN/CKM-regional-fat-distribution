# Workflow boundaries

The independent public entry point concerns the US NHANES component. It applies a Chinese loading matrix to standardized survey inputs. It does not derive those coefficients anew. Successful US execution therefore tests transport and association estimates given the supplied aggregate parameters.

The Chinese derivation included 955 participants. Its analysis inputs are controlled. The 100-person repeated-measures cohort was selected according to observed stage transitions and had no comparison group remaining in stage 2 at both visits. Raw clinical source-population construction, the individual staging review and clinical data extraction cannot be reconstructed from this release. Approved researchers need the retained restricted clinical inputs to rerun downstream analyses.

Clinical nested cross-validation, reclassification, DCA, SHAP and individual trajectories require restricted inputs or authorized intermediate objects. Figure-numbering manifests are provenance, not computed evidence of model performance. Historical analysis or supplementary-table numbers in the R code can differ from the manuscript. Table S27 is a compiled statistical inventory; a successful public survey run does not regenerate every clinical row.

The final S10 plot can be rendered with Python (reportlab, pypdfium2 and Pillow) from approved aggregate exports. Supply two licensed font files for the regular and bold faces. Example commands, with all outputs going to new directories:

~~~sh
Rscript scripts/export_s10_aggregates.R /authorized/master.rds /work/new-s10-aggregates
python scripts/render_s10.py /work/new-s10-aggregates /work/new-s10-figure /fonts/regular.ttf /fonts/bold.ttf
~~~

Historic v1.0.2 public preparation scripts are preserved for provenance as marked in the analysis manifest. Use the current public entry point for manuscript comparisons. See [execution coverage](REPRODUCIBILITY.md) for the actual tests performed in this candidate.
