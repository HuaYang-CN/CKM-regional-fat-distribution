# Figure and table production

The R stages exported historical analytical tables and plots before the manuscript was reordered. See the manuscript mapping in manifests/ and the CSVs in this directory. The five alcohol rows C07–C11 of current Table S27 are refitted by:

    Rscript publication/reproduce_alcohol_sensitivity.R results/alcohol_check

Run from the repository root with the corrected public input specified by CKM_NHANES_READY_FILE and the four official XPT files in CKM_NHANES_RAW_DIR. The dedicated entry reads full-precision loadings, fits survey models and writes observed results for Table 2 and S11/S12/S27. Counts at the end of the script are consistency checks.

RUN_ANALYSES.R instead executes the integrated model and analytical table stages with authorized Chinese inputs. Initialize a new output directory via CKM_OUTPUT_ROOT. Stage 01 and the corrected Stage 03 use the same revised alcohol definition.

The historical build_ijo_*.py scripts consume local analytical files from the restricted pipeline. export_locked_china_plot_objects.R produces participant-level Chinese paired scores exclusively for authorized local plotting. Keep that output outside all release trees. Set CKM_FONT_ROOT to a local directory of separately installed Liberation Sans TrueType files. The other Python publication stages also require case-sensitive intermediate filenames and the font resources specified by the scripts.

The November 18 panel mapping and recompose_panels.py expect the preceding publication PDFs in a local source_inputs/Figures/ tree and saved aggregate table inputs or the preceding supplementary Word file. Supply those types of input locally and choose fresh, distinct output directories. The PDFs and Chinese intermediate plot datasets are not in this repository. The reassembly alone produces the November 18 layout; the submitted S10 uses the subsequent equal-panel layout. The November 18 vector assembly alone is not an exact reproduction of that final S10.
