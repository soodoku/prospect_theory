# Data layout

Each source has an immutable `source/` directory. The pipeline reads those files but never changes them. Analysis-ready Parquet files are generated beside `source/` and are not hand edited.

Raw survey files contain respondent identifiers and open text and are excluded from Git. Each `source/manifest.csv` records the required filenames, byte counts, and SHA-256 checksums. The build stops if any listed source is missing or differs from its manifest entry.

The respondent Parquet files are public: the builds minimize them to the columns the analysis consumes, replace the FFRISP design codes with anonymized dense ranks, and assign deterministic sequential `case_id` values that reproduce from the sources without linking back to any panel identifier. `data/data-dictionary.md` documents the released schema and the minimization policy; `tests/testthat/test-data-contracts.R` enforces it. The data files are released under CC-BY 4.0 (see `LICENSE-DATA`).

`protzko/source/` contains the four received private OSF Stata files. Its
manifest fixes their identities. The pipeline builds a deidentified respondent
file, reconstructs the received anonymous stack, and documents its one blank
row and one missing-to-zero discrepancy. The manuscript, original scripts, and
study documentation remain outstanding.

`wording/arm_wording.csv` is a hand-curated, tracked record of the treatment
text for every wording pair and 2018 design cell, graded verbatim,
reconstructed, or functional by its `verbatim_status` column and citing its
source document. `scripts/06_appendix_diagnostics.R` joins it to the arm
registry in `scripts/00_config.R` to generate the appendix wording tables, so
condition labels cannot drift from the results exhibits.
