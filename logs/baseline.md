# Reproducibility baseline

The inherited project had no test or build target. On 2026-08-17, each available script and the most complete legacy TeX file were run from the repository root.

| Artifact | Result | First failure |
|---|---|---|
| `01_ffrisp_recode.R` | failed | undefined `clean()` |
| `02_luth_recode.R` | failed | missing `janitor` package |
| `03_cps_recode.R` | failed | referenced nonexistent `data/CPS/2009/cps.rdata` |
| `04_tabs_prospect.R` | failed | incompatible list input in `meanf()` |
| `tversky_kahneman.tex` | failed | obsolete, unavailable `apa.cls` |

The full replacement pipeline is defined by `scripts/99_run_all.R`.
