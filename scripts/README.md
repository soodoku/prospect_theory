# Active analysis pipeline

The active pipeline is the numbered sequence run by `scripts/99_run_all.R`. Do not edit generated tables or figures by hand.

| Stage | Purpose | Principal outputs |
|---|---|---|
| `00_config.R`, `00_utils.R` | Shared paths, the canonical arm registry (condition labels, manipulation families, per-arm factor codes), validation helpers, and publication style | Shared functions and constants |
| `01_ffrisp_build.R` | Build the restricted FFRISP respondent file | `data/ffrisp/ffrisp_respondent_2009.parquet` |
| `02_luth_build.R` | Build the restricted Luth respondent file | `data/luth/luth_respondent_2009.parquet` |
| `08_protzko_profile.R`, `09_protzko_build.R` | Profile and build the three 2018 study exports, cross-checking derived factors against the recorded Qualtrics block labels | `data/protzko/protzko_respondent.parquet` and source-flow artifacts |
| `03_main_analysis.R` | Estimate 2009 wording contrasts | Per-survey effects table, macros, and the invariance-profile figure |
| `04_sensitivity_analysis.R`, `05_exploratory_moderators.R` | Produce planned sensitivities, exploratory moderation, and the per-experiment education-moderation synthesis | Sensitivity, moderator, and education-moderation tables and macros |
| `06_appendix_diagnostics.R`, `07_open_ended_audit.R` | Generate sample-flow, wording-inventory, verbatim-wording, and coding-audit exhibits | Appendix tables |
| `10_protzko_analysis.R` | Estimate the 2018 factorial contrasts | Pooled 2018 cell table, supporting data, and factorial figure |
| `11_archive_meta_analysis.R` | Synthesize all valid archive contrasts | Archive synthesis table and forest plot |
| `12_manipulation_meta_analysis.R` | Synthesize each manipulation's within-survey change against the original wording across all surveys that fielded it | Manipulation-effect table and macros |
| `13_family_synthesis.R` | Assemble the experiment-organized main exhibit | `tabs/family_effects.{parquet,tex}` and `figs/family_effects.pdf` |

Generated data artifacts are written as Parquet (type-safe) rather than CSV; hand-curated inputs (`data/ffrisp/fieldwork.csv`, `data/wording/arm_wording.csv`) remain CSV and are read with explicit column types.

Run `make check` after restoring `renv`; it rebuilds the pipeline, runs tests and linting, and compiles the manuscript. Historical scripts are retained privately by the authors and are not part of this build.
