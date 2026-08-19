# 99_run_all.R
# Run the complete data and analysis pipeline in order.
# Output: all generated data, tables, figures, and logs documented by each stage

library(here)

stages <- c(
  "scripts/01_ffrisp_build.R",
  "scripts/02_luth_build.R",
  "scripts/08_protzko_profile.R",
  "scripts/09_protzko_build.R",
  "scripts/03_main_analysis.R",
  "scripts/04_sensitivity_analysis.R",
  "scripts/05_exploratory_moderators.R",
  "scripts/06_appendix_diagnostics.R",
  "scripts/07_open_ended_audit.R",
  "scripts/10_protzko_analysis.R",
  "scripts/11_archive_meta_analysis.R",
  "scripts/12_manipulation_meta_analysis.R",
  "scripts/13_family_synthesis.R"
)

# Build and audit stages need the restricted source files. Without them, the
# pipeline runs the analysis stages from the released respondent files
# committed to the repository.
stage_requirements <- list(
  "scripts/01_ffrisp_build.R" = "data/ffrisp/source/manifest.csv",
  "scripts/02_luth_build.R" = "data/luth/source/manifest.csv",
  "scripts/07_open_ended_audit.R" = "data/luth/source/open_ended/manifest.csv",
  "scripts/08_protzko_profile.R" = "data/protzko/source/manifest.csv",
  "scripts/09_protzko_build.R" = "data/protzko/source/manifest.csv"
)

# The manifests are committed; the restricted payloads they list are not.
source(here("scripts", "00_utils.R"))

for (stage in stages) {
  requirement <- stage_requirements[[stage]]
  if (!is.null(requirement) && !sources_present(here(requirement))) {
    message(
      "Skipping ", stage, ": restricted sources listed in ", requirement,
      " are not present; using the released respondent files."
    )
    next
  }
  message("Running: ", stage)
  sys.source(here(stage), envir = new.env(parent = globalenv()))
}

message("Pipeline complete.")
