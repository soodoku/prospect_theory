# 04_sensitivity_analysis.R
# Test effect heterogeneity and estimate prespecified sample sensitivities.
# Outputs: heterogeneity, sensitivity, flow, and diagnostic files in tabs/.

library(arrow)
library(dplyr)
library(estimatr)
library(here)
library(kableExtra)
library(knitr)
library(readr)
library(survey)

source(here("scripts", "00_utils.R"))

ffrisp_path <- here("data", "ffrisp", "ffrisp_respondent_2009.parquet")
luth_path <- here("data", "luth", "luth_respondent_2009.parquet")
protzko_path <- here("data", "protzko", "protzko_respondent.parquet")
purrr::walk(c(ffrisp_path, luth_path, protzko_path), assert_file)

ffrisp <- read_parquet(ffrisp_path)
luth <- read_parquet(luth_path)
archive_2018 <- read_parquet(protzko_path)

ffrisp_primary <- ffrisp |>
  filter(instrument_valid, !is.na(certain_choice))
luth_primary <- luth |>
  filter(attention_pass, instrument_valid, !is.na(certain_choice))
luth_completed <- luth |>
  filter(instrument_valid, !is.na(certain_choice))

heterogeneity_test <- function(data, sample_name) {
  model <- lm_robust(
    certain_choice ~ loss_frame * factor(wording_id),
    data = data,
    se_type = "HC2"
  )
  interaction_terms <- grep(":", names(coef(model)), value = TRUE)
  test <- car::linearHypothesis(model, interaction_terms, test = "F") |>
    as.data.frame() |>
    slice_tail(n = 1L)
  tibble(
    sample = sample_name,
    n = nrow(data),
    numerator_df = test$Df,
    denominator_df = test$Res.Df,
    statistic = test$F,
    p_value = test[["Pr(>F)"]]
  )
}

heterogeneity <- bind_rows(
  heterogeneity_test(ffrisp_primary, "FFRISP probability panel"),
  heterogeneity_test(luth_primary, "Luth volunteer panel")
)
write_parquet(heterogeneity, here("tabs", "heterogeneity_tests.parquet"))

estimate_unweighted <- function(data, label) {
  model <- lm_robust(certain_choice ~ loss_frame, data = data, se_type = "HC2")
  term <- broom::tidy(model, conf.int = TRUE) |>
    filter(term == "loss_frame")
  tibble(
    specification = label,
    n = nrow(data),
    effect = -term$estimate,
    std_error = term$std.error,
    conf_low = -term$conf.high,
    conf_high = -term$conf.low
  )
}

old_lonely_psu <- getOption("survey.lonely.psu")
options(survey.lonely.psu = "adjust")
ffrisp_design <- ffrisp_primary |>
  filter(!is.na(survey_weight)) |>
  svydesign(
    ids = ~psu,
    strata = ~stratum,
    weights = ~survey_weight,
    data = _,
    nest = TRUE
  )
weighted_model <- svyglm(
  certain_choice ~ loss_frame,
  design = subset(ffrisp_design, wording_id == 1L)
)
weighted_interval <- confint(weighted_model)["loss_frame", ]
options(survey.lonely.psu = old_lonely_psu)

sensitivity <- bind_rows(
  estimate_unweighted(
    filter(ffrisp_primary, wording_id == 1L),
    "FFRISP original wording, unweighted"
  ),
  tibble(
    specification = "FFRISP original wording, survey weighted",
    n = nobs(weighted_model),
    effect = -coef(weighted_model)["loss_frame"],
    std_error = sqrt(vcov(weighted_model)["loss_frame", "loss_frame"]),
    conf_low = -weighted_interval[[2]],
    conf_high = -weighted_interval[[1]]
  ),
  estimate_unweighted(
    filter(luth_primary, wording_id == 1L),
    "Luth original wording, attention check passed"
  ),
  estimate_unweighted(
    filter(luth_completed, wording_id == 1L),
    "Luth original wording, all completed respondents"
  )
) |>
  mutate(across(c(effect, std_error, conf_low, conf_high), ~ 100 * .x))
write_parquet(sensitivity, here("tabs", "sensitivity_effects.parquet"))

sensitivity_table <- sensitivity |>
  mutate(
    `Difference [95% CI]` = sprintf(
      "%.1f [%.1f, %.1f]",
      effect,
      conf_low,
      conf_high
    )
  ) |>
  select(Specification = specification, N = n, `Difference [95% CI]`)

sensitivity_table |>
  kable(
    format = "latex",
    booktabs = TRUE,
    align = c("l", "r", "r"),
    caption = "Original-wording sensitivity analyses",
    label = "sensitivity",
    linesep = ""
  ) |>
  kable_styling(position = "center", latex_options = "HOLD_position") |>
  footnote(
    general = paste(
      "Note: Differences are gain-frame minus loss-frame percentage points for the",
      "original wording. The weighted FFRISP interval uses its stratified,",
      "clustered survey design; other intervals use HC2 standard errors."
    ),
    footnote_as_chunk = TRUE,
    general_title = "",
    threeparttable = TRUE
  ) |>
  save_kable(here("tabs", "sensitivity_effects.tex"))

ffrisp_heterogeneity <- heterogeneity |>
  filter(sample == "FFRISP probability panel") |>
  slice(1)
luth_heterogeneity <- heterogeneity |>
  filter(sample == "Luth volunteer panel") |>
  slice(1)
diagnostic_macros <- c(
  sprintf("\\newcommand{\\FFRHeterogeneityF}{%.2f}", ffrisp_heterogeneity$statistic),
  sprintf("\\newcommand{\\FFRHeterogeneityP}{%.3f}", ffrisp_heterogeneity$p_value),
  sprintf("\\newcommand{\\LuthHeterogeneityF}{%.2f}", luth_heterogeneity$statistic),
  sprintf("\\newcommand{\\LuthHeterogeneityP}{%.3f}", luth_heterogeneity$p_value)
)
writeLines(diagnostic_macros, here("tabs", "diagnostic_macros.tex"))

sample_flow <- tibble(
  sample = c(
    "FFRISP", "FFRISP", "FFRISP", "Luth", "Luth", "Luth", "Luth", "Luth",
    "Stanford discovery", "Stanford discovery", "Stanford replication",
    "Stanford replication", "UCSB replication", "UCSB replication"
  ),
  stage = c(
    "Source export (starting N)", "After valid panel-ID restriction",
    "Final analytic sample (nonmissing outcome)",
    "Source export (starting N)", "After attention-check restriction",
    "After valid-instrument restriction", "Final analytic sample (nonmissing outcome)",
    "Sensitivity: all completed, valid outcome",
    "Source export (starting N)", "Final analytic sample (nonmissing first choice)",
    "Source export (starting N)", "Final analytic sample (nonmissing first choice)",
    "Source export (starting N)", "Final analytic sample (nonmissing choice)"
  ),
  n = c(
    902L, nrow(ffrisp), nrow(ffrisp_primary), nrow(luth),
    sum(luth$attention_pass),
    sum(luth$attention_pass & luth$instrument_valid), nrow(luth_primary), nrow(luth_completed),
    sum(archive_2018$study == "Stanford discovery"),
    sum(archive_2018$study == "Stanford discovery" & archive_2018$analysis_complete),
    sum(archive_2018$study == "Stanford replication"),
    sum(archive_2018$study == "Stanford replication" & archive_2018$analysis_complete),
    sum(archive_2018$study == "UCSB replication"),
    sum(archive_2018$study == "UCSB replication" & archive_2018$analysis_complete)
  )
)
write_parquet(sample_flow, here("tabs", "sample_flow.parquet"))

message("Created: tabs/heterogeneity_tests.parquet")
message("Created: tabs/sensitivity_effects.parquet")
message("Created: tabs/sensitivity_effects.tex")
message("Created: tabs/diagnostic_macros.tex")
message("Created: tabs/sample_flow.parquet")
