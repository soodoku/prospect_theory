# 05_exploratory_moderators.R
# Estimate exploratory education and political-knowledge moderation.
# Outputs: moderator and education-moderation tables and macros in tabs/.

library(arrow)
library(dplyr)
library(estimatr)
library(here)
library(kableExtra)
library(knitr)
library(metafor)
library(readr)

source(here("scripts", "00_utils.R"))

education_levels <- c(
  "Less than high school",
  "High school",
  "Some college or associate degree",
  "Bachelor's degree or higher"
)

ffrisp <- read_parquet(here("data", "ffrisp", "ffrisp_respondent_2009.parquet")) |>
  filter(instrument_valid, !is.na(certain_choice)) |>
  mutate(education_index = as.integer(factor(education, levels = education_levels)) - 1L)

luth <- read_parquet(here("data", "luth", "luth_respondent_2009.parquet")) |>
  filter(attention_pass, instrument_valid, !is.na(certain_choice)) |>
  mutate(education_index = as.integer(factor(education, levels = education_levels)) - 1L)

archive_2018 <- read_parquet(here("data", "protzko", "protzko_respondent.parquet")) |>
  filter(analysis_complete) |>
  mutate(
    experiment = study,
    education_index = as.integer(education_index)
  )

# The registry factor codes classify every 2009 arm on the two factors the
# 2018 experiments randomized: FFRISP wording 2 and Luth wording 8 reverse the
# risky-option outcome order, and Luth wordings 6 and 7 fully state the
# certain option. The pooled model adjusts for those design features with the
# codes rather than treating all 2009 arms as original-format.
archive_all <- bind_rows(
  ffrisp |>
    transmute(
      experiment = "FFRISP probability panel",
      loss_frame,
      certain_choice,
      education_index,
      internal_flip = internal_flip_code,
      explicit_balance = explicit_balance_code
    ),
  luth |>
    transmute(
      experiment = "Luth volunteer panel",
      loss_frame,
      certain_choice,
      education_index,
      internal_flip = internal_flip_code,
      explicit_balance = explicit_balance_code
    ),
  archive_2018 |>
    transmute(
      experiment,
      loss_frame = 1L - gain_frame,
      certain_choice,
      education_index,
      internal_flip,
      explicit_balance
    )
)

estimate_moderation <- function(data, moderator, label, unit) {
  formula <- reformulate(
    c(
      "loss_frame * factor(wording_id)",
      moderator,
      paste0("loss_frame:", moderator)
    ),
    response = "certain_choice"
  )
  model <- lm_robust(formula, data = data, se_type = "HC2")
  term_name <- paste0("loss_frame:", moderator)
  term <- broom::tidy(model, conf.int = TRUE) |>
    filter(term == term_name)
  tibble(
    measure = label,
    unit = unit,
    n = nobs(model),
    interaction = -term$estimate,
    std_error = term$std.error,
    conf_low = -term$conf.high,
    conf_high = -term$conf.low,
    p_value = term$p.value
  )
}

estimate_adjusted_moderation <- function(data, label, unit, controls) {
  formula <- reformulate(
    c("loss_frame", "education_index", "loss_frame:education_index", controls),
    response = "certain_choice"
  )
  model <- lm_robust(formula, data = data, se_type = "HC2")
  term <- broom::tidy(model, conf.int = TRUE) |>
    filter(term == "loss_frame:education_index")
  tibble(
    measure = label,
    unit = unit,
    n = nobs(model),
    interaction = -term$estimate,
    std_error = term$std.error,
    conf_low = -term$conf.high,
    conf_high = -term$conf.low,
    p_value = term$p.value
  )
}

results <- bind_rows(
  estimate_moderation(
    ffrisp,
    "education_index",
    "FFRISP education",
    "one-category increase"
  ),
  estimate_moderation(
    luth,
    "education_index",
    "Luth education",
    "one-category increase"
  ),
  estimate_moderation(
    luth,
    "political_knowledge",
    "Luth political knowledge",
    "zero-to-one increase"
  ),
  estimate_adjusted_moderation(
    archive_2018 |> filter(!is.na(education_index)),
    "Three 2018 experiments pooled, education",
    "one-category increase",
    c("factor(experiment)", "loss_frame:factor(experiment)",
      "internal_flip", "explicit_balance",
      "loss_frame:internal_flip", "loss_frame:explicit_balance")
  ),
  estimate_adjusted_moderation(
    archive_all |> filter(!is.na(education_index)),
    "All five experiments, education",
    "one-category increase",
    c("factor(experiment)", "loss_frame:factor(experiment)",
      "internal_flip", "explicit_balance",
      "loss_frame:internal_flip", "loss_frame:explicit_balance")
  )
) |>
  mutate(
    holm_p_value = p.adjust(p_value, method = "holm"),
    across(c(interaction, std_error, conf_low, conf_high), ~ 100 * .x)
  )

write_parquet(results, here("tabs", "moderator_interactions.parquet"))

moderator_table <- results |>
  mutate(
    `Interaction [95% CI]` = sprintf(
      "%.1f [%.1f, %.1f]",
      interaction,
      conf_low,
      conf_high
    ),
    p = sprintf("%.3f", p_value),
    `Holm p` = sprintf("%.3f", holm_p_value)
  ) |>
  select(Measure = measure, Unit = unit, N = n, `Interaction [95% CI]`, p, `Holm p`)

moderator_table |>
  kable(
    format = "latex",
    booktabs = TRUE,
    align = c("l", "l", "r", "r", "r", "r"),
    caption = "Exploratory moderator interactions",
    label = "moderators",
    linesep = ""
  ) |>
  kable_styling(
    position = "center",
    font_size = 8,
    latex_options = "HOLD_position"
  ) |>
  footnote(
    general = paste(
      "Note: Positive interactions indicate a larger gain-minus-loss contrast at higher",
      "moderator values. Models include wording indicators and all wording-by-frame",
      "interactions. Political knowledge is a five-item factual battery, not an",
      "intelligence measure. These analyses are exploratory."
    ),
    footnote_as_chunk = TRUE,
    general_title = "",
    threeparttable = TRUE
  ) |>
  save_kable(here("tabs", "moderator_interactions.tex"))

# Per-experiment education interactions and their random-effects synthesis.
# Each experiment is estimated on its own, adjusting for its own randomized
# design factors, and the five interaction coefficients are then pooled with
# a REML random-effects model. All quantities are exploratory.
study_levels <- c(
  "Stanford discovery", "Stanford replication", "UCSB replication"
)
education_by_experiment <- bind_rows(
  estimate_moderation(
    ffrisp,
    "education_index",
    "FFRISP probability panel",
    "one-category increase"
  ),
  estimate_moderation(
    luth,
    "education_index",
    "Luth volunteer panel",
    "one-category increase"
  ),
  purrr::map_dfr(study_levels, function(study_name) {
    estimate_adjusted_moderation(
      archive_2018 |>
        filter(experiment == study_name, !is.na(education_index)),
      study_name,
      "one-category increase",
      c("internal_flip", "explicit_balance",
        "loss_frame:internal_flip", "loss_frame:explicit_balance")
    )
  })
) |>
  mutate(across(c(interaction, std_error, conf_low, conf_high), ~ 100 * .x))

education_meta <- rma.uni(
  yi = interaction,
  sei = std_error,
  data = education_by_experiment,
  method = "REML",
  test = "knha"
)
education_prediction <- predict(education_meta)

education_table_data <- education_by_experiment |>
  transmute(
    measure = .data$measure,
    n = .data$n,
    interaction = .data$interaction,
    conf_low = .data$conf_low,
    conf_high = .data$conf_high
  ) |>
  bind_rows(
    tibble(
      measure = "Random-effects synthesis",
      n = sum(education_by_experiment$n),
      interaction = as.numeric(education_meta$b),
      conf_low = education_meta$ci.lb,
      conf_high = education_meta$ci.ub
    )
  )

write_parquet(education_table_data, here("tabs", "education_meta.parquet"))

education_table_data |>
  mutate(
    `Interaction [95% CI]` = sprintf(
      "%.1f [%.1f, %.1f]",
      interaction,
      conf_low,
      conf_high
    ),
    N = format(n, big.mark = ",")
  ) |>
  select(Experiment = measure, N, `Interaction [95% CI]`) |>
  kable(
    format = "latex",
    booktabs = TRUE,
    align = c("l", "r", "r"),
    caption = "Education moderation of the framing contrast, by experiment",
    label = "education-meta",
    linesep = ""
  ) |>
  kable_styling(position = "center", latex_options = "HOLD_position") |>
  row_spec(nrow(education_table_data) - 1L, hline_after = TRUE) |>
  footnote(
    general = paste(
      "Note: Entries are the change in the gain-minus-loss framing contrast,",
      "in percentage points, per one-category increase in education, from",
      "linear probability models with HC2 standard errors that adjust for each",
      "experiment's randomized wording or design factors. The synthesis row",
      "pools the five interaction coefficients with a REML random-effects",
      "model and Knapp--Hartung inference. Education is an imperfect proxy for",
      "reasoning ability, and all rows are exploratory."
    ),
    footnote_as_chunk = TRUE,
    general_title = "",
    threeparttable = TRUE
  ) |>
  save_kable(here("tabs", "education_meta.tex"))

moderator_macros <- sprintf(
  c("\\newcommand{\\ModeratorMinimumHolmP}{%.3f}"),
  min(results$holm_p_value)
)
moderator_macros <- c(
  moderator_macros,
  sprintf("\\newcommand{\\EducationMetaK}{%d}", education_meta$k),
  sprintf("\\newcommand{\\EducationMetaEffect}{%.1f}", as.numeric(education_meta$b)),
  sprintf("\\newcommand{\\EducationMetaLow}{%.1f}", education_meta$ci.lb),
  sprintf("\\newcommand{\\EducationMetaHigh}{%.1f}", education_meta$ci.ub),
  sprintf("\\newcommand{\\EducationMetaP}{%.3f}", education_meta$pval),
  sprintf("\\newcommand{\\EducationMetaISquared}{%.1f}", education_meta$I2),
  sprintf("\\newcommand{\\EducationMetaPredLow}{%.1f}", education_prediction$pi.lb),
  sprintf("\\newcommand{\\EducationMetaPredHigh}{%.1f}", education_prediction$pi.ub)
)
moderator_macros <- c(
  moderator_macros,
  sprintf(
    "\\newcommand{\\ModeratorArchiveInteraction}{%.1f}",
    results$interaction[results$measure == "Three 2018 experiments pooled, education"]
  ),
  sprintf(
    "\\newcommand{\\ModeratorArchiveLow}{%.1f}",
    results$conf_low[results$measure == "Three 2018 experiments pooled, education"]
  ),
  sprintf(
    "\\newcommand{\\ModeratorArchiveHigh}{%.1f}",
    results$conf_high[results$measure == "Three 2018 experiments pooled, education"]
  ),
  sprintf(
    "\\newcommand{\\ModeratorAllInteraction}{%.1f}",
    results$interaction[results$measure == "All five experiments, education"]
  ),
  sprintf(
    "\\newcommand{\\ModeratorAllLow}{%.1f}",
    results$conf_low[results$measure == "All five experiments, education"]
  ),
  sprintf(
    "\\newcommand{\\ModeratorAllHigh}{%.1f}",
    results$conf_high[results$measure == "All five experiments, education"]
  )
)
writeLines(moderator_macros, here("tabs", "moderator_macros.tex"))

message("Created: tabs/education_meta.parquet")
message("Created: tabs/education_meta.tex")
message("Created: tabs/moderator_interactions.parquet")
message("Created: tabs/moderator_interactions.tex")
message("Created: tabs/moderator_macros.tex")
