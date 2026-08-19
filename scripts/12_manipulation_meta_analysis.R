# 12_manipulation_meta_analysis.R
# Estimate each manipulation's effect on the framing contrast as a
# within-survey change relative to the same survey's original wording, and
# synthesize the change across every survey that fielded the manipulation.
# Outputs: manipulation-effect data, appendix table, and macros under meta/

library(arrow)
library(dplyr)
library(here)
library(kableExtra)
library(knitr)
library(metafor)
library(readr)

source(here("scripts", "00_config.R"))
source(here("scripts", "00_utils.R"))

main_effects <- read_parquet(here("tabs", "main_effects.parquet"))
study_2018 <- read_parquet(here("tabs", "protzko_frame_effects.parquet"))

survey_contrasts <- bind_rows(
  main_effects |>
    transmute(
      survey = .data$sample,
      effect_id = .data$effect_id,
      family = .data$family,
      family_label = .data$family_label,
      variant = .data$variant,
      wording = .data$wording,
      display_order = .data$display_order,
      effect = .data$effect,
      std_error = .data$std_error
    ),
  study_2018 |>
    transmute(
      survey = .data$study,
      effect_id = .data$effect_id,
      family = .data$family,
      family_label = .data$family_label,
      variant = .data$variant,
      wording = .data$wording,
      display_order = .data$display_order,
      effect = .data$effect,
      std_error = .data$std_error
    )
)

originals <- survey_contrasts |>
  filter(.data$family == "original") |>
  select(
    survey,
    original_effect = effect,
    original_std_error = std_error
  )

# The manipulated and original arms are disjoint randomized groups within one
# survey, so the change-score variance is the sum of the two contrast
# variances. Change-scores from the same survey share that survey's original
# arm and are therefore correlated across manipulations; within each pooled
# row below, each survey contributes once, so the pooled inputs are
# independent.
manipulation_effects <- survey_contrasts |>
  filter(.data$family != "original") |>
  left_join(originals, by = "survey", relationship = "many-to-one") |>
  mutate(
    change = .data$effect - .data$original_effect,
    change_std_error = sqrt(.data$std_error^2 + .data$original_std_error^2),
    change_pp = 100 * .data$change,
    change_conf_low_pp = 100 * (.data$change - qnorm(0.975) * .data$change_std_error),
    change_conf_high_pp = 100 * (.data$change + qnorm(0.975) * .data$change_std_error)
  )

stopifnot(
  nrow(manipulation_effects) == 18L,
  !anyNA(manipulation_effects$change)
)

write_parquet(
  manipulation_effects,
  here("meta", "derived", "manipulation_effects.parquet")
)

synthesize_row <- function(group) {
  k <- nrow(group)
  if (k == 1L) {
    tibble(
      k = 1L,
      estimate_pp = group$change_pp,
      conf_low_pp = group$change_conf_low_pp,
      conf_high_pp = group$change_conf_high_pp,
      method = "Single within-survey difference"
    )
  } else {
    fit <- rma.uni(
      yi = group$change,
      sei = group$change_std_error,
      method = "REML",
      test = "knha"
    )
    tibble(
      k = k,
      estimate_pp = 100 * as.numeric(fit$b),
      conf_low_pp = 100 * fit$ci.lb,
      conf_high_pp = 100 * fit$ci.ub,
      method = "REML random effects"
    )
  }
}

# One row per (family, variant). The fully-stated Luth pair and the
# functionally described 2018 balanced factor carry different arm labels but
# implement the same manipulation, so the row label is set here explicitly.
row_labels <- tibble::tribble(
  ~family, ~variant, ~row_label,
  "risky_flip", NA, "Risky-outcome order reversed",
  "response_order", "response_only", "Response order reversed",
  "response_order", "with_risky_flip", "Risky-outcome and response order reversed",
  "balanced", "fully_stated", "Certain option fully stated (balanced)",
  "balanced", "components_reversed", "Certain option fully stated, order reversed",
  "balanced", "with_risky_flip", "Balanced certain option, risky-outcome order reversed",
  "numeric", "199", "Certain outcome: 199 saved",
  "numeric", "201", "Certain outcome: 201 saved",
  "rewording", NA, "Expanded drug wording"
)

manipulation_meta <- manipulation_effects |>
  group_by(.data$family, .data$family_label, .data$variant) |>
  group_modify(~ synthesize_row(.x)) |>
  ungroup() |>
  left_join(row_labels, by = c("family", "variant")) |>
  left_join(
    manipulation_effects |>
      group_by(.data$family, .data$variant) |>
      summarize(min_order = min(.data$display_order), .groups = "drop"),
    by = c("family", "variant")
  ) |>
  arrange(.data$min_order) |>
  select(-min_order)

stopifnot(!anyNA(manipulation_meta$row_label))

stopifnot(nrow(manipulation_meta) == 9L)

write_parquet(
  manipulation_meta,
  here("meta", "tables", "manipulation_meta.parquet")
)

manipulation_meta |>
  transmute(
    Manipulation = .data$row_label,
    Surveys = .data$k,
    `Change [95% CI]` = sprintf(
      "%.1f [%.1f, %.1f]",
      .data$estimate_pp,
      .data$conf_low_pp,
      .data$conf_high_pp
    )
  ) |>
  kable(
    format = "latex",
    booktabs = TRUE,
    align = c("l", "r", "r"),
    caption = "Manipulation effects on the framing contrast, synthesized across surveys",
    label = "manipulation-meta",
    linesep = ""
  ) |>
  kable_styling(position = "center", latex_options = "HOLD_position") |>
  footnote(
    general = paste(
      "Note: Each entry is the change in the gain-minus-loss framing contrast,",
      "in percentage points, relative to the same survey's original wording;",
      "both arms are randomized within the survey. Rows with several surveys",
      "pool the within-survey changes with a REML random-effects model and",
      "Knapp--Hartung inference; single-survey rows report the one available",
      "difference with a normal interval. Changes from the same survey share",
      "that survey's original-wording arm, so rows are not independent of one",
      "another; within each row, surveys are independent."
    ),
    footnote_as_chunk = TRUE,
    general_title = "",
    threeparttable = TRUE
  ) |>
  save_kable(here("meta", "tables", "manipulation_meta.tex"))

meta_row <- function(family_value, variant_value = NULL) {
  rows <- manipulation_meta |>
    filter(.data$family == family_value)
  if (!is.null(variant_value)) {
    rows <- filter(rows, .data$variant == variant_value)
  }
  stopifnot(nrow(rows) == 1L)
  rows
}

change_macros <- function(row, stem) {
  c(
    sprintf("\\newcommand{\\%sChangeEffect}{%.1f}", stem, row$estimate_pp),
    sprintf("\\newcommand{\\%sChangeLow}{%.1f}", stem, row$conf_low_pp),
    sprintf("\\newcommand{\\%sChangeHigh}{%.1f}", stem, row$conf_high_pp),
    sprintf("\\newcommand{\\%sChangeK}{%d}", stem, row$k)
  )
}

# Attenuation-oriented variants flip the sign so prose can say "reduced by X
# points" with a positive X and a correctly ordered interval.
attenuation_macros <- function(row, stem) {
  c(
    sprintf("\\newcommand{\\%sAttenuation}{%.1f}", stem, -row$estimate_pp),
    sprintf("\\newcommand{\\%sAttenuationLow}{%.1f}", stem, -row$conf_high_pp),
    sprintf("\\newcommand{\\%sAttenuationHigh}{%.1f}", stem, -row$conf_low_pp)
  )
}

macros <- c(
  change_macros(meta_row("risky_flip"), "Flip"),
  attenuation_macros(meta_row("balanced", "fully_stated"), "Balanced"),
  attenuation_macros(meta_row("balanced", "with_risky_flip"), "BalancedFlip"),
  change_macros(meta_row("balanced", "fully_stated"), "Balanced"),
  change_macros(meta_row("balanced", "with_risky_flip"), "BalancedFlip"),
  change_macros(meta_row("balanced", "components_reversed"), "BalancedReversed"),
  change_macros(meta_row("response_order", "response_only"), "ResponseOrder"),
  change_macros(meta_row("response_order", "with_risky_flip"), "RiskyResponse"),
  change_macros(meta_row("numeric", "199"), "Lower"),
  change_macros(meta_row("numeric", "201"), "Upper"),
  change_macros(meta_row("rewording"), "Reword")
)
write_lines(macros, here("meta", "tables", "manipulation_meta_macros.tex"))

message("Created: meta/derived/manipulation_effects.parquet")
message("Created: meta/tables/manipulation_meta.parquet")
message("Created: meta/tables/manipulation_meta.tex")
message("Created: meta/tables/manipulation_meta_macros.tex")
