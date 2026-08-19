# 10_protzko_analysis.R
# Estimate the Protzko frame, internal-flip, and explicit-balancing contrasts.
# Outputs: Protzko tables, macros, and study-specific effect figure

library(arrow)
library(broom)
library(dplyr)
library(estimatr)
library(ggplot2)
library(here)
library(kableExtra)
library(knitr)
library(readr)

source(here("scripts", "00_config.R"))
source(here("scripts", "00_utils.R"))

data_path <- here("data", "protzko", "protzko_respondent.parquet")
assert_file(data_path)

analysis <- read_parquet(data_path) |>
  filter(.data$analysis_complete) |>
  mutate(
    flip_label = if_else(
      .data$internal_flip == 1L,
      "Internal flip",
      "Original internal order"
    ),
    explicit_label = if_else(
      .data$explicit_balance == 1L,
      "Balanced certain option",
      "Unbalanced certain option"
    ),
    design = .data$wording
  )

assert_rows(analysis, 4585L, "Protzko complete-case analysis")

source_flow <- read_parquet(
  here("tabs", "protzko_source_flow.parquet")
)
source_flow |>
  transmute(
    Study = .data$study,
    `Source rows` = format(.data$source_rows, big.mark = ","),
    `Missing first choice` = .data$missing_choice,
    `Analysis rows` = format(.data$analysis_rows, big.mark = ",")
  ) |>
  kable(
    format = "latex",
    booktabs = TRUE,
    align = c("l", "r", "r", "r"),
    caption = "2018 factorial experiment source-to-analysis flow",
    label = "protzko-flow",
    linesep = ""
  ) |>
  kable_styling(position = "center", latex_options = "HOLD_position") |>
  footnote(
    general = paste(
      "Note: Stanford rows contribute the first framing response only.",
      "The received anonymous combined file adds a blank row and changes one",
      "source-missing first choice to zero; neither change enters the analysis."
    ),
    footnote_as_chunk = TRUE,
    general_title = "",
    threeparttable = TRUE
  ) |>
  save_kable(here("tabs", "protzko_source_flow.tex"))

estimate_frame_effect <- function(data, adjust_study = FALSE) {
  formula <- if (adjust_study) {
    certain_choice ~ factor(study) + gain_frame
  } else {
    certain_choice ~ gain_frame
  }
  model <- lm_robust(formula, data = data, se_type = "HC2")
  estimate <- tidy(model, conf.int = TRUE) |>
    filter(.data$term == "gain_frame")
  tibble(
    n_gain = sum(data$gain_frame == 1L),
    n_loss = sum(data$gain_frame == 0L),
    gain_mean = mean(data$certain_choice[data$gain_frame == 1L]),
    loss_mean = mean(data$certain_choice[data$gain_frame == 0L]),
    effect = estimate$estimate,
    std_error = estimate$std.error,
    conf_low = estimate$conf.low,
    conf_high = estimate$conf.high,
    p_value = estimate$p.value
  )
}

cell_means <- analysis |>
  group_by(
    .data$study,
    .data$explicit_balance,
    .data$explicit_label,
    .data$internal_flip,
    .data$flip_label,
    .data$gain_frame
  ) |>
  summarize(
    n = n(),
    certain_mean = mean(.data$certain_choice),
    std_error = sd(.data$certain_choice) / sqrt(n()),
    conf_low = pmax(0, .data$certain_mean - qt(0.975, n() - 1L) * .data$std_error),
    conf_high = pmin(1, .data$certain_mean + qt(0.975, n() - 1L) * .data$std_error),
    .groups = "drop"
  )
write_parquet(cell_means, here("tabs", "protzko_cell_means.parquet"))

study_effects <- analysis |>
  group_by(
    .data$study,
    .data$effect_id,
    .data$explicit_balance,
    .data$explicit_label,
    .data$internal_flip,
    .data$flip_label,
    .data$design,
    .data$family,
    .data$family_label,
    .data$variant,
    .data$display_order
  ) |>
  group_modify(~ estimate_frame_effect(.x)) |>
  ungroup() |>
  mutate(
    wording = .data$design,
    estimate_pp = 100 * .data$effect,
    conf_low_pp = 100 * .data$conf_low,
    conf_high_pp = 100 * .data$conf_high
  )
write_parquet(study_effects, here("tabs", "protzko_frame_effects.parquet"))

pooled_effects <- analysis |>
  group_by(
    .data$explicit_balance,
    .data$explicit_label,
    .data$internal_flip,
    .data$flip_label,
    .data$design,
    .data$family,
    .data$family_label,
    .data$variant,
    .data$display_order
  ) |>
  group_modify(~ estimate_frame_effect(.x, adjust_study = TRUE)) |>
  ungroup() |>
  mutate(
    wording = .data$design,
    estimate_pp = 100 * .data$effect,
    conf_low_pp = 100 * .data$conf_low,
    conf_high_pp = 100 * .data$conf_high
  )
write_parquet(pooled_effects, here("tabs", "protzko_pooled_effects.parquet"))

factorial_model <- lm_robust(
  certain_choice ~ factor(study) +
    gain_frame * internal_flip * explicit_balance,
  data = analysis,
  se_type = "HC2"
)
factorial_terms <- tidy(factorial_model, conf.int = TRUE) |>
  filter(
    .data$term %in% c(
      "gain_frame:internal_flip",
      "gain_frame:explicit_balance",
      "gain_frame:internal_flip:explicit_balance"
    )
  ) |>
  mutate(
    estimate_pp = 100 * .data$estimate,
    conf_low_pp = 100 * .data$conf.low,
    conf_high_pp = 100 * .data$conf.high
  )
write_parquet(factorial_terms, here("tabs", "protzko_factorial_terms.parquet"))

pooled_table <- pooled_effects |>
  transmute(
    `Certain option` = if_else(
      .data$explicit_balance == 1L,
      "Balanced",
      "Unbalanced"
    ),
    `Internal order` = .data$flip_label,
    `Gain frame` = sprintf("%.1f%%", 100 * .data$gain_mean),
    `Loss frame` = sprintf("%.1f%%", 100 * .data$loss_mean),
    `Difference [95% CI]` = sprintf(
      "%.1f [%.1f, %.1f]",
      .data$estimate_pp,
      .data$conf_low_pp,
      .data$conf_high_pp
    )
  )

pooled_table |>
  kable(
    format = "latex",
    booktabs = TRUE,
    align = c("l", "l", "r", "r", "r"),
    caption = "2018 Stanford/UCSB factorial experiments: certain choice by framing condition",
    label = "protzko-effects",
    linesep = ""
  ) |>
  kable_styling(position = "center", latex_options = "HOLD_position") |>
  footnote(
    general = paste(
      "Note: The three source exports contribute 4,585 respondents with valid",
      "first choices. Differences are gain-frame minus loss-frame percentage",
      "points from linear probability models with study indicators and HC2",
      "standard errors. Cell percentages are unadjusted. In balanced conditions,",
      "the certain option stated both outcomes."
    ),
    footnote_as_chunk = TRUE,
    general_title = "",
    threeparttable = TRUE
  ) |>
  save_kable(here("tabs", "protzko_effects.tex"))

explicit_interaction <- factorial_terms |>
  filter(.data$term == "gain_frame:explicit_balance")
flip_interaction <- factorial_terms |>
  filter(.data$term == "gain_frame:internal_flip")
three_way <- factorial_terms |>
  filter(.data$term == "gain_frame:internal_flip:explicit_balance")
format_p <- function(p_value) {
  if (p_value < 0.001) "<.001" else sprintf("%.3f", p_value)
}

cell_condition <- function(data, cell_slug) {
  switch(cell_slug,
    original = data$explicit_balance == 0L & data$internal_flip == 0L,
    flip = data$explicit_balance == 0L & data$internal_flip == 1L,
    balanced = data$explicit_balance == 1L & data$internal_flip == 0L,
    balancedflip = data$explicit_balance == 1L & data$internal_flip == 1L
  )
}

pooled_macro <- function(cell_slug, macro_stem) {
  row <- pooled_effects |>
    filter(.data$design_slug == paste0("cell_", cell_slug)) |>
    slice(1)
  per_study <- study_effects |>
    filter(cell_condition(study_effects, cell_slug))
  c(
    sprintf("\\newcommand{\\Protzko%sContrast}{%.1f}", macro_stem, row$estimate_pp),
    sprintf("\\newcommand{\\Protzko%sLow}{%.1f}", macro_stem, row$conf_low_pp),
    sprintf("\\newcommand{\\Protzko%sHigh}{%.1f}", macro_stem, row$conf_high_pp),
    sprintf(
      "\\newcommand{\\Protzko%sMin}{%.1f}", macro_stem, min(per_study$estimate_pp)
    ),
    sprintf(
      "\\newcommand{\\Protzko%sMax}{%.1f}", macro_stem, max(per_study$estimate_pp)
    )
  )
}

pooled_effects <- pooled_effects |>
  mutate(
    design_slug = paste0(
      "cell_",
      case_when(
        .data$explicit_balance == 0L & .data$internal_flip == 0L ~ "original",
        .data$explicit_balance == 0L & .data$internal_flip == 1L ~ "flip",
        .data$explicit_balance == 1L & .data$internal_flip == 0L ~ "balanced",
        .default = "balancedflip"
      )
    )
  )

macros <- c(
  sprintf("\\newcommand{\\ProtzkoN}{%s}", format(nrow(analysis), big.mark = ",")),
  pooled_macro("original", "Original"),
  pooled_macro("flip", "Flip"),
  pooled_macro("balanced", "Balanced"),
  pooled_macro("balancedflip", "BalancedFlip"),
  sprintf("\\newcommand{\\ProtzkoExplicitInteraction}{%.1f}", explicit_interaction$estimate_pp),
  sprintf("\\newcommand{\\ProtzkoExplicitLow}{%.1f}", explicit_interaction$conf_low_pp),
  sprintf("\\newcommand{\\ProtzkoExplicitHigh}{%.1f}", explicit_interaction$conf_high_pp),
  sprintf(
    "\\newcommand{\\ProtzkoExplicitAttenuation}{%.1f}",
    -explicit_interaction$estimate_pp
  ),
  sprintf(
    "\\newcommand{\\ProtzkoExplicitAttenuationLow}{%.1f}",
    -explicit_interaction$conf_high_pp
  ),
  sprintf(
    "\\newcommand{\\ProtzkoExplicitAttenuationHigh}{%.1f}",
    -explicit_interaction$conf_low_pp
  ),
  sprintf("\\newcommand{\\ProtzkoExplicitP}{%s}", format_p(explicit_interaction$p.value)),
  sprintf("\\newcommand{\\ProtzkoFlipInteraction}{%.1f}", flip_interaction$estimate_pp),
  sprintf("\\newcommand{\\ProtzkoFlipP}{%s}", format_p(flip_interaction$p.value)),
  sprintf("\\newcommand{\\ProtzkoThreeWay}{%.1f}", three_way$estimate_pp),
  sprintf("\\newcommand{\\ProtzkoThreeWayP}{%s}", format_p(three_way$p.value))
)
write_lines(macros, here("tabs", "protzko_macros.tex"))

design_levels <- study_effects |>
  distinct(.data$design, .data$display_order) |>
  arrange(.data$display_order) |>
  pull(.data$design)

study_effects <- study_effects |>
  mutate(
    design = factor(.data$design, levels = rev(design_levels)),
    study = factor(
      .data$study,
      levels = c(
        "Stanford discovery",
        "Stanford replication",
        "UCSB replication"
      )
    )
  )

plot <- ggplot(study_effects, aes(x = .data$estimate_pp, y = .data$design)) +
  geom_vline(
    xintercept = 0,
    color = "gray55",
    linewidth = 0.35,
    linetype = "dashed"
  ) +
  geom_errorbar(
    aes(xmin = .data$conf_low_pp, xmax = .data$conf_high_pp),
    orientation = "y",
    width = 0,
    color = colors_pub[["secondary"]],
    linewidth = 0.55
  ) +
  geom_point(color = colors_pub[["primary"]], size = 2.1) +
  facet_grid(.data$study ~ .) +
  scale_x_continuous(labels = function(x) paste0(x, " pp")) +
  labs(
    x = "Gain-frame minus loss-frame certain choice",
    y = NULL
  ) +
  theme_pub()

ggsave(
  here("figs", "protzko_frame_effects.pdf"),
  plot,
  width = fig_width_full,
  height = 6.0,
  device = cairo_pdf
)

message("Created: tabs/protzko_cell_means.parquet")
message("Created: tabs/protzko_source_flow.tex")
message("Created: tabs/protzko_frame_effects.parquet")
message("Created: tabs/protzko_pooled_effects.parquet")
message("Created: tabs/protzko_factorial_terms.parquet")
message("Created: tabs/protzko_effects.tex")
message("Created: tabs/protzko_macros.tex")
message("Created: figs/protzko_frame_effects.pdf")
