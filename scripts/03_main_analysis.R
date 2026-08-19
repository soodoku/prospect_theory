# 03_main_analysis.R
# Estimate gain-loss framing contrasts and generate the main exhibit.
# Outputs: main effect files in tabs/ and framing_effects.pdf in figs/.

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

ffrisp_path <- here("data", "ffrisp", "ffrisp_respondent_2009.parquet")
luth_path <- here("data", "luth", "luth_respondent_2009.parquet")
purrr::walk(c(ffrisp_path, luth_path), assert_file)

ffrisp <- read_parquet(ffrisp_path)
luth <- read_parquet(luth_path)
luth_primary <- luth |>
  filter(attention_pass)

registry_keep <- c(
  "sample", "wording_id", "wording", "family", "family_label", "variant",
  "effect_id", "display_order", "frame", "loss_frame", "certain_choice",
  "instrument_valid"
)
analysis <- bind_rows(
  select(ffrisp, all_of(registry_keep)),
  select(luth_primary, all_of(registry_keep))
) |>
  filter(instrument_valid, !is.na(certain_choice))

estimate_contrast <- function(data) {
  model <- lm_robust(
    certain_choice ~ loss_frame,
    data = data,
    se_type = "HC2"
  )
  term <- tidy(model, conf.int = TRUE) |>
    filter(term == "loss_frame")
  tibble(
    n_gain = sum(data$loss_frame == 0L),
    n_loss = sum(data$loss_frame == 1L),
    gain_mean = mean(data$certain_choice[data$loss_frame == 0L]),
    loss_mean = mean(data$certain_choice[data$loss_frame == 1L]),
    effect = -term$estimate,
    std_error = term$std.error,
    conf_low = -term$conf.high,
    conf_high = -term$conf.low,
    interval = "95% HC2 confidence interval"
  )
}

effects <- analysis |>
  group_by(
    sample, wording_id, wording, family, family_label, variant, effect_id,
    display_order
  ) |>
  group_modify(~ estimate_contrast(.x)) |>
  ungroup() |>
  mutate(
    sample_short = if_else(grepl("FFRISP", sample), "FFRISP", "Luth"),
    estimate_pp = 100 * effect,
    conf_low_pp = 100 * conf_low,
    conf_high_pp = 100 * conf_high
  ) |>
  arrange(sample_short, wording_id)

write_parquet(effects, here("tabs", "main_effects.parquet"))

invariance_profile <- luth_primary |>
  filter(
    instrument_valid,
    wording_id %in% 1:3,
    !is.na(certain_choice)
  ) |>
  mutate(
    certain_survivors = recode(
      wording_id,
      `1` = 200L,
      `2` = 199L,
      `3` = 201L
    )
  ) |>
  group_by(certain_survivors, frame) |>
  summarise(
    n = n(),
    estimate = mean(certain_choice),
    std_error = sd(certain_choice) / sqrt(n),
    critical_value = qt(0.975, df = n - 1L),
    conf_low = pmax(0, estimate - critical_value * std_error),
    conf_high = pmin(1, estimate + critical_value * std_error),
    .groups = "drop"
  ) |>
  arrange(certain_survivors, frame)

write_parquet(invariance_profile, here("tabs", "invariance_profile.parquet"))

effect_table <- effects |>
  mutate(
    `Gain frame` = sprintf("%.1f%%", 100 * gain_mean),
    `Loss frame` = sprintf("%.1f%%", 100 * loss_mean),
    `Difference [95% CI]` = sprintf(
      "%.1f [%.1f, %.1f]",
      estimate_pp,
      conf_low_pp,
      conf_high_pp
    )
  ) |>
  select(Wording = wording, `Gain frame`, `Loss frame`, `Difference [95% CI]`)

effect_table |>
  kable(
    format = "latex",
    booktabs = TRUE,
    align = c("l", "r", "r", "r"),
    caption = "2009 experiments: certain choice by frame and wording",
    label = "effects",
    linesep = ""
  ) |>
  kable_styling(position = "center", latex_options = "HOLD_position") |>
  pack_rows(index = c("FFRISP probability panel" = 3, "Luth volunteer panel" = 8)) |>
  footnote(
    general = paste(
      "Note: Entries in the middle columns are percentages choosing the certain option.",
      "Differences are gain-frame minus loss-frame percentage points.",
      "Confidence intervals use HC2 standard errors.",
      "The defective Luth natural-frequency pair is omitted."
    ),
    footnote_as_chunk = TRUE,
    general_title = "",
    threeparttable = TRUE
  ) |>
  save_kable(here("tabs", "main_effects.tex"))

macro_row <- function(sample_name, wording_number) {
  effects |>
    filter(
      .data$sample_short == sample_name,
      .data$wording_id == wording_number
    ) |>
    slice(1)
}

ffr_original <- macro_row("FFRISP", 1L)
luth_original <- macro_row("Luth", 1L)
luth_effects <- effects |>
  filter(sample_short == "Luth")
macros <- c(
  sprintf("\\newcommand{\\FFROriginalGain}{%.1f}", 100 * ffr_original$gain_mean),
  sprintf("\\newcommand{\\FFROriginalLoss}{%.1f}", 100 * ffr_original$loss_mean),
  sprintf("\\newcommand{\\FFROriginalEffect}{%.1f}", ffr_original$estimate_pp),
  sprintf("\\newcommand{\\FFROriginalLow}{%.1f}", ffr_original$conf_low_pp),
  sprintf("\\newcommand{\\FFROriginalHigh}{%.1f}", ffr_original$conf_high_pp),
  sprintf("\\newcommand{\\FFROriginalN}{%d}", ffr_original$n_gain + ffr_original$n_loss),
  sprintf("\\newcommand{\\LuthOriginalGain}{%.1f}", 100 * luth_original$gain_mean),
  sprintf("\\newcommand{\\LuthOriginalLoss}{%.1f}", 100 * luth_original$loss_mean),
  sprintf("\\newcommand{\\LuthOriginalEffect}{%.1f}", luth_original$estimate_pp),
  sprintf("\\newcommand{\\LuthOriginalLow}{%.1f}", luth_original$conf_low_pp),
  sprintf("\\newcommand{\\LuthOriginalHigh}{%.1f}", luth_original$conf_high_pp),
  sprintf("\\newcommand{\\LuthOriginalN}{%d}", luth_original$n_gain + luth_original$n_loss),
  sprintf("\\newcommand{\\LuthMinimumEffect}{%.1f}", min(luth_effects$estimate_pp)),
  sprintf("\\newcommand{\\LuthMaximumEffect}{%.1f}", max(luth_effects$estimate_pp)),
  sprintf(
    "\\newcommand{\\FFRMinimumEffect}{%.1f}",
    min(effects$estimate_pp[effects$sample_short == "FFRISP"])
  ),
  sprintf(
    "\\newcommand{\\FFRMaximumEffect}{%.1f}",
    max(effects$estimate_pp[effects$sample_short == "FFRISP"])
  )
)

# LaTeX control sequences cannot contain digits, so the numeric-perturbation
# arms get word names.
arm_macro_names <- c(
  ffrisp_original = "FFRArmOriginal",
  ffrisp_risky_flip = "FFRArmRiskyFlip",
  ffrisp_rewording = "FFRArmRewording",
  luth_original = "LuthArmOriginal",
  luth_199 = "LuthArmLower",
  luth_201 = "LuthArmUpper",
  luth_response_order = "LuthArmResponseOrder",
  luth_balanced = "LuthArmBalanced",
  luth_balanced_reversed = "LuthArmBalancedReversed",
  luth_risky_flip = "LuthArmRiskyFlip",
  luth_risky_response = "LuthArmRiskyResponse"
)
stopifnot(all(effects$effect_id %in% names(arm_macro_names)))
arm_macros <- unlist(lapply(seq_len(nrow(effects)), function(i) {
  stem <- arm_macro_names[[effects$effect_id[[i]]]]
  c(
    sprintf("\\newcommand{\\%sEffect}{%.1f}", stem, effects$estimate_pp[[i]]),
    sprintf("\\newcommand{\\%sLow}{%.1f}", stem, effects$conf_low_pp[[i]]),
    sprintf("\\newcommand{\\%sHigh}{%.1f}", stem, effects$conf_high_pp[[i]])
  )
}))
macros <- c(macros, arm_macros)
writeLines(macros, here("tabs", "macros.tex"))

plot_data <- effects |>
  mutate(
    sample_short = factor(sample_short, levels = c("FFRISP", "Luth")),
    wording = factor(wording, levels = rev(unique(wording)))
  )

plot <- ggplot(plot_data, aes(x = estimate_pp, y = wording)) +
  geom_vline(xintercept = 0, color = "gray55", linewidth = 0.35, linetype = "dashed") +
  geom_errorbar(
    aes(xmin = conf_low_pp, xmax = conf_high_pp),
    orientation = "y",
    width = 0,
    color = colors_pub[["secondary"]],
    linewidth = 0.55
  ) +
  geom_point(
    aes(color = wording_id == 1L),
    size = 2.1
  ) +
  scale_color_manual(
    values = c(`FALSE` = colors_pub[["primary"]], `TRUE` = colors_pub[["accent"]])
  ) +
  facet_grid(sample_short ~ ., scales = "free_y", space = "free_y") +
  scale_x_continuous(labels = function(x) paste0(x, " pp")) +
  labs(x = "Gain-frame minus loss-frame certain choice", y = NULL) +
  theme_pub()

ggsave(
  here("figs", "framing_effects.pdf"),
  plot,
  width = fig_width_full,
  height = fig_height_main,
  device = cairo_pdf
)

invariance_plot <- ggplot(
  invariance_profile,
  aes(x = certain_survivors, y = estimate, color = frame, group = frame)
) +
  geom_line(linewidth = 0.65) +
  geom_errorbar(
    aes(ymin = conf_low, ymax = conf_high),
    width = 0.08,
    linewidth = 0.55
  ) +
  geom_point(size = 2.2) +
  scale_color_manual(
    values = c(
      `Gain frame` = colors_pub[["accent"]],
      `Loss frame` = colors_pub[["primary"]]
    )
  ) +
  scale_x_continuous(breaks = 199:201) +
  scale_y_continuous(
    breaks = seq(0.3, 0.8, by = 0.1),
    labels = function(x) paste0(round(100 * x), "%")
  ) +
  coord_cartesian(ylim = c(0.25, 0.85)) +
  labs(
    x = "People certain to survive",
    y = "Choosing the certain option",
    color = NULL
  ) +
  theme_pub() +
  theme(legend.position = "top")

ggsave(
  here("figs", "invariance_profile.pdf"),
  invariance_plot,
  width = fig_width_full,
  height = 3.4,
  device = cairo_pdf
)

message("Created: tabs/main_effects.parquet")
message("Created: tabs/main_effects.tex")
message("Created: tabs/macros.tex")
message("Created: figs/framing_effects.pdf")
message("Created: tabs/invariance_profile.parquet")
message("Created: figs/invariance_profile.pdf")
