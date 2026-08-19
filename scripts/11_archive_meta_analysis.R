# 11_archive_meta_analysis.R
# Synthesize all independent gain-loss contrasts recovered in this archive.
# Outputs: meta/derived, meta/tables, and meta/figures artifacts

library(arrow)
library(dplyr)
library(ggplot2)
library(here)
library(kableExtra)
library(knitr)
library(metafor)
library(readr)

source(here("scripts", "00_config.R"))
source(here("scripts", "00_utils.R"))

main_path <- here("tabs", "main_effects.parquet")
protzko_path <- here("tabs", "protzko_frame_effects.parquet")
purrr::walk(c(main_path, protzko_path), assert_file)

dir.create(here("meta", "derived"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("meta", "tables"), recursive = TRUE, showWarnings = FALSE)
dir.create(here("meta", "figures"), recursive = TRUE, showWarnings = FALSE)

main_effects <- read_parquet(main_path) |>
  transmute(
    effect_id = .data$effect_id,
    study = .data$sample,
    design = .data$wording,
    family = .data$family,
    family_label = .data$family_label,
    display_order = .data$display_order,
    effect = .data$effect,
    std_error = .data$std_error,
    conf_low = .data$conf_low,
    conf_high = .data$conf_high,
    n_gain = .data$n_gain,
    n_loss = .data$n_loss,
    source_module = "2009 wording experiments"
  )

protzko_effects <- read_parquet(protzko_path) |>
  transmute(
    effect_id = .data$effect_id,
    study = .data$study,
    design = .data$design,
    family = .data$family,
    family_label = .data$family_label,
    display_order = .data$display_order,
    effect = .data$effect,
    std_error = .data$std_error,
    conf_low = .data$conf_low,
    conf_high = .data$conf_high,
    n_gain = .data$n_gain,
    n_loss = .data$n_loss,
    source_module = "2018 factorial experiments"
  )

archive_effects <- bind_rows(main_effects, protzko_effects) |>
  mutate(
    sampling_variance = .data$std_error^2,
    estimate_pp = 100 * .data$effect,
    conf_low_pp = 100 * .data$conf_low,
    conf_high_pp = 100 * .data$conf_high,
    display_label = paste(.data$study, .data$design, sep = ": ")
  )

assert_rows(archive_effects, 23L, "Archive meta-analysis effect file")
assert_unique(archive_effects, "effect_id", "Archive meta-analysis effect file")
stopifnot(
  all(is.finite(archive_effects$effect)),
  all(is.finite(archive_effects$std_error)),
  all(archive_effects$std_error > 0)
)

write_parquet(
  archive_effects,
  here("meta", "derived", "archive_effects.parquet")
)

random_model <- rma.uni(
  yi = effect,
  vi = sampling_variance,
  data = archive_effects,
  method = "REML",
  test = "knha"
)
fixed_model <- rma.uni(
  yi = effect,
  vi = sampling_variance,
  data = archive_effects,
  method = "FE"
)
random_prediction <- predict(random_model)

summary_data <- tibble(
  model = c("Random effects", "Fixed effect sensitivity"),
  estimate = c(coef(random_model)[[1]], coef(fixed_model)[[1]]),
  std_error = c(random_model$se[[1]], fixed_model$se[[1]]),
  conf_low = c(random_model$ci.lb[[1]], fixed_model$ci.lb[[1]]),
  conf_high = c(random_model$ci.ub[[1]], fixed_model$ci.ub[[1]]),
  prediction_low = c(random_prediction$pi.lb[[1]], NA_real_),
  prediction_high = c(random_prediction$pi.ub[[1]], NA_real_),
  tau_squared = c(random_model$tau2, NA_real_),
  i_squared = c(random_model$I2, NA_real_),
  q = c(random_model$QE, NA_real_),
  q_df = c(random_model$k - random_model$p, NA_real_),
  q_p_value = c(random_model$QEp, NA_real_)
) |>
  mutate(across(where(is.numeric), ~ as.numeric(.x)))

write_parquet(
  summary_data,
  here("meta", "tables", "archive_meta_summary.parquet")
)

summary_table <- summary_data |>
  transmute(
    Model = .data$model,
    `Mean difference [95% CI]` = sprintf(
      "%.1f [%.1f, %.1f]",
      100 * .data$estimate,
      100 * .data$conf_low,
      100 * .data$conf_high
    ),
    `95% prediction interval` = if_else(
      is.na(.data$prediction_low),
      "--",
      sprintf(
        "%.1f to %.1f",
        100 * .data$prediction_low,
        100 * .data$prediction_high
      )
    )
  )

summary_table |>
  kable(
    format = "latex",
    booktabs = TRUE,
    align = c("l", "r", "r"),
    caption = "Archive-wide synthesis of gain-loss framing contrasts",
    label = "archive-meta",
    linesep = ""
  ) |>
  kable_styling(position = "center", latex_options = "HOLD_position") |>
  footnote(
    general = paste(
      "Note: Entries are gain-frame minus loss-frame percentage points across",
      "23 independent randomized contrasts. The primary model uses REML and",
      "Knapp--Hartung inference. This synthesis describes the recovered archive",
      "and is not a literature-wide meta-analysis."
    ),
    footnote_as_chunk = TRUE,
    general_title = "",
    threeparttable = TRUE
  ) |>
  save_kable(here("meta", "tables", "archive_meta_summary.tex"))

macros <- c(
  sprintf("\\newcommand{\\ArchiveMetaK}{%d}", random_model$k),
  sprintf(
    "\\newcommand{\\ArchiveMetaN}{%s}",
    format(sum(archive_effects$n_gain + archive_effects$n_loss), big.mark = ",")
  ),
  sprintf("\\newcommand{\\ArchiveMetaEffect}{%.1f}", 100 * coef(random_model)[[1]]),
  sprintf("\\newcommand{\\ArchiveMetaLow}{%.1f}", 100 * random_model$ci.lb[[1]]),
  sprintf("\\newcommand{\\ArchiveMetaHigh}{%.1f}", 100 * random_model$ci.ub[[1]]),
  sprintf("\\newcommand{\\ArchiveMetaPredLow}{%.1f}", 100 * random_prediction$pi.lb[[1]]),
  sprintf("\\newcommand{\\ArchiveMetaPredHigh}{%.1f}", 100 * random_prediction$pi.ub[[1]]),
  sprintf("\\newcommand{\\ArchiveMetaISquared}{%.1f}", random_model$I2),
  sprintf("\\newcommand{\\ArchiveMetaTau}{%.3f}", sqrt(random_model$tau2))
)
write_lines(
  macros,
  here("meta", "tables", "archive_meta_macros.tex")
)

family_levels <- archive_effects |>
  arrange(.data$display_order) |>
  distinct(.data$family_label) |>
  pull(.data$family_label)

plot_data <- archive_effects |>
  arrange(.data$display_order, .data$study) |>
  mutate(
    display_label = factor(.data$display_label, levels = rev(.data$display_label)),
    module_label = factor(.data$family_label, levels = family_levels)
  )

plot <- ggplot(plot_data, aes(x = .data$estimate_pp, y = .data$display_label)) +
  annotate(
    "rect",
    xmin = 100 * random_model$ci.lb[[1]],
    xmax = 100 * random_model$ci.ub[[1]],
    ymin = -Inf,
    ymax = Inf,
    fill = colors_pub[["light"]],
    alpha = 0.35
  ) +
  geom_vline(
    xintercept = 0,
    color = "gray55",
    linewidth = 0.35,
    linetype = "dashed"
  ) +
  geom_vline(
    xintercept = 100 * coef(random_model)[[1]],
    color = colors_pub[["accent"]],
    linewidth = 0.65
  ) +
  geom_errorbar(
    aes(xmin = .data$conf_low_pp, xmax = .data$conf_high_pp),
    orientation = "y",
    width = 0,
    color = colors_pub[["secondary"]],
    linewidth = 0.5
  ) +
  geom_point(color = colors_pub[["primary"]], size = 1.8) +
  facet_grid(
    .data$module_label ~ .,
    scales = "free_y",
    space = "free_y",
    labeller = label_wrap_gen(width = 14)
  ) +
  scale_x_continuous(labels = function(x) paste0(x, " pp")) +
  labs(
    x = "Gain-frame minus loss-frame certain choice",
    y = NULL
  ) +
  theme_pub(base_size = 9) +
  theme(
    strip.text = element_text(size = 8),
    strip.text.y = element_text(angle = 0, hjust = 0)
  )

ggsave(
  here("meta", "figures", "archive_meta_forest.pdf"),
  plot,
  width = fig_width_full,
  height = 9.0,
  device = cairo_pdf
)

message("Created: meta/derived/archive_effects.parquet")
message("Created: meta/tables/archive_meta_summary.parquet")
message("Created: meta/tables/archive_meta_summary.tex")
message("Created: meta/tables/archive_meta_macros.tex")
message("Created: meta/figures/archive_meta_forest.pdf")
