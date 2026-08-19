# 13_family_synthesis.R
# Assemble the experiment-organized main exhibit: one table and one figure
# with manipulation families as rows and the five surveys as columns.
# Outputs: tabs/family_effects.{parquet,tex}, figs/family_effects.pdf

library(arrow)
library(dplyr)
library(ggplot2)
library(here)
library(kableExtra)
library(knitr)
library(readr)
library(tidyr)

source(here("scripts", "00_config.R"))
source(here("scripts", "00_utils.R"))

main_effects <- read_parquet(here("tabs", "main_effects.parquet"))
study_2018 <- read_parquet(
  here("tabs", "protzko_frame_effects.parquet")
)

survey_levels <- c(
  "FFRISP", "Luth", "Stanford discovery", "Stanford replication",
  "UCSB replication"
)

effects_long <- bind_rows(
  main_effects |>
    transmute(
      survey = .data$sample_short,
      wording = .data$wording,
      family = .data$family,
      family_label = .data$family_label,
      display_order = .data$display_order,
      estimate_pp = .data$estimate_pp,
      conf_low_pp = .data$conf_low_pp,
      conf_high_pp = .data$conf_high_pp,
      n = .data$n_gain + .data$n_loss
    ),
  study_2018 |>
    transmute(
      survey = .data$study,
      wording = .data$wording,
      family = .data$family,
      family_label = .data$family_label,
      display_order = .data$display_order,
      estimate_pp = .data$estimate_pp,
      conf_low_pp = .data$conf_low_pp,
      conf_high_pp = .data$conf_high_pp,
      n = .data$n_gain + .data$n_loss
    )
)

stopifnot(nrow(effects_long) == 23L)

# The defective Luth natural-frequency pair is documented in the appendix
# wording inventory; the table note records the exclusion rather than an
# empty row.
family_effects <- effects_long |>
  mutate(
    cell = sprintf(
      "%.1f [%.1f, %.1f]",
      .data$estimate_pp,
      .data$conf_low_pp,
      .data$conf_high_pp
    )
  ) |>
  select(
    survey, wording, family, family_label, display_order, cell,
    estimate_pp, conf_low_pp, conf_high_pp, n
  )

write_parquet(family_effects, here("tabs", "family_effects.parquet"))

family_table <- family_effects |>
  select(wording, family_label, display_order, survey, cell) |>
  pivot_wider(names_from = survey, values_from = cell) |>
  arrange(.data$display_order) |>
  mutate(across(all_of(survey_levels), ~ replace_na(.x, "")))

family_groups <- family_table |>
  count(.data$family_label) |>
  arrange(match(.data$family_label, unique(family_table$family_label)))
group_index <- setNames(family_groups$n, family_groups$family_label)

family_table |>
  select(Wording = wording, all_of(survey_levels)) |>
  kable(
    format = "latex",
    booktabs = TRUE,
    align = c("l", rep("r", 5)),
    caption = "Gain-loss framing contrasts by manipulation and survey",
    label = "family-effects",
    linesep = "",
    col.names = c(
      "Wording", "FFRISP", "Luth", "Stanford discovery",
      "Stanford replication", "UCSB replication"
    )
  ) |>
  kable_styling(
    position = "center",
    font_size = 7,
    latex_options = "HOLD_position"
  ) |>
  column_spec(1, width = "4.8cm") |>
  pack_rows(index = group_index) |>
  footnote(
    general = paste(
      "Note: Entries are gain-frame minus loss-frame differences in choosing",
      "the certain option, in percentage points, with 95 percent HC2 confidence",
      "intervals. Blank cells mark manipulations a survey did not field;",
      "analyzed counts per cell appear in the appendix allocation table. The",
      "fielded 2018 text is unavailable, so the balanced-certain-option cells",
      "are described functionally. The excluded Luth natural-frequency pair",
      "had an instrument defect."
    ),
    footnote_as_chunk = TRUE,
    general_title = "",
    threeparttable = TRUE
  ) |>
  save_kable(here("tabs", "family_effects.tex"))

plot_data <- effects_long |>
  mutate(
    survey = factor(.data$survey, levels = survey_levels),
    family_label = factor(
      .data$family_label,
      levels = unique(.data$family_label[order(.data$display_order)])
    ),
    wording = factor(
      .data$wording,
      levels = rev(unique(.data$wording[order(.data$display_order)]))
    )
  )

survey_colors <- c(
  FFRISP = colors_pub[["accent"]],
  Luth = colors_pub[["primary"]],
  `Stanford discovery` = "#5B7B9C",
  `Stanford replication` = colors_pub[["secondary"]],
  `UCSB replication` = "#B08D57"
)

plot <- ggplot(
  plot_data,
  aes(x = .data$estimate_pp, y = .data$wording, color = .data$survey)
) +
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
    linewidth = 0.5,
    position = position_dodge(width = 0.65)
  ) +
  geom_point(size = 1.8, position = position_dodge(width = 0.65)) +
  scale_color_manual(values = survey_colors) +
  facet_grid(
    .data$family_label ~ .,
    scales = "free_y",
    space = "free_y",
    labeller = label_wrap_gen(width = 14)
  ) +
  scale_x_continuous(labels = function(x) paste0(x, " pp")) +
  labs(
    x = "Gain-frame minus loss-frame certain choice",
    y = NULL,
    color = NULL
  ) +
  theme_pub(base_size = 9) +
  theme(
    legend.position = "top",
    strip.text.y = element_text(angle = 0, hjust = 0)
  )

ggsave(
  here("figs", "family_effects.pdf"),
  plot,
  width = fig_width_full,
  height = 7.6,
  device = cairo_pdf
)

message("Created: tabs/family_effects.parquet")
message("Created: tabs/family_effects.tex")
message("Created: figs/family_effects.pdf")
