# 00_config.R
# Shared paths, labels, the canonical arm registry, and publication style.
# Output: none

# Canonical registry of every randomized wording pair (2009) and design cell
# (2018), at pair/cell grain. This is the single source of truth for condition
# labels, cross-survey manipulation families, and the per-arm design-factor
# codes consumed by the pooled moderator models. `wording_id` for 2018 cells
# is explicit_balance * 2 + internal_flip + 1.
family_labels <- c(
  original = "Original problem",
  risky_flip = "Risky-option outcome order",
  response_order = "Response order",
  balanced = "Certain-option completeness",
  numeric = "Certain-outcome perturbation",
  rewording = "Scenario rewording",
  natfreq = "Natural-frequency wording"
)

registry_2009 <- tibble::tribble(
  ~sample, ~wording_id, ~effect_id, ~family, ~variant, ~wording,
  ~valid, ~internal_flip_code, ~explicit_balance_code, ~response_order_code,
  ~certain_survivors, ~display_order,
  "FFRISP probability panel", 1L, "ffrisp_original", "original", NA,
  "Original wording", TRUE, 0L, 0L, 0L, 200L, 1L,
  "FFRISP probability panel", 2L, "ffrisp_risky_flip", "risky_flip", NA,
  "Risky-outcome order reversed", TRUE, 1L, 0L, 0L, 200L, 2L,
  "FFRISP probability panel", 3L, "ffrisp_rewording", "rewording", NA,
  "Expanded drug wording", TRUE, 0L, 0L, 0L, 200L, 11L,
  "Luth volunteer panel", 1L, "luth_original", "original", NA,
  "Original wording", TRUE, 0L, 0L, 0L, 200L, 1L,
  "Luth volunteer panel", 2L, "luth_199", "numeric", "199",
  "Certain outcome: 199 saved", TRUE, 0L, 0L, 0L, 199L, 9L,
  "Luth volunteer panel", 3L, "luth_201", "numeric", "201",
  "Certain outcome: 201 saved", TRUE, 0L, 0L, 0L, 201L, 10L,
  "Luth volunteer panel", 4L, "luth_natfreq", "natfreq", NA,
  "Natural-frequency wording", FALSE, 0L, 0L, 0L, 200L, 12L,
  "Luth volunteer panel", 5L, "luth_response_order", "response_order",
  "response_only", "Response order reversed", TRUE, 0L, 0L, 1L, 200L, 3L,
  "Luth volunteer panel", 6L, "luth_balanced", "balanced", "fully_stated",
  "Certain option fully stated", TRUE, 0L, 1L, 0L, 200L, 5L,
  "Luth volunteer panel", 7L, "luth_balanced_reversed", "balanced",
  "components_reversed", "Certain option fully stated, order reversed",
  TRUE, 0L, 1L, 0L, 200L, 6L,
  "Luth volunteer panel", 8L, "luth_risky_flip", "risky_flip", NA,
  "Risky-outcome order reversed", TRUE, 1L, 0L, 0L, 200L, 2L,
  "Luth volunteer panel", 9L, "luth_risky_response", "response_order",
  "with_risky_flip", "Risky-outcome and response order reversed",
  TRUE, 1L, 0L, 1L, 200L, 4L
)

protzko_cells <- tibble::tribble(
  ~internal_flip, ~explicit_balance, ~cell_slug, ~family, ~variant, ~wording,
  ~display_order,
  0L, 0L, "original", "original", NA, "Original wording", 1L,
  1L, 0L, "risky_flip", "risky_flip", NA, "Risky-outcome order reversed", 2L,
  0L, 1L, "balanced", "balanced", "fully_stated", "Balanced certain option", 7L,
  1L, 1L, "balanced_flip", "balanced", "with_risky_flip",
  "Balanced certain option, risky-outcome order reversed", 8L
)

protzko_studies <- tibble::tibble(
  sample = c("Stanford discovery", "Stanford replication", "UCSB replication"),
  study_slug = c("sd", "sr", "ucsb")
)

registry_2018 <- merge(protzko_studies, protzko_cells) |>
  dplyr::mutate(
    wording_id = explicit_balance * 2L + internal_flip + 1L,
    effect_id = paste(study_slug, cell_slug, sep = "_"),
    valid = TRUE,
    internal_flip_code = internal_flip,
    explicit_balance_code = explicit_balance,
    response_order_code = 0L,
    certain_survivors = 200L
  ) |>
  dplyr::select(
    sample, wording_id, effect_id, family, variant, wording, valid,
    internal_flip_code, explicit_balance_code, response_order_code,
    certain_survivors, display_order
  )

arm_registry <- dplyr::bind_rows(
  dplyr::mutate(registry_2009, module = "2009 wording experiments"),
  dplyr::mutate(registry_2018, module = "2018 factorial experiments")
) |>
  dplyr::mutate(family_label = unname(family_labels[family]))

registry_columns <- c(
  "wording", "family", "family_label", "variant", "effect_id",
  "internal_flip_code", "explicit_balance_code", "response_order_code",
  "certain_survivors", "display_order"
)

# Attach registry columns to a respondent-level table keyed by sample and
# wording_id. Every row must match exactly one registry arm.
join_arm_registry <- function(data, sample_value = NULL) {
  if (!is.null(sample_value)) {
    data <- dplyr::mutate(data, sample = sample_value)
  }
  joined <- dplyr::left_join(
    data,
    dplyr::select(
      arm_registry,
      dplyr::all_of(c("sample", "wording_id", registry_columns))
    ),
    by = c("sample", "wording_id"),
    relationship = "many-to-one"
  )
  if (anyNA(joined$effect_id)) {
    stop("Arm registry join failed: unmatched sample/wording_id rows",
      call. = FALSE
    )
  }
  joined
}

colors_pub <- c(
  primary = "#243447",
  secondary = "#7A8793",
  accent = "#A23B3B",
  light = "#CBD2D9"
)

fig_width_full <- 6.5
fig_height_main <- 5.7

theme_pub <- function(base_size = 10.5, base_family = "") {
  ggplot2::theme_bw(base_size = base_size, base_family = base_family) +
    ggplot2::theme(
      panel.grid.major.y = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.border = ggplot2::element_blank(),
      axis.line.x = ggplot2::element_line(color = "gray35", linewidth = 0.35),
      axis.ticks.y = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(color = "gray20"),
      axis.title = ggplot2::element_text(color = "gray10"),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", hjust = 0),
      legend.position = "none",
      plot.margin = ggplot2::margin(8, 10, 8, 8)
    )
}
