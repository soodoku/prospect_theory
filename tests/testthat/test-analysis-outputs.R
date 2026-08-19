library(arrow)
library(dplyr)
library(here)
library(readr)
library(testthat)

effects <- read_parquet(here("tabs", "main_effects.parquet"))
invariance <- read_parquet(here("tabs", "invariance_profile.parquet"))
heterogeneity <- read_parquet(here("tabs", "heterogeneity_tests.parquet"))
sensitivity <- read_parquet(here("tabs", "sensitivity_effects.parquet"))
moderators <- read_parquet(here("tabs", "moderator_interactions.parquet"))
condition_counts <- read_parquet(here("tabs", "condition_counts.parquet"))
wording_inventory <- read_parquet(
  here("tabs", "wording_inventory.parquet")
)
open_ended <- read_parquet(
  here("tabs", "open_ended_reliability.parquet")
)
open_ended_categories <- read_parquet(
  here("tabs", "open_ended_category_reliability.parquet")
)
open_ended_frame <- read_parquet(
  here("tabs", "open_ended_frame_effects.parquet")
)
sample_flow <- read_parquet(here("tabs", "sample_flow.parquet"))
protzko_effects <- read_parquet(
  here("tabs", "protzko_frame_effects.parquet")
)
protzko_pooled <- read_parquet(
  here("tabs", "protzko_pooled_effects.parquet")
)
protzko_factorial <- read_parquet(
  here("tabs", "protzko_factorial_terms.parquet")
)
archive_meta_effects <- read_parquet(
  here("meta", "derived", "archive_effects.parquet")
)
archive_meta_summary <- read_parquet(
  here("meta", "tables", "archive_meta_summary.parquet")
)
manipulation_effects <- read_parquet(
  here("meta", "derived", "manipulation_effects.parquet")
)
manipulation_meta <- read_parquet(
  here("meta", "tables", "manipulation_meta.parquet")
)

test_that("main estimates include every valid wording pair", {
  expect_equal(nrow(effects), 11L)
  expect_equal(sum(effects$sample_short == "FFRISP"), 3L)
  expect_equal(sum(effects$sample_short == "Luth"), 8L)
  expect_false(any(effects$sample_short == "Luth" & effects$wording_id == 4L))
})

test_that("original-wording contrasts reproduce fixed targets", {
  ffrisp_original <- effects |>
    filter(sample_short == "FFRISP", wording_id == 1L)
  luth_original <- effects |>
    filter(sample_short == "Luth", wording_id == 1L)

  expect_equal(ffrisp_original$n_gain + ffrisp_original$n_loss, 299L)
  expect_equal(ffrisp_original$effect, 0.2978274531, tolerance = 1e-9)
  expect_equal(luth_original$n_gain + luth_original$n_loss, 232L)
  expect_equal(luth_original$effect, 0.2515140187, tolerance = 1e-9)
})

test_that("local description-invariance profile preserves paired outcomes", {
  expect_equal(nrow(invariance), 6L)
  expect_equal(sort(unique(invariance$certain_survivors)), 199:201)
  expect_equal(sum(invariance$n), 647L)
  expect_true(all(invariance$conf_low <= invariance$estimate))
  expect_true(all(invariance$estimate <= invariance$conf_high))

  frame_gaps <- invariance |>
    select(certain_survivors, frame, estimate) |>
    tidyr::pivot_wider(names_from = frame, values_from = estimate) |>
    mutate(gap = `Gain frame` - `Loss frame`)
  expect_true(all(frame_gaps$gap > 0))
  expect_true(file.exists(here("figs", "invariance_profile.pdf")))
})

test_that("diagnostic outputs satisfy their contracts", {
  expect_equal(nrow(heterogeneity), 2L)
  expect_equal(nrow(sensitivity), 4L)
  expect_equal(nrow(moderators), 5L)
  expect_true(all(moderators$holm_p_value >= moderators$p_value))
  expect_true(all(sensitivity$conf_low < sensitivity$effect))
  expect_true(all(sensitivity$effect < sensitivity$conf_high))
  expect_equal(nrow(open_ended_frame), 8L)
  expect_equal(nrow(sample_flow), 14L)
  final_2018 <- sample_flow |>
    filter(
      sample %in% c(
        "Stanford discovery", "Stanford replication", "UCSB replication"
      ),
      grepl("Final analytic sample", stage)
    )
  expect_equal(nrow(final_2018), 3L)
  expect_equal(sum(final_2018$n), 4585L)
})

test_that("appendix allocation counts cover all deployed wording pairs", {
  expect_equal(nrow(condition_counts), 24L)
  expect_equal(sum(condition_counts$sample == "FFRISP"), 3L)
  expect_equal(sum(condition_counts$sample == "Luth"), 9L)
  expect_equal(sum(condition_counts$sample == "Stanford discovery"), 4L)
  expect_equal(sum(condition_counts$sample == "Stanford replication"), 4L)
  expect_equal(sum(condition_counts$sample == "UCSB replication"), 4L)

  defective <- condition_counts |>
    filter(sample == "Luth", wording_id == 4L)
  expect_equal(nrow(defective), 1L)
  expect_false(defective$instrument_valid)
  expect_equal(defective$analyzed_gain, 0)
  expect_equal(defective$analyzed_loss, 0)
})

test_that("wording inventory documents every deployed pair and cell", {
  expect_equal(nrow(wording_inventory), 16L)
  expect_equal(sum(wording_inventory$sample == "FFRISP"), 3L)
  expect_equal(sum(wording_inventory$sample == "Luth"), 9L)
  expect_equal(sum(wording_inventory$sample == "2018 factorial"), 4L)
  expect_equal(sum(wording_inventory$status == "Excluded"), 1L)
  expect_setequal(
    unique(wording_inventory$verbatim_status),
    c("verbatim", "reconstructed", "functional")
  )

  excluded <- wording_inventory |>
    filter(status == "Excluded")
  expect_match(excluded$wording, "Natural-frequency")
  expect_match(excluded$other_change, "Programs C and D")
})

test_that("open-ended audit exports aggregate reliability only", {
  primary <- open_ended |>
    filter(measure == "Primary-code exact agreement")

  expect_equal(primary$n, 2295L)
  expect_equal(primary$agreement, 0.7882353, tolerance = 1e-7)
  expect_equal(primary$kappa, 0.7305711, tolerance = 1e-7)
  expect_equal(nrow(open_ended_categories), 8L)
  expect_equal(open_ended_categories$code, 1:8)

  exported_names <- c(names(open_ended), names(open_ended_categories))
  expect_false(any(grepl("response|verbatim|text", exported_names, ignore.case = TRUE)))
})

test_that("Protzko factorial analysis preserves all randomized contrasts", {
  expect_equal(nrow(protzko_effects), 12L)
  expect_equal(nrow(protzko_pooled), 4L)
  expect_equal(sum(protzko_effects$n_gain + protzko_effects$n_loss), 4585L)
  expect_true(all(protzko_effects$effect > 0))

  explicit_interaction <- protzko_factorial |>
    filter(term == "gain_frame:explicit_balance")
  expect_equal(nrow(explicit_interaction), 1L)
  expect_equal(
    explicit_interaction$estimate,
    -0.2039339003,
    tolerance = 1e-9
  )
  expect_true(file.exists(here("figs", "protzko_frame_effects.pdf")))
})

test_that("manipulation-effect synthesis differences within survey", {
  expect_equal(nrow(manipulation_effects), 18L)
  expect_equal(nrow(manipulation_meta), 9L)
  expect_equal(
    manipulation_meta$k[manipulation_meta$family == "risky_flip"],
    5L
  )
  expect_equal(
    manipulation_meta$k[
      manipulation_meta$family == "balanced" &
        manipulation_meta$variant == "fully_stated"
    ],
    4L
  )
  expect_equal(sum(manipulation_meta$k), 18L)

  balanced_changes <- manipulation_effects |>
    filter(family == "balanced", variant == "fully_stated")
  expect_equal(nrow(balanced_changes), 4L)
  expect_true(all(balanced_changes$change < 0))
  expect_true(
    manipulation_meta$estimate_pp[
      manipulation_meta$family == "balanced" &
        manipulation_meta$variant == "fully_stated"
    ] < 0
  )
})

test_that("archive meta-analysis is separate and reproducible", {
  expect_equal(nrow(archive_meta_effects), 23L)
  expect_equal(n_distinct(archive_meta_effects$effect_id), 23L)
  expect_equal(sum(archive_meta_effects$source_module == "2009 wording experiments"), 11L)
  expect_equal(sum(archive_meta_effects$source_module == "2018 factorial experiments"), 12L)

  random_summary <- archive_meta_summary |>
    filter(model == "Random effects")
  expect_equal(random_summary$estimate, 0.2469692244, tolerance = 1e-9)
  expect_true(random_summary$prediction_low < random_summary$estimate)
  expect_true(random_summary$estimate < random_summary$prediction_high)
  expect_true(file.exists(here("meta", "figures", "archive_meta_forest.pdf")))
})
