library(arrow)
library(dplyr)
library(here)
library(readr)
library(testthat)

source(here("scripts", "00_config.R"))

registry_sample <- function(short) {
  recode(
    short,
    FFRISP = "FFRISP probability panel",
    Luth = "Luth volunteer panel"
  )
}

test_that("arm registry satisfies its invariants", {
  expect_equal(nrow(arm_registry), 24L)
  expect_equal(n_distinct(arm_registry$effect_id), 24L)
  expect_equal(sum(!arm_registry$valid), 1L)
  expect_equal(arm_registry$effect_id[!arm_registry$valid], "luth_natfreq")

  valid_counts <- arm_registry |>
    filter(valid) |>
    count(family) |>
    arrange(family)
  expect_equal(
    valid_counts,
    tibble(
      family = c(
        "balanced", "numeric", "original", "response_order", "rewording",
        "risky_flip"
      ),
      n = c(8L, 2L, 5L, 2L, 1L, 5L)
    )
  )
})

test_that("registry factor codes classify the 2009 arms correctly", {
  code <- function(id, column) {
    arm_registry[[column]][arm_registry$effect_id == id]
  }
  expect_equal(code("ffrisp_risky_flip", "internal_flip_code"), 1L)
  expect_equal(code("luth_risky_flip", "internal_flip_code"), 1L)
  expect_equal(code("luth_risky_response", "internal_flip_code"), 1L)
  expect_equal(code("luth_balanced", "explicit_balance_code"), 1L)
  expect_equal(code("luth_balanced_reversed", "explicit_balance_code"), 1L)
  expect_equal(code("luth_response_order", "response_order_code"), 1L)
  expect_equal(code("luth_risky_response", "response_order_code"), 1L)
  expect_equal(code("ffrisp_original", "internal_flip_code"), 0L)
  expect_equal(code("ffrisp_original", "explicit_balance_code"), 0L)
  expect_equal(code("luth_199", "certain_survivors"), 199L)
  expect_equal(code("luth_201", "certain_survivors"), 201L)

  no_code_2009 <- arm_registry |>
    filter(
      module == "2009 wording experiments",
      !effect_id %in% c(
        "ffrisp_risky_flip", "luth_risky_flip", "luth_risky_response",
        "luth_balanced", "luth_balanced_reversed", "luth_response_order"
      )
    )
  expect_true(all(no_code_2009$internal_flip_code == 0L))
  expect_true(all(no_code_2009$explicit_balance_code == 0L))
  expect_true(all(no_code_2009$response_order_code == 0L))
})

test_that("shared manipulations carry one canonical label across surveys", {
  risky_flip <- arm_registry |>
    filter(family == "risky_flip")
  expect_equal(unique(risky_flip$wording), "Risky-outcome order reversed")
  originals <- arm_registry |>
    filter(family == "original")
  expect_equal(unique(originals$wording), "Original wording")
})

test_that("generated exhibits agree with the registry labels", {
  effects <- read_parquet(here("tabs", "main_effects.parquet"))
  protzko <- read_parquet(
    here("tabs", "protzko_frame_effects.parquet")
  )
  archive <- read_parquet(
    here("meta", "derived", "archive_effects.parquet")
  )
  counts <- read_parquet(here("tabs", "condition_counts.parquet"))
  inventory <- read_parquet(
    here("tabs", "wording_inventory.parquet")
  )
  family_effects <- read_parquet(
    here("tabs", "family_effects.parquet")
  )

  registry_labels <- arm_registry |>
    select(effect_id, registry_wording = wording)

  effects_check <- effects |>
    left_join(registry_labels, by = "effect_id")
  expect_true(all(effects_check$wording == effects_check$registry_wording))

  protzko_check <- protzko |>
    left_join(registry_labels, by = "effect_id")
  expect_equal(n_distinct(protzko$effect_id), 12L)
  expect_true(all(protzko_check$wording == protzko_check$registry_wording))

  expect_setequal(
    archive$effect_id,
    c(effects$effect_id, protzko$effect_id)
  )
  archive_check <- archive |>
    left_join(registry_labels, by = "effect_id")
  expect_true(all(archive_check$design == archive_check$registry_wording))

  counts_check <- counts |>
    mutate(sample_full = registry_sample(sample)) |>
    left_join(
      arm_registry |>
        select(sample_full = sample, wording_id, registry_wording = wording),
      by = c("sample_full", "wording_id")
    )
  expect_true(all(counts_check$wording == counts_check$registry_wording))

  inventory_check <- inventory |>
    filter(sample %in% c("FFRISP", "Luth")) |>
    mutate(sample_full = registry_sample(sample)) |>
    left_join(
      arm_registry |>
        select(sample_full = sample, wording_id, registry_wording = wording),
      by = c("sample_full", "wording_id")
    )
  expect_true(
    all(inventory_check$wording == inventory_check$registry_wording)
  )

  expect_equal(nrow(family_effects), 23L)
  expect_false(any(family_effects$family == "natfreq"))
  expect_equal(n_distinct(family_effects$survey), 5L)
})
