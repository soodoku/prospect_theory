library(arrow)
library(dplyr)
library(here)
library(readr)
library(testthat)

source(here("scripts", "00_utils.R"))

ffrisp <- read_parquet(here("data", "ffrisp", "ffrisp_respondent_2009.parquet"))
luth <- read_parquet(here("data", "luth", "luth_respondent_2009.parquet"))
protzko <- read_parquet(here("data", "protzko", "protzko_respondent.parquet"))

test_that("restricted sources match their recorded manifests", {
  skip_if_not(
    file.exists(here("data", "ffrisp", "source", "ffrisp1029_1.csv")),
    "restricted sources not present; released respondent files are tested below"
  )
  expect_true(verify_source_manifest(here("data", "ffrisp", "source")))
  expect_true(verify_source_manifest(here("data", "luth", "source")))
  expect_true(
    verify_source_manifest(here("data", "luth", "source", "open_ended"))
  )
  expect_true(verify_source_manifest(here("data", "protzko", "source")))
})

test_that("canonical respondent files preserve their row contracts", {
  expect_equal(nrow(ffrisp), 898L)
  expect_equal(n_distinct(ffrisp$case_id), 898L)
  expect_equal(sum(ffrisp$profile_matched), 898L)
  expect_equal(sum(is.na(ffrisp$survey_weight)), 2L)
  expect_equal(sum(is.na(ffrisp$certain_choice)), 11L)

  expect_equal(nrow(luth), 2330L)
  expect_equal(n_distinct(luth$case_id), 2330L)
  expect_equal(sum(luth$attention_pass), 2017L)
  expect_equal(sum(!luth$instrument_valid), 280L)
  expect_equal(sum(!luth$instrument_valid & luth$attention_pass), 240L)
  expect_equal(sum(is.na(luth$certain_choice)), 9L)
  expect_true(all(luth$instrument_valid[luth$attention_pass & is.na(luth$certain_choice)] == FALSE))

  expect_equal(nrow(protzko), 4589L)
  expect_equal(n_distinct(protzko$case_id), 4589L)
  expect_equal(sum(protzko$analysis_complete), 4585L)
  expect_equal(sum(is.na(protzko$certain_choice)), 4L)
})

test_that("canonical files contain no direct source identifiers", {
  prohibited <- c("identifier", "uniq", "rid", "raw_respondent_id")
  expect_length(intersect(names(ffrisp), prohibited), 0L)
  expect_length(intersect(names(luth), prohibited), 0L)
  expect_length(intersect(names(protzko), prohibited), 0L)
  expect_length(
    intersect(
      names(protzko),
      c("ipaddress", "responseid", "psid", "pid", "zipcode", "useragent")
    ),
    0L
  )
})

test_that("released files carry no restricted demographic or design columns", {
  restricted <- c(
    "age", "female", "duration", "birth_year", "source_row",
    "raw_education", "raw_race_ethnicity", "raw_region", "raw_hispanic",
    "raw_gender", "raw_age", "raw_stratum", "raw_psu", "raw_month_12_weight"
  )
  for (data in list(ffrisp, luth, protzko)) {
    expect_length(intersect(names(data), restricted), 0L)
    expect_false(any(grepl("^qd14_", names(data))))
  }
  expect_false("education" %in% names(protzko))
  expect_true("education_index" %in% names(protzko))
})

test_that("FFRISP stratum and psu labels derive only from the released file", {
  ffrisp_rows <- ffrisp |>
    mutate(.row = row_number())
  stratum_order <- ffrisp_rows |>
    summarize(size = n(), first = min(.row), .by = stratum) |>
    arrange(desc(size), first)
  expect_equal(stratum_order$stratum, seq_len(nrow(stratum_order)))

  psu_order <- ffrisp_rows |>
    summarize(size = n(), first = min(.row), .by = c(stratum, psu)) |>
    arrange(stratum, desc(size), first) |>
    mutate(expected = row_number(), .by = stratum)
  expect_equal(psu_order$psu, psu_order$expected)
})

test_that("sources_present requires a complete, nonempty manifest", {
  source_dir <- tempfile("sources_present_")
  dir.create(source_dir)
  manifest_path <- file.path(source_dir, "manifest.csv")

  expect_false(sources_present(manifest_path))
  writeLines("file,sha256,bytes", manifest_path)
  expect_false(sources_present(manifest_path))
  writeLines(c("file,sha256,bytes", "present.txt,abc,1"), manifest_path)
  expect_false(sources_present(manifest_path))
  writeLines("data", file.path(source_dir, "present.txt"))
  expect_true(sources_present(manifest_path))
})

test_that("received Protzko combined file discrepancy is documented", {
  reconstruction <- read_parquet(
    here("tabs", "protzko_reconstruction.parquet")
  )
  expect_equal(nrow(reconstruction), 1L)
  expect_equal(reconstruction$combined_row, 3089L)
  expect_equal(reconstruction$variable, "certain")
  expect_true(is.na(reconstruction$source_value))
  expect_equal(reconstruction$received_combined_value, 0L)
})

test_that("frame and response-order mappings are correct", {
  expect_equal(ffrisp$loss_frame, as.integer(ffrisp$condition %% 2L == 0L))
  expect_equal(luth$loss_frame, as.integer(luth$condition %% 2L == 0L))

  conditions <- 1:18
  certain_for_option_a <- luth_certain_choice(rep(1, 18), conditions)
  expect_equal(which(certain_for_option_a == 0L), c(9L, 10L, 17L, 18L))
  expect_equal(
    luth_certain_choice(rep(2, 18), conditions),
    1L - certain_for_option_a
  )
})

test_that("political-knowledge scoring follows the documented key", {
  expect_equal(
    score_knowledge("Vice President", 3, "67", 2, 1, TRUE),
    1
  )
  expect_equal(
    score_knowledge("V.P.", 3, "2/3", 2, 1, TRUE),
    1
  )
  expect_equal(
    score_knowledge("Senator", 1, "50", 1, 3, TRUE),
    0
  )
  expect_true(is.na(score_knowledge("Vice President", 3, "67", 2, 1, FALSE)))
})
