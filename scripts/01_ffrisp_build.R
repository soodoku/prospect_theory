# 01_ffrisp_build.R
# Build the deidentified FFRISP respondent-level analysis file.
# Output: data/ffrisp/ffrisp_respondent_2009.parquet, logs/ffrisp_build.txt

library(arrow)
library(dplyr)
library(haven)
library(here)
library(readr)

source(here("scripts", "00_config.R"))
source(here("scripts", "00_utils.R"))

source_dir <- here("data", "ffrisp", "source")
verify_source_manifest(source_dir)
survey_path <- here("data", "ffrisp", "source", "ffrisp1029_1.csv")
profile_path <- here("data", "ffrisp", "source", "sociodem.dta")
weight_path <- here("data", "ffrisp", "source", "FFRISP_weights.dta")
purrr::walk(c(survey_path, profile_path, weight_path), assert_file)

raw_survey <- read_csv(survey_path, show_col_types = FALSE)
raw_profile <- read_dta(profile_path) |> rename_with(tolower)
raw_weights <- read_dta(weight_path) |> rename_with(tolower)

assert_unique(filter(raw_profile, !is.na(abtsrbiid)), "abtsrbiid", "FFRISP profile")
assert_unique(raw_weights, "abtsrbiid", "FFRISP weights")

survey <- raw_survey |>
  rename(raw_condition = `Qktcomp) Computational Logic`) |>
  filter(complete == "COMPLETED", grepl("^[0-9]+$", identifier)) |>
  mutate(raw_respondent_id = as.numeric(identifier))

assert_rows(survey, 898L, "FFRISP completed sample with valid panel IDs")
assert_unique(survey, "raw_respondent_id", "FFRISP completed survey")

choice_matrix <- as.matrix(select(survey, qkt1, qkt2, qkt3, qkt4, qkt5, qkt6))
nonmissing_choices <- rowSums(!is.na(choice_matrix))
stopifnot(all(nonmissing_choices <= 1L))
raw_choice <- choice_matrix[cbind(seq_len(nrow(survey)), survey$raw_condition)]

profile <- raw_profile |>
  filter(!is.na(abtsrbiid)) |>
  transmute(
    raw_respondent_id = abtsrbiid,
    profile_matched = TRUE,
    raw_stratum = vstratum,
    raw_psu = vpsu,
    raw_gender = ppgender,
    raw_age = ppage,
    raw_race_ethnicity = ppethm,
    raw_education = ppeduc,
    raw_region = ppreg4
  )

weights <- raw_weights |>
  transmute(raw_respondent_id = abtsrbiid, raw_month_12_weight = w12_finalweights)

analysis <- survey |>
  mutate(raw_choice = raw_choice) |>
  left_join(profile, by = "raw_respondent_id", relationship = "many-to-one") |>
  left_join(weights, by = "raw_respondent_id", relationship = "many-to-one")

assert_rows(analysis, 898L, "FFRISP post-join sample")
stopifnot(sum(analysis$profile_matched %in% TRUE) == 898L)

analysis <- analysis |>
  arrange(rid) |>
  mutate(
    case_id = sprintf("ffrisp_%04d", row_number()),
    condition = as.integer(raw_condition),
    wording_id = as.integer((condition + 1L) %/% 2L),
    frame = if_else(condition %% 2L == 0L, "Loss frame", "Gain frame"),
    loss_frame = as.integer(frame == "Loss frame"),
    certain_choice = as.integer(raw_choice == 1),
    education = education_category(raw_education, c(8L, 9L, 11L, 12L)),
    survey_weight = as.numeric(raw_month_12_weight),
    instrument_valid = TRUE
  ) |>
  join_arm_registry(sample_value = "FFRISP probability panel")

# The released file carries only what the analysis consumes. The archived
# design codes are replaced by labels ordered by stratum size with ties
# broken by first appearance in the response-ordered file: both quantities
# are derivable from the released file itself, so the labels add no
# information about the archived codes and do not preserve their ordering,
# while the clustering the weighted sensitivity needs is intact.
analysis <- mutate(analysis, .row = row_number())

stratum_map <- analysis |>
  summarize(.size = n(), .first = min(.row), .by = raw_stratum) |>
  arrange(desc(.size), .first) |>
  mutate(stratum = row_number()) |>
  select(raw_stratum, stratum)

psu_map <- analysis |>
  summarize(.size = n(), .first = min(.row), .by = c(raw_stratum, raw_psu)) |>
  arrange(raw_stratum, desc(.size), .first) |>
  mutate(psu = row_number(), .by = raw_stratum) |>
  select(raw_stratum, raw_psu, psu)

analysis <- analysis |>
  left_join(stratum_map, by = "raw_stratum", relationship = "many-to-one") |>
  left_join(
    psu_map,
    by = c("raw_stratum", "raw_psu"),
    relationship = "many-to-one"
  ) |>
  select(
    case_id, sample, condition, wording_id, wording, family, family_label,
    variant, effect_id, internal_flip_code, explicit_balance_code,
    response_order_code, certain_survivors, display_order, frame, loss_frame,
    certain_choice, instrument_valid, profile_matched, survey_weight,
    stratum, psu, education
  )

output_path <- here("data", "ffrisp", "ffrisp_respondent_2009.parquet")
write_parquet(analysis, output_path)

build_log <- c(
  sprintf("raw survey rows: %d", nrow(raw_survey)),
  sprintf("completed rows: %d", sum(raw_survey$complete == "COMPLETED")),
  sprintf("analysis rows with numeric panel ID: %d", nrow(analysis)),
  sprintf("profile matches: %d", sum(analysis$profile_matched %in% TRUE)),
  sprintf("month 12 weight matches: %d", sum(!is.na(analysis$survey_weight))),
  sprintf("month 12 weight missing: %d", sum(is.na(analysis$survey_weight))),
  sprintf("framing outcome missing: %d", sum(is.na(analysis$certain_choice)))
)
writeLines(build_log, here("logs", "ffrisp_build.txt"))
message("Created: ", output_path)
