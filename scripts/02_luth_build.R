# 02_luth_build.R
# Build the deidentified Luth respondent-level analysis file.
# Output: data/luth/luth_respondent_2009.parquet, logs/luth_build.txt

library(arrow)
library(dplyr)
library(haven)
library(here)
library(readr)

source(here("scripts", "00_config.R"))
source(here("scripts", "00_utils.R"))

source_dir <- here("data", "luth", "source")
verify_source_manifest(source_dir)
survey_path <- here("data", "luth", "source", "luth1024.csv")
demographic_path <- here("data", "luth", "source", "dem.luth.sav")
knowledge_path <- here("data", "luth", "source", "party.luth.sav")
purrr::walk(c(survey_path, demographic_path, knowledge_path), assert_file)

raw_survey <- read_csv(survey_path, show_col_types = FALSE)
raw_demographic <- read_sav(demographic_path) |> rename_with(tolower)
raw_knowledge <- read_sav(knowledge_path) |> rename_with(tolower)

assert_unique(raw_survey, "uniq", "Luth experiment")
assert_unique(raw_demographic, "identifier", "Luth demographic survey")
assert_unique(raw_knowledge, "identifier", "Luth political survey")

survey <- raw_survey |>
  filter(complete == "COMPLETED")
assert_rows(survey, 2330L, "Luth completed sample")

choice_columns <- paste0("q", seq_len(18), "a")
choice_matrix <- as.matrix(select(survey, all_of(choice_columns)))
raw_choice <- choice_matrix[cbind(seq_len(nrow(survey)), survey$qra)]

demographic <- raw_demographic |>
  select(
    raw_respondent_id = identifier,
    raw_birth_year = qd7y,
    raw_gender = qd8,
    raw_education = qd11,
    raw_hispanic = qd12,
    starts_with("qd14_")
  ) |>
  mutate(
    demographic_matched = TRUE,
    birth_year = suppressWarnings(as.integer(as.character(as_factor(raw_birth_year))))
  )

knowledge <- raw_knowledge |>
  transmute(
    raw_respondent_id = identifier,
    knowledge_matched = TRUE,
    knowledge_survey_completed = status == "COMPLETED",
    qk1, qk2, qk3, qk4, qk5
  )

analysis <- survey |>
  mutate(raw_choice = raw_choice, raw_respondent_id = uniq) |>
  left_join(demographic, by = "raw_respondent_id", relationship = "many-to-one") |>
  left_join(knowledge, by = "raw_respondent_id", relationship = "many-to-one")

assert_rows(analysis, 2330L, "Luth post-join sample")

analysis <- analysis |>
  arrange(rid) |>
  mutate(
    case_id = sprintf("luth_%04d", row_number()),
    attention_pass = qcheck == 5,
    condition = as.integer(qra),
    wording_id = as.integer((condition + 1L) %/% 2L),
    frame = if_else(condition %% 2L == 0L, "Loss frame", "Gain frame"),
    loss_frame = as.integer(frame == "Loss frame"),
    certain_choice = luth_certain_choice(raw_choice, condition),
    instrument_valid = !condition %in% c(7L, 8L),
    education = education_category(raw_education, c(7L, 8L, 11L, 12L)),
    political_knowledge = score_knowledge(
      qk1, qk2, qk3, qk4, qk5,
      !is.na(knowledge_survey_completed) & knowledge_survey_completed
    ),
    demographic_matched = coalesce(demographic_matched, FALSE),
    knowledge_matched = coalesce(knowledge_matched, FALSE),
    knowledge_survey_completed = coalesce(knowledge_survey_completed, FALSE)
  ) |>
  join_arm_registry(sample_value = "Luth volunteer panel") |>
  # The released file carries only what the analysis consumes; other
  # demographics stay in the restricted sources.
  select(
    case_id, sample, attention_pass, condition, wording_id, wording, family,
    family_label, variant, effect_id, internal_flip_code, explicit_balance_code,
    response_order_code, certain_survivors, display_order, frame, loss_frame,
    certain_choice, instrument_valid, demographic_matched, knowledge_matched,
    education, political_knowledge, knowledge_survey_completed
  )

stopifnot(sum(is.na(analysis$certain_choice)) == 9L)
stopifnot(sum(is.na(analysis$certain_choice) & analysis$attention_pass) == 6L)
stopifnot(sum(analysis$attention_pass) == 2017L)

output_path <- here("data", "luth", "luth_respondent_2009.parquet")
write_parquet(analysis, output_path)

build_log <- c(
  sprintf("raw survey rows: %d", nrow(raw_survey)),
  sprintf("completed experiment rows retained: %d", nrow(analysis)),
  sprintf("attention-check analysis sample: %d", sum(analysis$attention_pass)),
  sprintf("demographic matches: %d", sum(analysis$demographic_matched %in% TRUE)),
  sprintf("political-survey matches: %d", sum(analysis$knowledge_matched %in% TRUE)),
  sprintf(
    "completed political-survey matches: %d",
    sum(analysis$knowledge_survey_completed %in% TRUE)
  ),
  sprintf("invalid natural-frequency pair rows: %d", sum(!analysis$instrument_valid)),
  paste(
    "condition 8 is invalid because the deployed stem names Programs C and D",
    "while its response options name Programs A and B"
  )
)
writeLines(build_log, here("logs", "luth_build.txt"))
message("Created: ", output_path)
