# 09_protzko_build.R
# Build the deidentified Protzko analysis file from the three study exports.
# Outputs: data/protzko/protzko_respondent.parquet, reconstruction and build logs

library(arrow)
library(dplyr)
library(haven)
library(here)
library(purrr)
library(readr)

source(here("scripts", "00_config.R"))
source(here("scripts", "00_utils.R"))

source_dir <- here("data", "protzko", "source")
verify_source_manifest(source_dir)

discovery_path <- file.path(source_dir, "StanfordProspectwithinsubjects2x2.dta")
replication_path <- file.path(
  source_dir,
  "StanfordProspectwithinsubjects2x2REPLICATE.dta"
)
ucsb_path <- file.path(source_dir, "UCSBreplicateexplicitbalance.dta")
combined_path <- file.path(source_dir, "study2prospecttheorycombinedanon.dta")

raw_discovery <- read_dta(discovery_path)
raw_replication <- read_dta(replication_path)
raw_ucsb <- read_dta(ucsb_path)
received_combined <- read_dta(combined_path)

assert_rows(raw_discovery, 1647L, "Protzko Stanford discovery source")
assert_rows(raw_replication, 1442L, "Protzko Stanford replication source")
assert_rows(raw_ucsb, 1500L, "Protzko UCSB replication source")
assert_rows(received_combined, 4590L, "Protzko received combined file")

derive_stanford <- function(data, study_code, study_label) {
  data |>
    transmute(
      case_id = sprintf("%s_%04d", study_code, row_number()),
      study = study_label,
      source_row = row_number(),
      certain_choice = as.integer(na_if(.data$certain1st, -99)),
      gain_frame = as.integer(.data$gainframe1st),
      internal_flip = as.integer(.data$flip),
      explicit_balance = as.integer(.data$explicit),
      education = as.numeric(na_if(.data$education, -99)),
      female = as.integer(na_if(.data$female, -99)),
      age = 2018L - as.numeric(na_if(.data$birthyear, -99))
    )
}

discovery <- derive_stanford(
  raw_discovery,
  "protzko_discovery",
  "Stanford discovery"
)
replication <- derive_stanford(
  raw_replication,
  "protzko_replication",
  "Stanford replication"
)
ucsb <- raw_ucsb |>
  transmute(
    case_id = sprintf("protzko_ucsb_%04d", row_number()),
    study = "UCSB replication",
    source_row = row_number(),
    certain_choice = as.integer(.data$certain),
    gain_frame = as.integer(.data$gainframe),
    internal_flip = as.integer(.data$flip),
    explicit_balance = as.integer(.data$explicit),
    education = as.numeric(na_if(.data$education, -99)),
    female = as.integer(na_if(.data$female, -99)),
    age = as.numeric(.data$age)
  )

# The Stanford exports carry Qualtrics block-order fields (DO-BR-FL) whose
# values name the deployed arm and the frame block order, e.g.
# "internal flip - gain|internal flip - loss". Exactly one is populated per
# respondent. They are the only treatment-arm labels recorded in the 2018
# sources, so we use them to corroborate the derived factor coding. The
# deployed labels include a typo ("Excplicit balancing"), hence the loose
# keyword matching.
parse_block_arm <- function(value) {
  normalized <- tolower(trimws(value))
  arm <- rep(NA_character_, length(normalized))
  arm[grepl("original", normalized)] <- "original"
  arm[grepl("balanc", normalized)] <- "balanced"
  arm[grepl("internal flip", normalized)] <- "risky_flip"
  arm[grepl("plus internal flip", normalized)] <- "balanced_flip"
  arm
}

validate_stanford_blocks <- function(raw_data, derived, study_label) {
  block_fields <- as.matrix(select(raw_data, starts_with("dobrfl_")))
  stopifnot(ncol(block_fields) == 4L)
  populated <- !is.na(block_fields) & trimws(block_fields) != ""
  stopifnot(all(rowSums(populated) == 1L))
  block_value <- block_fields[cbind(
    seq_len(nrow(block_fields)),
    max.col(populated)
  )]
  observed_arm <- parse_block_arm(block_value)
  expected_arm <- case_when(
    derived$explicit_balance == 1L & derived$internal_flip == 1L ~ "balanced_flip",
    derived$explicit_balance == 1L ~ "balanced",
    derived$internal_flip == 1L ~ "risky_flip",
    .default = "original"
  )
  first_block <- vapply(
    strsplit(tolower(block_value), "|", fixed = TRUE),
    function(x) x[[1]],
    character(1)
  )
  observed_gain_first <- as.integer(grepl("gain", first_block))
  stopifnot(
    all(observed_arm == expected_arm),
    all(observed_gain_first == derived$gain_frame)
  )
  sprintf(
    "%s: %d block-order labels agree with derived arm and frame coding",
    study_label,
    nrow(raw_data)
  )
}

block_check_log <- c(
  validate_stanford_blocks(raw_discovery, discovery, "Stanford discovery"),
  validate_stanford_blocks(raw_replication, replication, "Stanford replication")
)

# The UCSB export has no block-order fields; its precomputed interaction
# columns are the available internal consistency check on the factor coding.
stopifnot(
  all(as.integer(raw_ucsb$gainflip) == ucsb$gain_frame * ucsb$internal_flip),
  all(as.integer(raw_ucsb$gainbal) == ucsb$gain_frame * ucsb$explicit_balance),
  all(
    as.integer(raw_ucsb$gainflbal) ==
      ucsb$gain_frame * ucsb$internal_flip * ucsb$explicit_balance
  )
)
block_check_log <- c(
  block_check_log,
  sprintf(
    "UCSB replication: %d precomputed interaction columns agree with derived factor coding",
    nrow(ucsb)
  )
)

analysis <- bind_rows(discovery, replication, ucsb) |>
  mutate(
    loss_frame = 1L - gain_frame,
    education_index = case_when(
      is.na(education) ~ NA_integer_,
      education <= 8 ~ 0L,
      education == 9 ~ 1L,
      education <= 11 ~ 2L,
      education >= 12 ~ 3L,
      .default = NA_integer_
    ),
    analysis_complete = !is.na(certain_choice),
    wording_id = explicit_balance * 2L + internal_flip + 1L,
    sample = study
  ) |>
  join_arm_registry() |>
  # The released file carries only what the analysis consumes; raw education,
  # gender, age, and source row positions stay in the restricted sources.
  select(-sample, -education, -female, -age, -source_row)

assert_rows(analysis, 4589L, "Protzko source-derived respondent file")
assert_unique(analysis, "case_id", "Protzko source-derived respondent file")
stopifnot(sum(!analysis$analysis_complete) == 4L)
stopifnot(
  all(analysis$gain_frame %in% 0:1),
  all(analysis$loss_frame %in% 0:1),
  all(analysis$internal_flip %in% 0:1),
  all(analysis$explicit_balance %in% 0:1),
  all(analysis$certain_choice[analysis$analysis_complete] %in% 0:1)
)

write_parquet(
  analysis,
  here("data", "protzko", "protzko_respondent.parquet")
)

source_flow <- analysis |>
  group_by(study) |>
  summarize(
    source_rows = n(),
    missing_choice = sum(!analysis_complete),
    analysis_rows = sum(analysis_complete),
    .groups = "drop"
  ) |>
  bind_rows(
    summarize(
      analysis,
      study = "Total",
      source_rows = n(),
      missing_choice = sum(!analysis_complete),
      analysis_rows = sum(analysis_complete)
    )
  )
write_parquet(source_flow, here("tabs", "protzko_source_flow.parquet"))

legacy_expected <- bind_rows(replication, discovery, ucsb) |>
  transmute(
    certain = as.numeric(certain_choice),
    gainframe = as.numeric(gain_frame),
    flip = as.numeric(internal_flip),
    explicit = as.numeric(explicit_balance)
  )
received_nonblank <- received_combined |>
  slice_head(n = nrow(legacy_expected)) |>
  mutate(across(everything(), as.numeric))

different_value <- function(expected, received) {
  xor(is.na(expected), is.na(received)) |
    (!is.na(expected) & !is.na(received) & expected != received)
}

comparison <- map2_dfc(
  legacy_expected,
  received_nonblank,
  different_value
)
names(comparison) <- names(legacy_expected)
different_rows <- which(rowSums(comparison) > 0L)

stopifnot(
  length(different_rows) == 1L,
  different_rows == 3089L,
  comparison$certain[[3089L]],
  sum(comparison) == 1L,
  all(is.na(received_combined[4590L, ]))
)

reconstruction <- tibble(
  combined_row = different_rows,
  study = analysis$study[analysis$case_id == "protzko_discovery_1647"],
  source_row = 1647L,
  variable = "certain",
  source_value = NA_integer_,
  received_combined_value = as.integer(received_nonblank$certain[different_rows]),
  interpretation = paste(
    "Received combined file changes one missing first choice to zero;",
    "all other nonblank cells match the source-derived stack."
  )
)
write_parquet(reconstruction, here("tabs", "protzko_reconstruction.parquet"))

build_log <- c(
  sprintf("source rows: %d", nrow(analysis)),
  sprintf("source-derived complete cases: %d", sum(analysis$analysis_complete)),
  sprintf("source-derived missing choices: %d", sum(!analysis$analysis_complete)),
  sprintf("received combined rows: %d", nrow(received_combined)),
  sprintf("received combined complete cases: %d", sum(complete.cases(received_combined))),
  "received combined trailing blank rows: 1",
  "received combined cell differences from source-derived stack: 1",
  "difference: row 3089 certain choice is missing in source and zero in combined",
  block_check_log
)
write_lines(build_log, here("logs", "protzko_build.txt"))

message("Created: data/protzko/protzko_respondent.parquet")
message("Created: tabs/protzko_source_flow.parquet")
message("Created: tabs/protzko_reconstruction.parquet")
message("Created: logs/protzko_build.txt")
