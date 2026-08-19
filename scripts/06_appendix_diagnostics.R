# 06_appendix_diagnostics.R
# Generate sample-flow, allocation, and panel-retention appendix tables.
# Outputs: appendix diagnostic files in tabs/.

library(arrow)
library(dplyr)
library(here)
library(kableExtra)
library(knitr)
library(readr)
library(tidyr)

source(here("scripts", "00_config.R"))
source(here("scripts", "00_utils.R"))

ffrisp <- read_parquet(here("data", "ffrisp", "ffrisp_respondent_2009.parquet"))
luth <- read_parquet(here("data", "luth", "luth_respondent_2009.parquet"))
archive_2018 <- read_parquet(here("data", "protzko", "protzko_respondent.parquet"))

sample_flow <- read_parquet(here("tabs", "sample_flow.parquet")) |>
  mutate(N = format(n, big.mark = ",")) |>
  select(Sample = sample, Stage = stage, N)

sample_flow |>
  kable(
    format = "latex",
    booktabs = TRUE,
    align = c("l", "l", "r"),
    caption = "Analysis sample flow",
    label = "sample-flow",
    linesep = ""
  ) |>
  kable_styling(position = "center", latex_options = "HOLD_position") |>
  column_spec(1, width = "3.2cm") |>
  column_spec(2, width = "6.6cm") |>
  column_spec(3, width = "1.2cm") |>
  footnote(
    general = paste(
      "Note: FFRISP's numeric-identifier step removes four test or nonpanel records.",
      "The Luth valid-pair step removes the defective natural-frequency pair.",
      "The final rows identify the primary analytic sample for each survey.",
      "The Luth sensitivity row retains completed respondents with a valid outcome",
      "without the attention screen. The 2018 rows show source-to-analysis attrition",
      "separately for each of the three study exports."
    ),
    footnote_as_chunk = TRUE,
    general_title = "",
    threeparttable = TRUE
  ) |>
  save_kable(here("tabs", "sample_flow.tex"))

cell_counts <- bind_rows(
  ffrisp |>
    transmute(
      sample = "FFRISP",
      wording_id,
      wording,
      frame = if_else(frame == "Gain frame", "gain", "loss"),
      completed = TRUE,
      analyzed = instrument_valid & !is.na(certain_choice),
      instrument_valid
    ),
  luth |>
    transmute(
      sample = "Luth",
      wording_id,
      wording,
      frame = if_else(frame == "Gain frame", "gain", "loss"),
      completed = TRUE,
      analyzed = attention_pass & instrument_valid & !is.na(certain_choice),
      instrument_valid
    ),
  archive_2018 |>
    transmute(
      sample = study,
      wording_id,
      wording,
      frame = if_else(gain_frame == 1L, "gain", "loss"),
      completed = TRUE,
      analyzed = analysis_complete,
      instrument_valid = TRUE
    )
) |>
  group_by(sample, wording_id, wording, frame) |>
  summarize(
    completed = sum(completed),
    analyzed = sum(analyzed),
    instrument_valid = all(instrument_valid),
    .groups = "drop"
  ) |>
  pivot_wider(
    names_from = frame,
    values_from = c(completed, analyzed),
    names_glue = "{.value}_{frame}"
  ) |>
  arrange(
    factor(
      sample,
      levels = c(
        "FFRISP", "Luth", "Stanford discovery", "Stanford replication",
        "UCSB replication"
      )
    ),
    wording_id
  )

write_parquet(cell_counts, here("tabs", "condition_counts.parquet"))

cell_table <- cell_counts |>
  mutate(
    `Analyzed gain` = if_else(
      instrument_valid,
      as.character(analyzed_gain),
      "Excluded"
    ),
    `Analyzed loss` = if_else(
      instrument_valid,
      as.character(analyzed_loss),
      "Excluded"
    )
  ) |>
  select(
    Sample = sample,
    Wording = wording,
    `Completed gain` = completed_gain,
    `Completed loss` = completed_loss,
    `Analyzed gain`,
    `Analyzed loss`
  )

cell_table |>
  kable(
    format = "latex",
    booktabs = TRUE,
    align = c("l", "l", "r", "r", "r", "r"),
    caption = "Completed and analyzed respondents by wording pair and design cell",
    label = "condition-counts",
    linesep = "",
    col.names = c("Sample", "Wording", "Gain", "Loss", "Gain", "Loss")
  ) |>
  add_header_above(c(" " = 2, "Completed" = 2, "Analyzed" = 2)) |>
  kable_styling(
    position = "center",
    font_size = 8,
    latex_options = "HOLD_position"
  ) |>
  footnote(
    general = paste(
      "Note: Completed counts use the canonical exports after removal of four FFRISP",
      "test or nonpanel records. Analyzed counts require a nonmissing outcome and,",
      "for Luth, passage of the pretreatment attention check. The 2018 rows show",
      "all four factorial cells within each study export. Dashes mark the",
      "excluded pair whose deployed stem and response labels named different programs."
    ),
    footnote_as_chunk = TRUE,
    general_title = "",
    threeparttable = TRUE
  ) |>
  save_kable(here("tabs", "condition_counts.tex"))

fieldwork <- read_csv(
  here("data", "ffrisp", "fieldwork.csv"),
  col_types = cols(
    wave = col_integer(),
    month = col_character(),
    host = col_character(),
    completes = col_integer(),
    percent_of_enrolled = col_double()
  )
)
stopifnot(nrow(fieldwork) == 12L, fieldwork$completes[[12]] == 902L)
retention_table <- fieldwork |>
  mutate(
    Completes = format(completes, big.mark = ","),
    `Percent of 1,000` = sprintf("%.1f%%", percent_of_enrolled)
  ) |>
  select(Wave = wave, Month = month, Host = host, Completes, `Percent of 1,000`)

retention_table |>
  kable(
    format = "latex",
    booktabs = TRUE,
    align = c("r", "l", "l", "r", "r"),
    caption = "FFRISP monthly survey retention",
    label = "ffrisp-retention",
    linesep = ""
  ) |>
  kable_styling(position = "center", latex_options = "HOLD_position") |>
  footnote(
    general = paste(
      "Note: Source is Table 1 of the archived FFRISP Final Methodology Report.",
      "Percentages retain one decimal place here; the report rounded them to whole",
      "percentages."
    ),
    footnote_as_chunk = TRUE,
    general_title = "",
    threeparttable = TRUE
  ) |>
  save_kable(here("tabs", "ffrisp_retention.tex"))

# The wording documentation lives in a curated file at deployment grain: the
# three FFRISP pairs, the nine Luth pairs, and the four 2018 design cells
# (identical across the three study exports, whose arm identity the build
# corroborates against the recorded block labels). Condition labels come from
# the arm registry so the inventory cannot drift from the results exhibits.
arm_wording <- read_csv(
  here("data", "wording", "arm_wording.csv"),
  col_types = cols(
    wording_id = col_integer(),
    .default = col_character()
  )
)

registry_deployments <- arm_registry |>
  mutate(
    sample_key = case_when(
      sample == "FFRISP probability panel" ~ "FFRISP",
      sample == "Luth volunteer panel" ~ "Luth",
      .default = "2018 factorial"
    )
  ) |>
  distinct(
    sample_key, wording_id, wording, family, family_label, variant,
    display_order, valid
  )

stopifnot(nrow(registry_deployments) == 16L)

wording_inventory <- arm_wording |>
  left_join(
    registry_deployments,
    by = c("sample_key", "wording_id"),
    relationship = "one-to-one"
  ) |>
  mutate(sample = sample_key) |>
  arrange(
    factor(sample_key, levels = c("FFRISP", "Luth", "2018 factorial")),
    wording_id
  )

stopifnot(
  !anyNA(wording_inventory$wording),
  identical(wording_inventory$status == "Excluded", !wording_inventory$valid)
)

wording_inventory <- wording_inventory |>
  select(
    sample, wording_id, wording, family, family_label, variant,
    verbatim_status, source_document, gain_certain, loss_certain,
    other_change, status, gain_text, loss_text, stem_note
  )

write_parquet(wording_inventory, here("tabs", "wording_inventory.parquet"))

wording_inventory |>
  select(
    Sample = sample,
    Wording = wording,
    `Certain option: gain` = gain_certain,
    `Certain option: loss` = loss_certain,
    `Other deployed feature` = other_change,
    Status = status
  ) |>
  kable(
    format = "latex",
    booktabs = TRUE,
    longtable = TRUE,
    caption = "Deployed wording-pair and design-cell inventory",
    label = "wording-inventory",
    linesep = "",
    align = rep("l", 6)
  ) |>
  kable_styling(
    font_size = 8,
    latex_options = "repeat_header"
  ) |>
  column_spec(1, width = "1.3cm") |>
  column_spec(2, width = "3.2cm") |>
  column_spec(3:4, width = "3.6cm") |>
  column_spec(5, width = "6.4cm") |>
  column_spec(6, width = "1.5cm") |>
  save_kable(here("tabs", "wording_inventory.tex"))

verbatim_rows <- wording_inventory |>
  transmute(
    Sample = sample,
    Wording = wording,
    Record = verbatim_status,
    `Gain-frame options` = gain_text,
    `Loss-frame options` = loss_text,
    Stem = stem_note
  )

verbatim_rows |>
  kable(
    format = "latex",
    booktabs = TRUE,
    longtable = TRUE,
    caption = "Deployed and reconstructed treatment text",
    label = "verbatim-wording",
    linesep = "\\addlinespace",
    align = rep("l", 6)
  ) |>
  kable_styling(
    font_size = 8,
    latex_options = "repeat_header"
  ) |>
  column_spec(1, width = "1.2cm") |>
  column_spec(2, width = "2.6cm") |>
  column_spec(3, width = "1.7cm") |>
  column_spec(4:5, width = "6.2cm") |>
  column_spec(6, width = "2.6cm") |>
  footnote(
    general = paste(
      "Note: Option texts separate the certain and risky options with ||.",
      "Verbatim rows quote the archived deployed item text; reconstructed",
      "rows quote the archived draft descriptions checked against the",
      "deployed questionnaire PDFs; functional rows describe the 2018 cells",
      "from the recorded factor coding and Qualtrics block labels because the",
      "fielded text is unavailable."
    ),
    footnote_as_chunk = TRUE,
    general_title = "",
    threeparttable = FALSE
  ) |>
  save_kable(here("tabs", "verbatim_wording.tex"))

# kableExtra renders a longtable footnote as \multicolumn{6}{l}{...}, whose
# unconstrained natural width would widen the whole table to the note's
# one-line length. Constrain the note cell so it wraps at the table width.
verbatim_tex <- readLines(here("tabs", "verbatim_wording.tex"))
verbatim_tex <- sub(
  "\\multicolumn{6}{l}{\\rule{0pt}{1em}",
  "\\multicolumn{6}{@{}p{21.0cm}@{}}{\\rule{0pt}{1em}",
  verbatim_tex,
  fixed = TRUE
)
writeLines(verbatim_tex, here("tabs", "verbatim_wording.tex"))

message("Created: tabs/verbatim_wording.tex")
message("Created: tabs/sample_flow.tex")
message("Created: tabs/condition_counts.parquet")
message("Created: tabs/condition_counts.tex")
message("Created: tabs/ffrisp_retention.tex")
message("Created: tabs/wording_inventory.parquet")
message("Created: tabs/wording_inventory.tex")
