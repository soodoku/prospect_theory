# 07_open_ended_audit.R
# Audit independent coding of Luth open-ended explanations without exporting text.
# Outputs: reliability files in tabs/.

library(dplyr)
library(arrow)
library(estimatr)
library(here)
library(kableExtra)
library(knitr)
library(readr)
library(readxl)

source(here("scripts", "00_utils.R"))

source_dir <- here("data", "luth", "source", "open_ended")
verify_source_manifest(source_dir)
coding_path <- file.path(source_dir, "all.luth.open.ended.xls")

coding <- read_excel(coding_path) |>
  transmute(
    anne_primary = suppressWarnings(as.integer(anne.code1)),
    anne_secondary = suppressWarnings(as.integer(anne.code2)),
    jeremy_primary = suppressWarnings(as.integer(jeremy.code1)),
    jeremy_secondary = suppressWarnings(as.integer(jeremy.code2))
  )
stopifnot(nrow(coding) == 2331L, all(is.na(coding[nrow(coding), ])))

luth <- read_parquet(here("data", "luth", "luth_respondent_2009.parquet"))
stopifnot(nrow(luth) == 2330L)

valid_code <- function(x) !is.na(x) & x %in% 1:8

cohen_kappa <- function(x, y, levels = sort(unique(c(x, y)))) {
  x <- factor(x, levels = levels)
  y <- factor(y, levels = levels)
  observed <- mean(x == y)
  joint <- table(x, y) / length(x)
  expected <- sum(rowSums(joint) * colSums(joint))
  (observed - expected) / (1 - expected)
}

double_coded <- coding |>
  filter(valid_code(anne_primary), valid_code(jeremy_primary))
stopifnot(nrow(double_coded) == 2295L)

anne_sets <- Map(
  function(primary, secondary) {
    assigned <- c(primary, secondary)
    sort(unique(assigned[valid_code(assigned)]))
  },
  double_coded$anne_primary,
  double_coded$anne_secondary
)
jeremy_sets <- Map(
  function(primary, secondary) {
    assigned <- c(primary, secondary)
    sort(unique(assigned[valid_code(assigned)]))
  },
  double_coded$jeremy_primary,
  double_coded$jeremy_secondary
)
set_agreement <- mapply(identical, anne_sets, jeremy_sets)
set_overlap <- mapply(function(x, y) length(intersect(x, y)) > 0L, anne_sets, jeremy_sets)

overall <- tibble(
  measure = c(
    "Primary-code exact agreement",
    "Full code-set exact agreement",
    "At least one assigned code overlaps"
  ),
  n = nrow(double_coded),
  agreement = c(
    mean(double_coded$anne_primary == double_coded$jeremy_primary),
    mean(set_agreement),
    mean(set_overlap)
  ),
  kappa = c(
    cohen_kappa(double_coded$anne_primary, double_coded$jeremy_primary, 1:8),
    NA_real_,
    NA_real_
  )
)
write_parquet(overall, here("tabs", "open_ended_reliability.parquet"))

category_labels <- c(
  "No stated reason",
  "Equal expected value",
  "Certainty or risk aversion",
  "Risk offers chance all survive",
  "Saves more people",
  "Easier to understand",
  "Other",
  "Better choice without a reason"
)

category_reliability <- lapply(seq_along(category_labels), function(code) {
  anne <- as.integer(double_coded$anne_primary == code)
  jeremy <- as.integer(double_coded$jeremy_primary == code)
  tibble(
    code,
    category = category_labels[[code]],
    anne_n = sum(anne),
    jeremy_n = sum(jeremy),
    agreement = mean(anne == jeremy),
    kappa = cohen_kappa(anne, jeremy, 0:1)
  )
}) |>
  bind_rows()
write_parquet(category_reliability, here("tabs", "open_ended_category_reliability.parquet"))

category_labels <- c(
  "No stated reason", "Equal expected value", "Certainty or risk aversion",
  "Risk offers chance all survive", "Saves more people", "Easier to understand",
  "Other", "Better choice without a reason"
)

explanations <- luth |>
  mutate(row_id = row_number()) |>
  filter(attention_pass, instrument_valid, !is.na(certain_choice)) |>
  mutate(primary_code = coding$anne_primary[row_id]) |>
  filter(valid_code(primary_code))

explanation_effects <- lapply(seq_along(category_labels), function(code) {
  data <- explanations |>
    mutate(category_present = as.integer(primary_code == code))
  model <- lm_robust(
    category_present ~ loss_frame + factor(wording_id),
    data = data,
    se_type = "HC2"
  )
  term <- broom::tidy(model, conf.int = TRUE) |>
    filter(term == "loss_frame")
  gain_pct <- mean(data$category_present[data$loss_frame == 0]) * 100
  loss_pct <- mean(data$category_present[data$loss_frame == 1]) * 100
  tibble(
    code = code,
    category = category_labels[[code]],
    n = nrow(data),
    gain_pct = gain_pct,
    loss_pct = loss_pct,
    loss_minus_gain_pp = 100 * term$estimate,
    conf_low = 100 * term$conf.low,
    conf_high = 100 * term$conf.high,
    p_value = term$p.value
  )
}) |>
  bind_rows() |>
  mutate(holm_p_value = p.adjust(p_value, method = "holm"))

write_parquet(explanation_effects, here("tabs", "open_ended_frame_effects.parquet"))

certainty_reason <- explanation_effects |>
  filter(code == 3L)
chance_reason <- explanation_effects |>
  filter(code == 4L)
open_ended_macros <- c(
  sprintf("\\newcommand{\\OpenEndedN}{%s}", format(nrow(explanations), big.mark = ",")),
  sprintf("\\newcommand{\\OpenEndedCertaintyGain}{%.1f}", certainty_reason$gain_pct),
  sprintf("\\newcommand{\\OpenEndedCertaintyLoss}{%.1f}", certainty_reason$loss_pct),
  sprintf("\\newcommand{\\OpenEndedCertaintyDiff}{%.1f}", certainty_reason$loss_minus_gain_pp),
  sprintf("\\newcommand{\\OpenEndedChanceGain}{%.1f}", chance_reason$gain_pct),
  sprintf("\\newcommand{\\OpenEndedChanceLoss}{%.1f}", chance_reason$loss_pct),
  sprintf("\\newcommand{\\OpenEndedChanceDiff}{%.1f}", chance_reason$loss_minus_gain_pp)
)
write_lines(open_ended_macros, here("tabs", "open_ended_macros.tex"))

explanation_effects |>
  mutate(
    `Gain %` = sprintf("%.1f", gain_pct),
    `Loss %` = sprintf("%.1f", loss_pct),
    `Loss - gain [95% CI]` = sprintf(
      "%.1f [%.1f, %.1f]", loss_minus_gain_pp, conf_low, conf_high
    ),
    p = sprintf("%.3f", p_value),
    `Holm p` = sprintf("%.3f", holm_p_value)
  ) |>
  select(Category = category, N = n, `Gain %`, `Loss %`, `Loss - gain [95% CI]`, p, `Holm p`) |>
  kable(
    format = "latex", booktabs = TRUE,
    align = c("l", "r", "r", "r", "r", "r", "r"),
    caption = "Framing differences in coded post-choice explanations",
    label = "open-ended-frame-effects", linesep = ""
  ) |>
  kable_styling(position = "center", font_size = 8, latex_options = "HOLD_position") |>
  footnote(
    general = paste(
      "Note: Percentages use Anne's primary code among attention-check passers",
      "with a valid instrument and framing outcome. Models adjust for wording pair",
      "and use HC2 standard errors. Codes classify explanations after the choice;",
      "they are descriptive and do not identify a causal mechanism."
    ),
    footnote_as_chunk = TRUE, general_title = "", threeparttable = TRUE
  ) |>
  save_kable(here("tabs", "open_ended_frame_effects.tex"))

reliability_table <- overall |>
  mutate(
    N = format(n, big.mark = ","),
    Agreement = sprintf("%.1f%%", 100 * agreement),
    `Cohen's kappa` = if_else(is.na(kappa), "--", sprintf("%.3f", kappa))
  ) |>
  select(Measure = measure, N, Agreement, `Cohen's kappa`)

category_table <- category_reliability |>
  mutate(
    `Binary kappa` = sprintf("%.3f", kappa)
  ) |>
  select(
    Code = code,
    Category = category,
    `Anne n` = anne_n,
    `Jeremy n` = jeremy_n,
    `Binary kappa`
  )

reliability_table |>
  kable(
    format = "latex",
    booktabs = TRUE,
    align = c("l", "r", "r", "r"),
    caption = "Reliability of the archived open-ended coding",
    label = "open-ended-reliability",
    linesep = ""
  ) |>
  kable_styling(
    position = "center",
    font_size = 9,
    latex_options = "HOLD_position"
  ) |>
  footnote(
    general = paste(
      "Note: Anne and Jeremy supplied the two code sets. Primary-code statistics use",
      "responses with valid primary codes from both coders. Each coder could add a",
      "secondary category. Dashes mark set-based measures without a single",
      "multicategory Cohen's kappa in this audit."
    ),
    footnote_as_chunk = TRUE,
    general_title = "",
    threeparttable = TRUE
  ) |>
  save_kable(here("tabs", "open_ended_reliability.tex"))

category_table |>
  kable(
    format = "latex",
    booktabs = TRUE,
    align = c("r", "l", "r", "r", "r"),
    caption = "Category-specific reliability of primary open-ended codes",
    label = "open-ended-categories",
    linesep = ""
  ) |>
  kable_styling(
    position = "center",
    font_size = 9,
    latex_options = "HOLD_position"
  ) |>
  footnote(
    general = paste(
      "Note: Counts and binary kappa values use primary codes only. The instructions",
      "allowed multiple categories. A contemporaneous memo identifies overlap among",
      "codes 3, 4, and 5 and ambiguity involving codes 1, 7, and 8."
    ),
    footnote_as_chunk = TRUE,
    general_title = "",
    threeparttable = TRUE
  ) |>
  save_kable(here("tabs", "open_ended_category_reliability.tex"))

message("Created: tabs/open_ended_reliability.parquet")
message("Created: tabs/open_ended_category_reliability.parquet")
message("Created: tabs/open_ended_reliability.tex")
message("Created: tabs/open_ended_category_reliability.tex")
message("Created: tabs/open_ended_frame_effects.parquet")
message("Created: tabs/open_ended_frame_effects.tex")
message("Created: tabs/open_ended_macros.tex")
