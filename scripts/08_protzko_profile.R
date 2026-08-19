# 08_protzko_profile.R
# Profile the immutable Protzko source files without exporting identifiers.
# Outputs: logs/protzko_data_profile.txt and tabs/protzko_source_cells.csv

library(arrow)
library(dplyr)
library(haven)
library(here)
library(purrr)
library(readr)
library(tidyr)

source(here("scripts", "00_utils.R"))

source_dir <- here("data", "protzko", "source")
verify_source_manifest(source_dir)

source_files <- c(
  "StanfordProspectwithinsubjects2x2.dta",
  "StanfordProspectwithinsubjects2x2REPLICATE.dta",
  "UCSBreplicateexplicitbalance.dta",
  "study2prospecttheorycombinedanon.dta"
)
source_paths <- file.path(source_dir, source_files)
source_data <- set_names(map(source_paths, read_dta), source_files)

sensitive_pattern <- paste(
  c(
    "^v[0-9]+$", "ipaddress", "responseid", "psid", "(^|_)pid($|_)", "zipcode",
    "useragent", "q457", "q459", "q465", "q491", "(^|_)sid$"
  ),
  collapse = "|"
)

display_value <- function(x) {
  if (inherits(x, "Date")) {
    return(format(x, "%Y-%m-%d"))
  }
  as.character(x)
}

top_values <- function(x, variable) {
  if (grepl(sensitive_pattern, tolower(variable), perl = TRUE)) {
    return("withheld: identifier or quasi-identifier")
  }
  observed <- x[!is.na(x)]
  if (length(observed) == 0L) {
    return("")
  }
  counts <- sort(table(display_value(observed), useNA = "no"), decreasing = TRUE)
  counts <- head(counts, 5L)
  paste0(names(counts), " (", as.integer(counts), ")", collapse = "; ")
}

variable_profile <- function(data) {
  map_dfr(names(data), function(variable) {
    x <- data[[variable]]
    observed <- x[!is.na(x)]
    numeric_x <- is.numeric(x) || inherits(x, "Date")
    tibble(
      variable,
      storage = paste(class(x), collapse = "/"),
      label = coalesce(attr(x, "label"), ""),
      n_missing = sum(is.na(x)),
      pct_missing = 100 * mean(is.na(x)),
      n_distinct = n_distinct(observed),
      minimum = if (numeric_x && length(observed) > 0L) {
        display_value(min(observed))
      } else {
        ""
      },
      maximum = if (numeric_x && length(observed) > 0L) {
        display_value(max(observed))
      } else {
        ""
      },
      top_values = top_values(x, variable)
    )
  })
}

analysis_columns <- list(
  StanfordProspectwithinsubjects2x2.dta = c(
    "q1857", "q1865", "q1859", "q1867", "q1861", "q1869", "q1863",
    "q1871", "certain1st", "certain2nd", "gainframe1st", "flip",
    "explicit", "qualified", "duration"
  ),
  StanfordProspectwithinsubjects2x2REPLICATE.dta = c(
    "q1857", "q1865", "q1859", "q1867", "q1861", "q1869", "q1863",
    "q1871", "certain1st", "certain2nd", "gainframe1st", "flip",
    "explicit", "qualified", "duration"
  ),
  UCSBreplicateexplicitbalance.dta = c(
    "certain", "certaincc", "certaince", "certainec", "certainee",
    "gainframe", "flip", "explicit", "gainflip", "gainbal", "gainflbal",
    "age", "Duration__in_seconds_"
  ),
  study2prospecttheorycombinedanon.dta = c(
    "certain", "gainframe", "flip", "explicit"
  )
)

profile_lines <- capture.output({
  cat("Protzko source-data profile\n")
  cat("Generated:", format(Sys.time(), tz = "UTC"), "UTC\n")
  cat("Raw identifiers and quasi-identifiers are not printed.\n")

  walk(names(source_data), function(file) {
    data <- source_data[[file]]
    columns <- intersect(analysis_columns[[file]], names(data))
    cat("\n============================================================\n")
    cat("FILE:", file, "\n")
    cat("SHAPE:", nrow(data), "rows x", ncol(data), "columns\n")
    cat("\nFIRST SIX ROWS, ANALYSIS FIELDS (RAW VALUES)\n")
    print(as.data.frame(head(select(data, all_of(columns)), 6L)), row.names = FALSE)
    cat("\nLAST SIX ROWS, ANALYSIS FIELDS (RAW VALUES)\n")
    print(as.data.frame(tail(select(data, all_of(columns)), 6L)), row.names = FALSE)
    cat("\nVARIABLE PROFILE\n")
    print(as.data.frame(variable_profile(data)), row.names = FALSE)
  })
})

write_lines(profile_lines, here("logs", "protzko_data_profile.txt"))

cell_counts <- imap_dfr(source_data, function(data, file) {
  required <- c("certain", "gainframe", "flip", "explicit")
  if (!all(required %in% names(data))) {
    return(tibble())
  }
  data |>
    count(across(all_of(required)), name = "n") |>
    mutate(file = file, .before = 1L)
})

write_parquet(cell_counts, here("tabs", "protzko_source_cells.parquet"))

message("Created: logs/protzko_data_profile.txt")
message("Created: tabs/protzko_source_cells.parquet")
