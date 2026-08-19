# 00_utils.R
# Assertions, recodes, and output helpers shared by the pipeline.
# Output: none

assert_file <- function(path) {
  if (!file.exists(path)) {
    stop("Required file not found: ", path, call. = FALSE)
  }
}

verify_source_manifest <- function(source_dir) {
  manifest_path <- file.path(source_dir, "manifest.csv")
  assert_file(manifest_path)
  manifest <- readr::read_csv(manifest_path, show_col_types = FALSE)
  required_columns <- c("file", "sha256", "bytes")
  if (!all(required_columns %in% names(manifest))) {
    stop(
      "Source manifest must contain: ",
      paste(required_columns, collapse = ", "),
      call. = FALSE
    )
  }

  paths <- file.path(source_dir, manifest$file)
  purrr::walk(paths, assert_file)
  actual_bytes <- as.numeric(file.info(paths)$size)
  actual_sha256 <- purrr::map_chr(paths, digest::digest, file = TRUE, algo = "sha256")
  mismatch <- actual_bytes != manifest$bytes | actual_sha256 != manifest$sha256
  if (any(mismatch)) {
    stop(
      "Source manifest mismatch: ",
      paste(manifest$file[mismatch], collapse = ", "),
      call. = FALSE
    )
  }

  invisible(TRUE)
}

# TRUE only when a manifest exists, lists at least one file, and every listed
# file is present. Used by the runner to decide whether a build stage can run
# or the released respondent files must be used instead.
sources_present <- function(manifest_path) {
  if (!file.exists(manifest_path)) {
    return(FALSE)
  }
  manifest <- utils::read.csv(manifest_path)
  if (is.null(manifest$file) || nrow(manifest) == 0L) {
    return(FALSE)
  }
  all(file.exists(file.path(dirname(manifest_path), manifest$file)))
}

assert_unique <- function(data, key, label) {
  if (anyDuplicated(data[key])) {
    stop(label, " is not unique on: ", paste(key, collapse = ", "), call. = FALSE)
  }
}

assert_rows <- function(data, expected, label) {
  if (nrow(data) != expected) {
    stop(label, " has ", nrow(data), " rows; expected ", expected, call. = FALSE)
  }
}

education_category <- function(x, cutpoints) {
  dplyr::case_when(
    is.na(x) ~ NA_character_,
    x <= cutpoints[[1]] ~ "Less than high school",
    x == cutpoints[[2]] ~ "High school",
    x <= cutpoints[[3]] ~ "Some college or associate degree",
    x >= cutpoints[[4]] ~ "Bachelor's degree or higher",
    .default = NA_character_
  )
}

luth_certain_choice <- function(raw_choice, condition) {
  dplyr::case_when(
    is.na(raw_choice) ~ NA_integer_,
    condition %in% c(9L, 10L, 17L, 18L) ~ as.integer(raw_choice == 2),
    .default = as.integer(raw_choice == 1)
  )
}

score_knowledge <- function(qk1, qk2, qk3, qk4, qk5, completed) {
  qk1_normalized <- gsub("[^a-z]", "", tolower(as.character(qk1)))
  qk3_text <- trimws(as.character(qk3))
  qk3_number <- suppressWarnings(as.numeric(gsub("%", "", qk3_text)))
  item_1 <- as.integer(startsWith(qk1_normalized, "vicepres") | qk1_normalized == "vp")
  item_2 <- as.integer(qk2 == 3)
  item_3 <- as.integer(qk3_text == "2/3" | dplyr::between(qk3_number, 65, 68))
  item_4 <- as.integer(qk4 == 2)
  item_5 <- as.integer(qk5 == 1)
  items <- cbind(item_1, item_2, item_3, item_4, item_5)
  items[is.na(items)] <- 0L
  score <- rowMeans(items)
  score[!completed] <- NA_real_
  score
}

latex_escape <- function(x) {
  x <- gsub("&", "\\\\&", x, fixed = TRUE)
  x <- gsub("%", "\\\\%", x, fixed = TRUE)
  x <- gsub("_", "\\\\_", x, fixed = TRUE)
  x
}

fmt_pct <- function(x) sprintf("%.1f", 100 * x)
fmt_pp <- function(x) sprintf("%.1f", 100 * x)
