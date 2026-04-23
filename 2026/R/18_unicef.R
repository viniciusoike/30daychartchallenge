library(tidyverse)
library(readxl)
import::from(here, here)
import::from(readxl, excel_sheets, read_excel)

paths <- list.files(
  here("2026", "data", "unicef"),
  pattern = "\\.xlsx$",
  full.names = TRUE
)

paths <- paths[!stringr::str_detect(basename(paths), "^~\\$")]

params <- tibble(path = paths)

params <- params |>
  mutate(
    filename = basename(path),
    sheets = purrr::map(path, excel_sheets)
  ) |>
  unnest(cols = c(sheets)) |>
  mutate(
    name_dataset = janitor::make_clean_names(sheets),
    id = row_number()
  )

params <- params |>
  mutate(skip = if_else(id == 1, 1, 0))

import_excel <- function(path, sheet, skip = 2, ...) {
  read_excel(
    path,
    sheet = sheet,
    skip = skip,
    na = c("-", " ", "N/D", "NA"),
    ...
  )
}

import_header <- function(path, sheet, skip = 0, ...) {
  header <- import_excel(
    path,
    sheet,
    skip,
    n_max = 2,
    col_names = FALSE,
    .name_repair = janitor::make_clean_names,
    ...
  )
}

build_col_names <- function(header) {
  all_cols <- header |>
    mutate(header_row = row_number()) |>
    pivot_longer(cols = -header_row, names_to = "column", values_to = "value")

  h1_cols <- all_cols |>
    filter(header_row == 1) |>
    fill(value) |>
    pull(value)

  h2_cols <- all_cols |>
    filter(header_row == 2) |>
    mutate(value = replace_na(value, "")) |>
    pull(value)

  col_names <- paste0(h1_cols, "_", h2_cols)
  col_names <- janitor::make_clean_names(col_names)

  return(col_names)
}

.import_unicef_urbanization <- function() {
  series_params <- params |>
    filter(str_detect(filename, "Urbanization"), skip != 1) |>
    select(path, sheets, name_dataset)

  series <- map2(
    series_params$path,
    series_params$sheets,
    \(x, y) {
      import_excel(x, y, skip = 0, .name_repair = janitor::make_clean_names)
    }
  )
  names(series) <- series_params$name_dataset

  series <- tryCatch(
    bind_rows(series, .id = "dataset"),
    error = function(e) {
      cli::cli_alert_info("Failed to join datasets: {conditionMessage(e)}")
    }
  )

  return(series)
}

.import_unicef_literacy <- function() {
  series_params <- params |>
    filter(str_detect(filename, "Literacy"), skip != 1) |>
    select(path, sheets, name_dataset)

  series <- map2(
    series_params$path,
    series_params$sheets,
    .read_unicef_literacy
  )
  names(series) <- series_params$name_dataset

  series <- tryCatch(
    bind_rows(series, .id = "dataset"),
    error = function(e) {
      cli::cli_alert_info("Failed to join datasets: {conditionMessage(e)}")
    }
  )

  return(series)
}

.read_unicef_literacy <- function(path, sheet, ...) {
  raw_header <- import_excel(path, sheet, ...)
  col_names <- build_col_names(header)
  raw_data <- import_excel(path, sheet, col_names = col_names, ...)
  clean_data <- raw_data |>
    filter(!is.na(iso3))
  return(clean_data)
  # clean_dat <- .clean_unicef_literacy(raw_data)

  # return(clean_dat)
}

literacy <- .import_unicef_literacy()
urbanization <- .import_unicef_urbanization()
