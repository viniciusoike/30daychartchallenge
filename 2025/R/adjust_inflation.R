#' Adjust a series for inflation
#'
#' Converts nominal values into constant prices based on a specific year.
#'
#' @param df A `data.frame` type object with a date column called `date`
#' and a value column called `value`
#' @param base_year A `numeric` specifying which year to use as base. Defaults
#' to `2012`.
#' @param type One of `ipca` (default), `incc`, `igpm` or `igpdi`.
#'
#' @return
#' @export
#' @importFrom dplyr filter mutate select summarise pull inner_join
adjust_inflation <- function(
    df,
    type = "ipca",
    base_year = 2012,
    freq = "month",
    date_col = NULL,
    value_col = NULL) {

  if (is.null(value_col)) {
    value_col <- "value"
  }

  # Check if type is valid
  stopifnot(type %in% c("ipca", "igpdi", "incc", "igpm"))
  # Import the inflation series
  inflation <- get_price_series(type)

  if (freq == "month") {

    date_col = "date"

    # Calculate cumulative change from value (percent change)
    inflation <- inflation |>
      dplyr::filter(!is.na(value)) |>
      dplyr::mutate(cumulative = cumprod(1 + value / 100))
    # Compute the base as the average inflation of the selected year
    base_index <- inflation |>
      dplyr::mutate(ts_year = lubridate::year(ref.date)) |>
      dplyr::filter(ts_year == base_year) |>
      dplyr::summarise(avg = mean(cumulative)) |>
      dplyr::pull(avg)

  } else if (freq == "year") {

    date_col = "ts_year"

    # Compute annual inflation
    inflation <- inflation |>
      dplyr::filter(!is.na(value)) |>
      dplyr::mutate(ts_year = lubridate::year(ref.date)) |>
      dplyr::group_by(ts_year) |>
      dplyr::summarise(year_inflation = prod(1 + value / 100) - 1) %>%
      dplyr::ungroup() |>
      dplyr::mutate(cumulative = cumprod(1 + year_inflation))
    # Get base year inflation
    base_index <- inflation |>
      dplyr::filter(ts_year == base_year) |>
      dplyr::pull(cumulative)
  }

  # Compute the inflation factor
  inflation <- inflation |>
    dplyr::mutate(
      base = local(base_index),
      inflation_factor = base / cumulative
    ) |>
    # Select only the date and inflation factor columns
    dplyr::select(ref.date, inflation_factor) |>
    dplyr::rename(date = ref.date)

  # Adjust for inflation
  adjusted_df <- df |>
    # Merge the inflation data with the original series
    dplyr::inner_join(inflation, by = date_col) |>
    # Adjust values by the inflation factor
    dplyr::mutate(adjusted = !!sym(value_col) * inflation_factor) |>
    # Drops the inflation factor column
    dplyr::select(-inflation_factor)

  vl <- "adjusted"
  names(vl) <- paste0(value_col, "_adjusted")

  adjusted_df <- dplyr::rename(adjusted_df, dplyr::any_of(vl))

  return(adjusted_df)

}

get_price_series <- function(name) {

  df <- data.frame(
    series_name = c("ipca", "igpm", "igpdi", "incc"),
    code = c(433, 189, 190, 192)
  )

  # Check if name is valid
  stopifnot(any(name %in% df[["series_name"]]))
  # Get the code number based on the name of the series
  code <- subset(df, series_name == name)[["code"]]
  # Download the data from BACEN
  series <- GetBCBData::gbcbd_get_series(
    code,
    # Since Plano Real was enacted in the end of 1994 we recommend
    # using 1995-01-01 as a starting date
    first.date = as.Date("1995-01-01")
  )
  # Return as a tibble
  return(tidyr::as_tibble(series))

}
