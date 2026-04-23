library(microbenchmark)
library(priceR)
library(dplyr)

# Copied from fast_inflate
fast_inflate_original <- function(price, from, to) {

  make_multiplier <- function(from_input, to_input) {

    inflation_dataframe %>%
      filter(date > from_input & date <= to_input | date < from_input & date >= to_input ) %>%
      .$value %>% {. / 100} %>% {. + 1} %>% { ifelse(from_input < to_input, prod(.), { 1 / prod(.) }) }
  }


  multipliers <- mapply(make_multiplier, from_input = from, to_input = to)

  real_price <- price * multipliers

  real_price
}

# Using base R subselection ([, $)
fast_inflate_base <- function(price, from, to) {

  make_multiplier <- function(from_input, to_input) {

    if (from_input < to_input) {
      inflation_values <- inflation_dataframe$value[inflation_dataframe$date > from_input & inflation_dataframe$date <= to_input]
      return(prod(1 + inflation_values/100))
    } else {
      inflation_values <- inflation_dataframe$value[inflation_dataframe$date >= to_input & inflation_dataframe$date < from_input]
      return(1/prod(1 + inflation_values/100))
    }
  }

  multipliers <- mapply(make_multiplier, from_input = from, to_input = to)
  real_price <- price * multipliers
  return(real_price)

}
adjust_for_inflation()
# Using base R subselection ([, $) and pre-calculating all unique multipliers
fast_inflate_dplyr <- function(price, from, to) {

  make_multiplier <- function(from_input, to_input) {

    if (from_input < to_input) {
      inflation_values <- inflation_dataframe$value[inflation_dataframe$date > from_input & inflation_dataframe$date <= to_input]
      return(prod(1 + inflation_values/100))
    } else {
      inflation_values <- inflation_dataframe$value[inflation_dataframe$date >= to_input & inflation_dataframe$date < from_input]
      return(1/prod(1 + inflation_values/100))
    }
  }

  if (length(price) <= 1 || length(unique(paste(from, to, sep = "_"))) == length(from)) {
    # Use original solution
    multipliers <- mapply(make_multiplier, from_input = from, to_input = to)
    real_price <- price * multipliers
    return(real_price)
  }

  # Create data frame of all inputs
  pairs <- tibble(
    row_id = 1:length(from),
    from = from,
    to = to,
    price = price
  )

  # Extract unique date pairs
  unique_pairs <- dplyr::distinct(pairs[, c("from", "to")])

  # Calculate multipliers for each unique pair
  unique_pairs <- unique_pairs %>%
    dplyr::rowwise() %>%
    dplyr::mutate(multiplier = make_multiplier(from, to)) %>%
    dplyr::ungroup()

  # Create lookup table for multipliers
  lookup <- setNames(
    unique_pairs$multiplier,
    paste(unique_pairs$from, unique_pairs$to, sep = "_")
  )

  # Apply multipliers using lookup
  keys <- paste(from, to, sep = "_")
  real_price <- price * lookup[keys]

  # pure dplyr approach
  real_price <- pairs %>%
    dplyr::left_join(unique_pairs, by = c("from", "to")) %>%
    dplyr::arrange(row_id) %>%
    dplyr::mutate(adjusted_price = price * multiplier) %>%
    dplyr::pull(adjusted_price)

  return(unname(real_price))

}

# Create fake data using the same parameters above

set.seed(123)
number_of_rows <- 10000
country <- "US"

nominal_prices <- rnorm(number_of_rows, mean=10, sd=3)
years <- round(rnorm(number_of_rows, mean=2006, sd=5))
df <- data.frame(years, nominal_prices)

inflation_dataframe <- retrieve_inflation_data(country)

# Run the benchmark
results <- microbenchmark(
  original = fast_inflate_original(df$nominal_prices, df$years, 2008),
  optim_base = fast_inflate_base(df$nominal_prices, df$years, 2008),
  optim_dplyr = fast_inflate_dplyr(df$nominal_prices, df$years, 2008),
  times = 20
)

# Print the benchmark results
print(results)

# Create a plot of the results
ggplot2::autoplot(results)

# Check if results match
benchmark <- fast_inflate_original(df$nominal_prices, df$years, 2008)
optimized <- fast_inflate_dplyr(df$nominal_prices, df$years, 2008)
all.equal(benchmark, optimized)


# "normal" data

data("USMacroG", package = "AER")

dat <- data.frame(
  date = zoo::as.Date.ts(time(USMacroG)),
  zoo::coredata(USMacroG[, c(1:3)])
)

names(dat)[2:4] <- c("gdp", "consumption", "invest")

results <- microbenchmark(
  original = adjust_for_inflation(dat$gdp, from_date = dat$date, country = country, to_date = 2008),
  optim_dplyr = adjust_for_inflation_modified(dat$gdp, from_date = dat$date, country = country, to_date = 2008),
  times = 5
)

