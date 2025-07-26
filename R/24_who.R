library(dplyr)
library(tidyr)
library(ggplot2)
library(ragg)
library(stringr)
library(patchwork)
library(ggtext)

import::from(forecast, auto.arima, forecast)
import::from(zoo, as.Date.ts, coredata)
import::from(here, here)

who <- readr::read_csv(
  here("data/day_24/WHOMortalityDatabaseRoad traffic accidents.csv"),
  skip = 6
)

who <- who %>%
  janitor::clean_names() |>
  rename(
    death_rate = death_rate_per_100_000_population,
    death_rate_adj = age_standardized_death_rate_per_100_000_standard_population,
    pct_deaths = percentage_of_cause_specific_deaths_out_of_total_deaths
  ) %>%
  mutate(
    death_rate = as.numeric(stringr::str_remove(death_rate, ","))
  )

# Forecast traffic deaths in 2022 to make a fair comparison

who_series <- who |>
  filter(
    sex == "All",
    age_group == "[All]"
  ) |>
  select(
    year,
    number,
    country_code
  )


# Helper function to convert tibble to time series
df_to_ts <- function(df) {
  y <- ts(df$number, start = min(df$year), end = max(df$year), frequency = 1)
  return(y)
}
# Helper function to convert time series to tibble
ts_to_df <- function(ts) {
  dplyr::tibble(
    date = zoo::as.Date.ts(stats::time(ts)),
    series = zoo::coredata(ts)
  )
}

# Convert tibble to time series and forecast with ARIMA
fcast_arima <- function(dat) {
  ts <- df_to_ts(dat)

  model <- forecast::auto.arima(ts)
  fcast <- forecast::forecast(model, h = 5)

  fcast_series <- ts_to_df(fcast$mean)

  original_series <- ts_to_df(ts)

  out <- dplyr::bind_rows(
    list("original" = original_series, "predicted" = fcast_series),
    .id = "type"
  )

  return(out)
}

fcast_who <- who_series |>
  group_by(country_code) |>
  nest() |>
  mutate(
    arima_fcast = purrr::map(data, fcast_arima)
  )

# The US accounts for around 18.5% off all traffic deaths
fcast_2022 <- fcast_who |>
  unnest(cols = "arima_fcast") |>
  ungroup() |>
  mutate(year = lubridate::year(date)) |>
  filter(year == 2022) |>
  mutate(share = series / sum(series) * 100) |>
  arrange(desc(share))

# Select age groups

age_groups <- c(
  "[All]",
  "[1-4]",
  "[5-9]",
  "[10-14]",
  "[15-19]",
  "[20-24]",
  "[25-29]"
)

# Select countries
sel_countries <- c("BRA", "USA", "EGY", "JPN", "GBR", "MEX")


# Filter data
subdat <- who |>
  filter(
    sex == "All",
    age_group %in% age_groups,
    country_code %in% sel_countries
  )

death_age <- subdat |>
  filter(age_group != "[All]", year >= 1980) |>
  mutate(
    age_min = if_else(
      age_group == "[0]",
      0,
      as.numeric(str_extract(age_group, "(?<=\\[)[0-9]{1,2}"))
    ),
    age_max = age_min + 4,
    age_label = str_c(age_min, " a ", age_max, " anos"),
    age_label = factor(age_label),
    age_label = forcats::fct_reorder(age_label, age_min)
  ) |>
  select(
    region_code,
    region_name,
    country_code,
    country_name,
    year,
    age_min,
    age_max,
    age_label,
    number,
    pct_deaths,
    death_rate
  )

# Get all deaths and use loess to get trend
total_trend <- subdat |>
  filter(age_group == "[All]", year >= 1970) |>
  mutate(
    country_name = if_else(country_code == "GBR", "GBR", country_name),
    country_name = if_else(country_code == "USA", "USA", country_name),
    year = as.numeric(year),
    trend_loess = loess(number ~ year, span = 0.65)$fitted,
    trend = stats::smooth.spline(year, number, spar = 1)$y,
    .by = "country_code"
  )

# Plot ------------------------------------------------------------------

## Plot elements -----------------------------------------------------------

font_text <- "Gill Sans"
offwhite <- "#fefefe"

# paletteer::paletteer_d("ggthemes::Green_Orange_Teal")

pal_colors <- c(
  "GBR" = "#191F40FF",
  "Brazil" = "#477B95FF",
  "USA" = "#5FA2A4FF",
  "Egypt" = "#8CBF9AFF",
  "Japan" = "#EF8A0CFF",
  "Mexico" = "#315B88FF"
)

df_axis <- tibble(
  x = 1968,
  y = seq(0, 50000, 10000),
  label = format(y, big.mark = ".")
)

df_axis <- df_axis %>%
  mutate(
    label = if_else(y == 50000, paste(label, "\nFatalities"), label)
  )


## Base plot ---------------------------------------------------------------

base_plot <- ggplot(total_trend, aes(year, trend_loess, color = country_name)) +
  geom_hline(yintercept = 0) +
  geom_point(
    aes(y = number, fill = country_name),
    color = "black",
    alpha = 0.25,
    shape = 21
  ) +
  geom_line(linewidth = 0.9) +
  geom_label(
    data = dplyr::filter(total_trend, year == max(year), .by = "country_name"),
    aes(label = country_name),
    family = font_text,
    size = 3.5,
    hjust = 0,
    vjust = 0,
    nudge_x = 2,
    nudge_y = c(0, 1, 0, 1, 0, 1) * 1000
  ) +
  geom_text(
    data = df_axis,
    aes(x, y, label = label),
    inherit.aes = FALSE,
    hjust = 1,
    family = font_text,
    size = 4,
    nudge_y = c(1000, 1000, 1000, 1000, 1000, 0)
  ) +
  scale_x_continuous(
    breaks = seq(1970, 2020, 10),
    limits = c(1964, 2038)
  ) +
  scale_y_continuous(
    breaks = seq(0, 50000, 10000),
    labels = scales::label_number(big.mark = "."),
    expand = expansion(mult = c(0, 0.05))
  ) +
  scale_color_manual(values = pal_colors) +
  scale_fill_manual(values = pal_colors) +
  guides(color = "none", fill = "none")

## Plot annotations -------------------------------------------------------

# Further improved - more concise and clearer
l1 <- "<span style='font-family:Lato'><span style='color:#EF8A0CFF'><b>Japan</b></span> and <span style='color:#191F40FF'><b>Britain</b></span><br>cut traffic deaths<br>80% since 1970</span>"
l2 <- "<span style='font-family:Lato'>Despite a recent downward trend,<br><span style='color:#477B95FF'><b>Brazil</b></span> saw traffic deaths<br>rise 80% in the past 40 years.</span>"
l3 <- "<span style='font-family:Lato'><span style='color:#5FA2A4FF'><b>USA</b></span> leads globally<br>with 46k deaths (2022)<br>— 18.5% of world total*</span>"


annotated_plot <- base_plot +
  geom_richtext(
    data = tibble(x = 2028.5, y = 7500, label = l1),
    aes(x, y, label = label),
    family = font_text,
    size = 3,
    inherit.aes = FALSE,
    hjust = 0
  ) +
  geom_richtext(
    data = tibble(x = 2015, y = 25000, label = l2),
    aes(x, y, label = label),
    family = font_text,
    size = 3,
    inherit.aes = FALSE,
    hjust = 0
  ) +
  geom_richtext(
    data = tibble(x = 2011, y = 52000, label = l3),
    aes(x, y, label = label),
    family = font_text,
    size = 3,
    inherit.aes = FALSE,
    hjust = 0
  )

final_plot <- annotated_plot &
  plot_annotation(
    title = "Diverging Roads: the global road safety crisis",
    subtitle = "While Japan and Britain cut deaths by 80%, other major countries struggle to keep the same pace.",
    caption = "Source: WHO Mortality Database\n(*)Estimated global death share."
  ) &
  theme_minimal(base_family = font_text) +
    theme(
      panel.background = element_rect(fill = offwhite, color = offwhite),
      plot.background = element_rect(fill = offwhite, color = offwhite),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(size = 10),
      axis.text.y = element_blank(),
      axis.title = element_blank(),
      plot.caption = element_text(size = 8, color = "gray40", hjust = 0),
      plot.title = element_text(size = 18),
      plot.subtitle = element_text(size = 10, color = "gray20"),
      plot.margin = margin(5, 10, 5, 10)
    )

ggsave(here("plots/24_who.png"), final_plot, width = 8, height = 5.5)
