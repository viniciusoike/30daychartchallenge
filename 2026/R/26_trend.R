# Day 26 - Trend (Uncertainties): São Paulo Metro ridership forecast ------

library(ggplot2)
library(dplyr)
library(patchwork)
library(forecast)
library(trendseries)
library(metrosp)
import::from(scales, label_number)

options(scales.big.mark = ".")
options(scales.decimal.mark = ",")

metro_cols <- metro_colors
metro_cols[1] <- "#1E3A5F"
metro_cols[3] <- "#C53030"
metro_cols[4] <- "#D69E2E"

## Data --------------------------------------------------------------------

data(passengers_entrance)
data(metro_colors)

sel_lines <- c(1, 2, 3, 4, 5)

series <- passengers_entrance |>
  filter(
    metric_abb == "total",
    line_number %in% sel_lines,
    date >= as.Date("2019-01-01"),
    date <= as.Date("2024-12-01")
  ) |>
  select(date, line_number, line_name, value) |>
  arrange(line_number, date)

## Forecast ----------------------------------------------------------------

dat <- passengers_entrance |>
  filter(
    line_number == 1,
    metric_abb == "total",
    date >= as.Date("2019-01-01"),
    date <= as.Date("2024-12-01")
  )

forecast_line <- function(df, h = 12) {
  ts_line <- ts(log(df$value), start = c(2019, 1), frequency = 12)

  sub_line <- window(ts_line, start = c(2022, 1))
  m <- Arima(sub_line, order = c(0, 1, 1), seasonal = c(0, 1, 1))
  fc <- forecast(m, h = h, level = c(80, 95))

  future_dates <- seq(max(df$date) + 31, by = "month", length.out = h)
  future_dates <- as.Date(format(future_dates, "%Y-%m-01"))

  tibble(
    date = future_dates,
    mean = as.numeric(fc$mean),
    lo80 = fc$lower[, 1],
    hi80 = fc$upper[, 1],
    lo95 = fc$lower[, 2],
    hi95 = fc$upper[, 2]
  )
}

fcasts <- series |>
  group_by(line_number, line_name) |>
  group_modify(~ forecast_line(.x)) |>
  ungroup() |>
  mutate(across(where(is.numeric), exp))

full_series <- bind_rows(series, fcasts)

full_series <- full_series |>
  augment_trends(
    value_col = "value",
    group_cols = "line_name",
    methods = "stl"
  ) |>
  mutate(
    is_fcast = factor(if_else(date >= as.Date("2025-01-01"), 1L, 0L)),
    value = if_else(is_fcast == 1L, mean, value),
    line_name = factor(
      line_name,
      levels = c("Blue", "Green", "Red", "Yellow", "Lilac"),
      labels = c("1-Blue", "2-Green", "3-Red", "4-Yellow", "5-Lilac")
    )
  ) |>
  arrange(line_number, date)

## Theme -------------------------------------------------------------------

offwhite <- "#f8fbf8"
font_text <- "Roboto Slab"

theme_plot <- theme_minimal(base_family = font_text) +
  theme_sub_plot(
    title = element_text(size = 16, family = "Georgia"),
    subtitle = element_text(size = 10, color = "gray40"),
    caption = element_text(size = 8, color = "gray60"),
    margin = margin(15, 10, 10, 10),
    background = element_rect(fill = offwhite, color = offwhite)
  ) +
  theme_sub_panel(
    grid.minor = element_blank(),
    grid.major.x = element_blank(),
    background = element_rect(fill = offwhite, color = offwhite)
  ) +
  theme_sub_axis_x(
    ticks = element_line(color = "gray20"),
    line = element_line(color = "gray20", linewidth = 0.5),
    text = element_text(color = "gray20")
  )

## Plot --------------------------------------------------------------------

x <- seq(0, 1, length.out = 2 * 12)[-1]
smooth <- cbind(x, scales::rescale(1 / (1 + exp(-(x * 10 - 5)))))

trend_plot <- ggplot(full_series, aes(color = line_name, fill = line_name)) +
  geom_ribbon(aes(date, ymin = lo95, ymax = hi95), lwd = 0.15, alpha = 0.15) +
  geom_ribbon(aes(date, ymin = lo80, ymax = hi80), lwd = 0.1, alpha = 0.3) +
  geom_point(aes(date, value), size = 0.5, alpha = 0.5) +
  geom_line(aes(date, trend_stl), linewidth = 0.7) +
  stat_connect(
    data = subset(full_series, is_fcast == 1),
    aes(date, value),
    linewidth = 0.5,
    alpha = 0.7,
    connection = smooth
  ) +
  facet_wrap(vars(line_name), scales = "free_y") +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(
    labels = label_number(scale = 1e-6),
    expand = expansion(mult = c(0, 0.1)),
    limits = c(0, NA)
  ) +
  scale_color_manual(values = unname(metro_cols)) +
  scale_fill_manual(values = unname(metro_cols)) +
  labs(
    title = "How many will ride the Metro tomorrow?",
    subtitle = stringr::str_wrap(
      "Monthly passenger entrances on São Paulo's Metro lines (millions). Solid lines are desseasoned trends from observed data; shaded fans show forecasts with 80% and 95% prediction intervals for 2025-2026.",
      101
    ),
    caption = "Source: Metrô/ViaQuatro/ViaMobilidade via {metrosp} • @viniciusoike",
    x = NULL,
    y = NULL
  ) +
  theme_plot +
  theme_sub_strip(
    text = element_text(size = 10, family = "Roboto Slab", color = "#ffffff"),
    background = element_rect(fill = "#0D1B2A", color = "#ffffff")
  ) +
  theme(legend.position = "none")

ggsave(
  here::here("2026", "plots", "26_trend.png"),
  trend_plot,
  width = 8,
  height = 5,
  dpi = 400
)
