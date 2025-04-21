library(dplyr)
library(ggplot2)
library(ragg)
library(patchwork)
import::from(tidyquant, tq_get)

indexes <- tq_get(c("^GSPC", "^IXIC", "^DJI"))

spfull <- filter(indexes, symbol == "^GSPC")

sp500 <- filter(indexes, symbol == "^GSPC", date >= as.Date("2021-01-01"))

xsp500 <- ts(sp500$close, frequency = 252)
ysp500 <- ts(sp500$close, frequency = 7)

arma <- arima(xsp500, order = c(0, 2, 2))

arma_trend <- Arima(ysp500, order = c(1, 0, 1))
arma_trend <- fitted(arma_trend)

sp500 <- sp500 |>
  mutate(
    dema = TTR::DEMA(close, 21),
    ema = TTR::EMA(close, 21),
    ma5 = TTR::SMA(close, n = 5),
    ma21 = TTR::SMA(close, n = 21),
    alma = TTR::ALMA(close)[, 1],
    tukey = runmed(close, k = 13)
  )

stl_sp500 <- stl(ysp500, s.window = 31)
mstl_sp500 <- mstl(ysp500)
cubic_spline <- smooth.spline(time(ysp500), ysp500)
# super_smooth <- supsmu(time(xsp500), xsp500, span = 0.3, bass = 8)

sp500$stl <- as.numeric(as_tibble(stl_sp500$time.series)$trend)
sp500$mstl <- as.numeric(as_tibble(mstl_sp500)$Trend)
sp500$arma <- as.numeric(arma_trend)
sp500$spline <- as.numeric(cubic_spline$y)
sp500$kalman <- as.numeric(smooth_kalman[, "slope"])
# sp500$friedman <- as.numeric(super_smooth$y)

dat <- filter(sp500, date >= as.Date("2024-10-01"))

dat <- dat |>
  mutate(
    tt = row_number(),
    tt2 = ifelse(date > as.Date("2025-02-19"), tt - 96, 0),
    friedman = supsmu(tt, close, span = 0.3, bass = 9)$y
  )

model_lm <- lm(close ~ tt + tt2, data = dat)
dat$pwlinear <- as.numeric(fitted(model_lm))

# Plot elements -----------------------------------------------------------

cores <- c("#1d3557", "#457b9d")
# cores <- RColorBrewer::brewer.pal(9, "PuBu")[c(8, 6)]
offwhite <- "#fefefe"

ds <- dat |>
  mutate(d = lubridate::day(date), m = lubridate::month(date)) |>
  group_by(m) |>
  summarise(date_start = min(date))

inds <- dat |>
  select(date, tt) |>
  inner_join(ds, by = c("date" = "date_start")) |>
  pull(tt)

dlabels <- c("24-Oct", "24-Nov", "24-Dec", "25-Jan", "25-Feb", "25-Mar", "25-Abr")

theme_plot <- theme_minimal(base_family = "Futura") +
  theme(
    panel.background = element_rect(fill = offwhite, color = offwhite),
    plot.background = element_rect(fill = offwhite, color = offwhite),
    panel.grid.minor = element_blank(),
    plot.title = element_text(hjust = 0.5)
  )

plot_series <- function(series, title = "") {

  ggplot(dat, aes(date)) +
    geom_point(
      aes(y = close),
      alpha = 0.4,
      size = 0.75,
      color = cores[1]
    ) +
    geom_line(
      aes(y = {{series}}),
      linewidth = 1,
      color = cores[1]
    ) +
    scale_x_date(date_labels = "%y-%b", date_breaks = "1 month") +
    scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
    labs(x = NULL, y = NULL, title = title) +
    theme_plot

}

plot_series_smooth <- function(method, title = "", k = NULL) {

  base_plot <- ggplot(dat, aes(date)) +
    geom_point(
      aes(y = close),
      alpha = 0.4,
      size = 0.75,
      color = cores[1]
    ) +
    scale_x_date(date_labels = "%y-%b", date_breaks = "1 month") +
    scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
    labs(x = NULL, y = NULL, title = title) +
    theme_plot

  if (method == "loess" | method == "gam") {

    p <- base_plot +
      geom_smooth(
        aes(y = close),
        method = "loess",
        linewidth = 1,
        color = cores[1],
        se = FALSE
      )

  }

  if (method == "poly" && k > 0) {

    p <- base_plot +
      geom_smooth(
        aes(y = close),
        method = "lm",
        formula = y ~ poly(x, k),
        linewidth = 1,
        color = cores[1],
        se = FALSE
      )

  }

  return(p)

}

plot_series_continuous <- function(series, title = "") {

  ggplot(dat, aes(tt)) +
    geom_point(
      aes(y = close),
      alpha = 0.4,
      size = 0.75,
      color = cores[1]
    ) +
    geom_line(
      aes(y = {{series}}),
      linewidth = 1,
      color = cores[1]
    ) +
    scale_x_continuous(breaks = inds, labels = dlabels) +
    scale_y_continuous(labels = scales::label_number(big.mark = ".")) +
    labs(x = NULL, y = NULL, title = title) +
    theme_plot

}

format_plot <- function(plot, pos) {

  if (pos == "middle") {
    p <- plot +
      theme(
        axis.text = element_blank()
      )
  }

  if (pos == "bottom") {
    p <- plot +
      theme(
        axis.text.y = element_blank()
      )
  }

  if (pos == "left") {
    p <- plot +
      theme(
        axis.text.x = element_blank()
      )
  }

  if (pos == "none") {
    return(plot)
  }

  return(p)

}

p11 <- plot_series_smooth("poly", k = 1, title = "Linear trend")
p12 <- plot_series_smooth("poly", k = 2, title = "Quadatric trend")
p13 <- plot_series_smooth("poly", k = 3, title = "Cubic trend")
p14 <- plot_series_smooth("poly", k = 27, title = "Overfitting polynomial")
p21 <- plot_series_continuous(pwlinear, "Piecewise Linear")
p22 <- plot_series(ma5, "MA-5")
p23 <- plot_series(ma21, "MA-21")
p24 <- plot_series(spline, "Cubic Spline")
p31 <- plot_series_smooth("loess", title = "LOESS")
p32 <- plot_series(stl, "STL")
p33 <- plot_series(ema, "EWMA")
p34 <- plot_series(dema, "DEWMA")
p41 <- plot_series_continuous(friedman, title = "Super Smooth")
p42 <- plot_series_continuous(tukey, title = "Running Median")
p43 <- plot_series(alma, title = "ALMA")
p44 <- plot_series(arma, title = "ARMA(1,1)")


p1 <- format_plot(p11, "left") | format_plot(p12, "middle") | format_plot(p13, "middle") | format_plot(p14, "middle")
p2 <- format_plot(p21, "left") | format_plot(p22, "middle") | format_plot(p23, "middle") | format_plot(p24, "middle")
p3 <- format_plot(p31, "left") | format_plot(p32, "middle") | format_plot(p33, "middle") | format_plot(p34, "middle")
p4 <- format_plot(p41, "none") | format_plot(p42, "bottom") | format_plot(p43, "bottom") | format_plot(p44, "bottom")

panel <- p1 / p2 / p3 / p4

panel <- panel & plot_annotation(
  title = "Smoothing soothes your S&P sorrows",
  subtitle = "Closing price of the S&P500 Index since Oct-24, presented with different smoothers.\nDisclaimer: this is a fun visualzation, not all of these smoothers make sense.",
  caption = "Source: Yahoo Finance.",
  theme = theme(
    panel.background = element_rect(fill = offwhite, color = offwhite),
    plot.background = element_rect(fill = offwhite, color = offwhite),
    plot.title = element_text(family = "Futura", size = 32),
    plot.subtitle = element_text(family = "Futura", color = "gray30"),
    plot.caption = element_text(family = "Futura", color = "gray30")
  )
)

ggsave(here::here("plots/19_smoothing_2.png"), panel, width = 14, height = 9.5)
