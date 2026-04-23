library(tidyverse)
library(GetBCBData)
library(realestatebr)
library(RcppRoll)
library(patchwork)
library(trendseries)
library(ragg)
library(here)
source(here::here("R/adjust_inflation.R"))
# source(here::here("R/functions/series_trend.R"))

pretty_number <- function(x, digits = 1, percent = FALSE) {
  x <- round(x, digits = digits)
  x <- format(x, big.mark = ".", decimal.mark = ",")
  if (isTRUE(percent)) {
    x <- paste0(x, "%")
  }
  return(x)
}

str_simplify <- function(x) {

  y <- stringi::stri_trans_general(x, id = "latin-ascii")
  y <- stringr::str_replace_all(y, " ", "_")
  y <- stringr::str_to_lower(y)
  return(y)

}

codes <- c(
  "fimob_pf_total" = 20704,
  "taxa_fimob_pf_mercado" = 20772,
  "taxa_fimob_pf_regulado" = 20773,
  "taxa_fimob_pf_total" = 20774,
  "selic" = 432
)

bcb_series <- gbcbd_get_series(codes)

bcb_series <- as_tibble(bcb_series)

bcb_series <- rename(bcb_series, date = ref.date)

taxa <- bcb_series |>
  filter(str_detect(series.name, "^taxa")) |>
  mutate(
    trend_ma = RcppRoll::roll_meanr(value, n = 9),
    .by = "series.name"
  )

fimob <- bcb_series |>
  filter(series.name == "fimob_pf_total") |>
  select(date, series.name, value) |>
  adjust_inflation(base_year = 2019) |>
  add_trend(value_col = "value_adjusted", trend = "stl", window_stl = 39) |>
  select(date, series.name, value_adjusted, trend_stl)

taxa_recent <- filter(taxa, date >= as.Date("2016-01-01"))
fimob_recent <- filter(fimob, date >= as.Date("2016-01-01"))
taxa_selic <- filter(bcb_series, series.name == "selic", date >= as.Date("2016-01-01"), date <= as.Date("2025-02-01"))

hrbrthemes::ipsum_pal()

cores <- c("#01665e", "gray20", "#4575b4")
offwhite <- "#ffffff"

p <- ggplot() +
  geom_step(
    data = taxa_selic,
    aes(x = date, y = value, color = series.name),
    linewidth = 1) +
  # Interest rate - muted raw series
  geom_line(
    data = filter(taxa_recent, series.name == "taxa_fimob_pf_total"),
    aes(x = date, y = value, color = series.name),
    color = cores[3],
    alpha = 0.4) +
  # Interest rate - muted raw points
  geom_point(
    data = filter(taxa_recent, series.name == "taxa_fimob_pf_total"),
    aes(x = date, y = value, color = series.name),
    color = cores[3],
    shape = 21,
    alpha = 0.4) +
  # Interest rate MA trend
  geom_line(
    data = filter(taxa_recent, series.name == "taxa_fimob_pf_total"),
    aes(x = date, y = trend_ma, color = series.name),
    linewidth = 1) +
  # Housing credit - muted raw series
  geom_line(
    data = fimob_recent,
    aes(x = date, y = value_adjusted / 10^3),
    color = cores[1],
    alpha = 0.4) +
  # Housing credit - muted raw points
  geom_point(
    data = fimob_recent,
    aes(x = date, y = value_adjusted / 10^3),
    color = cores[1],
    shape = 21,
    alpha = 0.4) +
  # Housing credit - smooth STL trend
  geom_line(
    data = fimob_recent,
    aes(x = date, y = trend_stl / 10^3, color = series.name),
    linewidth = 1) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(
    name = "(% a.a.)",
    breaks = seq(0, 20, 2.5),
    sec.axis = dup_axis(name = "Bi. R$ (2019 constant)")) +
  scale_color_manual(
    name = "",
    values = cores[c(1, 2, 3)],
    labels = c("Real Estate Credit (rhs)", "SELIC rate (lhs)",
               "Avg. interest rate, housing (lhs)")) +
  labs(
    title = "The ups and downs of real estate credit in Brazil",
    subtitle = "Total value of new monthly credit operations (real estate) for households in constant reais.\nPoints indicate raw values and lines are smooth trends.",
    x = NULL,
    caption = "Source: Brazilian Central Bank.") +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    panel.background = element_rect(fill = offwhite, color = offwhite),
    plot.background = element_rect(fill = offwhite, color = offwhite),
    legend.position = "top",
    legend.text = element_text(size = 12),
    plot.title = element_text(size = 16),
    panel.grid.minor = element_blank()
  )

ggsave(here("plots/16_negative.png"), p, width = 8, height = 6.2)

bcb_series |>
  filter(series.name %in% c("fimob_pf_total", "taxa_fimob_pf_total"))
