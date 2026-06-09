# Prompt: Uncertainties
# Space -- forecasting the sunspot cycle with a feed-forward neural network.
# A neural net (NNAR, via forecast::nnetar) trained on 275 years of monthly
# sunspot counts projects Solar Cycle 25's decline and the rise of Cycle 26,
# with simulated 80% / 95% prediction intervals fanning out into the future.

# Setup -----------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ggtext)
library(showtext)
library(forecast)
import::from(lubridate, make_date)

# Add n calendar months to a Date (avoids extra lubridate exports).
add_months <- function(d, n) seq(d, by = "month", length.out = n + 1)[n + 1]

font_add_google("Roboto Slab", "title_font")
font_add_google("Lato", "body_font")
showtext_auto()
showtext_opts(dpi = 300)

# Data ------------------------------------------------------------------------
# Monthly mean total sunspot number, WDC-SILSO (Royal Observatory of Belgium).
# Cache locally under the gitignored 2026/data/ tree; re-download if absent.

url <- "https://www.sidc.be/SILSO/DATA/SN_m_tot_V2.0.csv"
data_dir <- here::here("2026/data/space")
csv_path <- file.path(data_dir, "SN_m_tot_V2.0.csv")

if (!file.exists(csv_path)) {
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  download.file(url, csv_path, quiet = TRUE)
}

dat <- read.delim(csv_path, sep = ";", dec = ".", header = FALSE)
names(dat) <- c(
  "year",
  "month",
  "dec_date",
  "sunspot",
  "sunspot_sd",
  "ind1",
  "ind2"
)

dat <- dat |>
  as_tibble() |>
  mutate(
    date = make_date(year, month, 1),
    # Centred 13-month smoothing -- the SIDC-standard smoothed sunspot number.
    trend = as.numeric(stats::filter(sunspot, rep(1 / 13, 13), sides = 2))
  )

# Model -----------------------------------------------------------------------
# NNAR: a single-hidden-layer feed-forward neural net (nnet) on lagged values.
# frequency = 12 lets it learn the ~11-year cycle and oscillate forward;
# PI = TRUE simulates prediction-interval paths -> the uncertainty fan.
#
# lambda = 0.5 fits on a sqrt (Box-Cox) scale, then back-transforms. This keeps
# the forecast and its intervals non-negative *by construction* (log is out --
# the series has 67 months at exactly 0), and lets the fan widen with the level
# (narrow at solar minima, wide at peaks) instead of being symmetric.

set.seed(42)
ts_y <- ts(dat$sunspot, frequency = 12, start = c(dat$year[1], dat$month[1]))
fit <- nnetar(ts_y, lambda = 0.5)
fc <- forecast(fit, h = 180, PI = TRUE, npaths = 500)

last_date <- max(dat$date)
future_dates <- seq(last_date, by = "month", length.out = 181)[-1]

# The sqrt transform already bounds the forecast at ~0; pmax() only mops up
# tiny back-transform rounding so nothing dips below zero.
fc_df <- tibble(
  date = future_dates,
  mean = pmax(as.numeric(fc$mean), 0),
  lo80 = pmax(fc$lower[, 1], 0),
  hi80 = fc$upper[, 1],
  lo95 = pmax(fc$lower[, 2], 0),
  hi95 = fc$upper[, 2]
)

# Window the plotted history (model still trains on the full series).
hist_df <- filter(dat, date >= as.Date("1900-01-01"))

# Plot ------------------------------------------------------------------------
# One builder, two palettes: warm off-white (house style) and dark "space".

x_breaks <- seq(as.Date("1900-01-01"), as.Date("2040-01-01"), by = "20 years")

make_plot <- function(pal) {
  note <- tibble(
    x = as.Date("1912-01-01"),
    y = 300,
    label = glue::glue(
      "<b style='color:{pal$fc}'>80% / 95%</b> prediction intervals widen with the horizon"
    )
  )
  fc_lab <- tibble(
    x = max(fc_df$date),
    y = 298,
    label = glue::glue(
      "<b style='color:{pal$fc}'>Neural-network<br>forecast</b><br>NNAR(35,1,18)"
    )
  )

  ggplot() +
    geom_ribbon(
      data = fc_df,
      aes(date, ymin = lo95, ymax = hi95),
      fill = pal$fc,
      alpha = 0.16
    ) +
    geom_ribbon(
      data = fc_df,
      aes(date, ymin = lo80, ymax = hi80),
      fill = pal$fc,
      alpha = 0.30
    ) +
    geom_line(
      data = hist_df,
      aes(date, sunspot),
      color = pal$hist,
      alpha = 0.45,
      linewidth = 0.25
    ) +
    geom_line(
      data = hist_df,
      aes(date, trend),
      color = pal$trend,
      linewidth = 0.7
    ) +
    geom_line(
      data = fc_df,
      aes(date, mean),
      color = pal$fc,
      linewidth = 0.8
    ) +
    geom_vline(
      xintercept = last_date,
      linetype = 2,
      linewidth = 0.3,
      color = pal$ink_soft
    ) +
    geom_hline(yintercept = 0, linewidth = 0.5, color = pal$ink_soft) +
    # geom_richtext(
    #   data = note, aes(x, y, label = label),
    #   family = "body_font", size = 2.7, hjust = 0,
    #   color = pal$ink, fill = NA, label.color = NA
    # ) +
    # geom_richtext(
    #   data = fc_lab, aes(x, y, label = label),
    #   family = "body_font", size = 2.7, hjust = 1, vjust = 1, lineheight = 1.1,
    #   color = pal$ink, fill = NA, label.color = NA
    # ) +
    scale_x_date(
      breaks = x_breaks,
      date_labels = "%Y",
      expand = expansion(mult = c(0.01, 0.04))
    ) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    labs(
      title = "The Sun's uncertain future",
      subtitle = glue::glue(
        "A neural network trained on 275 years of monthly sunspot counts projects ",
        "Solar Cycle 25's decline and the rise of Cycle 26. The further it looks, ",
        "the less it knows — the <b style='color:{pal$fc}'>shaded bands</b> ",
        "spread accordingly."
      ),
      x = NULL,
      y = "Monthly mean sunspot number",
      caption = paste0(
        "Source: WDC-SILSO, Royal Observatory of Belgium — monthly sunspot ",
        "number. Forecast: feed-forward neural net (forecast::nnetar) with ",
        "80% / 95% prediction intervals. • @viniciusoike"
      )
    ) +
    theme_minimal(base_family = "body_font") +
    theme(
      plot.margin = margin(15, 12, 8, 12),
      plot.background = element_rect(fill = pal$bg, color = NA),
      panel.background = element_rect(fill = pal$bg, color = NA),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = pal$grid, linewidth = 0.3),
      panel.grid.major.x = element_blank(),
      plot.title = element_text(
        family = "title_font",
        face = "bold",
        size = 21,
        color = pal$ink
      ),
      plot.subtitle = element_textbox_simple(
        family = "body_font",
        size = 9.5,
        color = pal$ink_soft,
        lineheight = 1.25,
        margin = margin(t = 4, b = 14)
      ),
      plot.caption = element_text(size = 6.5, color = pal$ink_soft, hjust = 0),
      axis.title.y = element_text(size = 9, color = pal$ink_soft),
      axis.text = element_text(size = 9, color = pal$ink_soft),
      axis.ticks.x = element_line(color = pal$ink_soft),
      axis.ticks.length = unit(4, "pt")
    )
}

# Palettes --------------------------------------------------------------------

pal_light <- list(
  bg = "#f8f7f2",
  hist = "#9b2226",
  trend = "#9b2226",
  fc = "#E76F00",
  ink = "#1a1a1a",
  ink_soft = "#5c5c5c",
  grid = "#dedad5"
)

pal_dark <- list(
  bg = "#0b1026",
  hist = "#ffb703",
  trend = "#ffd166",
  fc = "#ff7b00",
  ink = "#ececec",
  ink_soft = "#9aa4b2",
  grid = "#1d2747"
)

# Save ------------------------------------------------------------------------

ggsave(
  here::here("2026/plots/25_space.png"),
  plot = make_plot(pal_light),
  width = 9,
  height = 5,
  dpi = 300
)

ggsave(
  here::here("2026/plots/25_space_dark.png"),
  plot = make_plot(pal_dark),
  width = 9,
  height = 5,
  dpi = 300
)
