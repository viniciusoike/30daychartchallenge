library(dplyr)
library(ggplot2)
library(ragg)
library(ggtext)
library(scales)
library(stringr)
library(tsibble)
library(fable)
library(fabletools)
library(feasts)
library(gganimate)

import::from(here, here)
import::from(rbcb, get_series)

# Data --------------------------------------------------------------------

# Brazilian monthly vehicle production (units), BCB SGS series 1373 (Anfavea).
# Strongly seasonal, with a near-total collapse in April 2020 (COVID lockdown:
# ~1,847 units vs. ~250k normal). Cache the raw pull alongside the other
# (gitignored) inputs so we only hit the BCB API once.
data_dir <- here("2026", "data", "anfavea")
if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
cache <- file.path(data_dir, "veh_production.rds")

if (!file.exists(cache)) {
  prod <- get_series(1373, start_date = "2012-01-01", as = "tibble")
  names(prod) <- c("date", "value")
  saveRDS(prod, cache)
}
prod <- readRDS(cache)

ts <- prod |>
  mutate(month = yearmonth(date)) |>
  as_tsibble(index = month)

# Fit competing models + forecast -----------------------------------------

origin <- yearmonth("2019 Dec")   # forecast origin: last month before COVID
h <- 24                           # forecast Jan 2020 -> Dec 2021

train <- ts |> filter(month <= origin)

# Five deliberately different models, fit in one pipeline. They agree on a
# "normal" 2020 and none can anticipate the lockdown.
fit <- train |>
  model(
    `Seasonal naive` = SNAIVE(value),
    ETS = ETS(value),
    ARIMA = ARIMA(value),
    `Linear (trend + season)` = TSLM(value ~ trend() + season())
  )

model_levels <- c("Seasonal naive", "ETS", "ARIMA", "Linear (trend + season)")

fc <- fit |> forecast(h = h)

# Tidy forecast: point forecast + 80/95% intervals per model.
fc_df <- fc |>
  mutate(
    lo95 = hilo(value, 95)$lower,
    hi95 = hilo(value, 95)$upper,
    lo80 = hilo(value, 80)$lower,
    hi80 = hilo(value, 80)$upper
  ) |>
  as_tibble() |>
  transmute(
    model = factor(.model, levels = model_levels),
    date = as.Date(month),
    mean = .mean,
    lo95, hi95, lo80, hi80
  )

# Recent history (drawn in before the fans bloom) and the actual outcome that
# craters through every interval.
hist_df <- prod |> filter(date >= as.Date("2016-01-01"), date <= as.Date("2019-12-01"))
actual_df <- prod |>
  filter(date >= as.Date("2019-12-01"), date <= as.Date("2021-12-01"))

origin_date <- as.Date("2019-12-01")
covid_df <- prod |> filter(date == as.Date("2020-04-01"))

# Plot --------------------------------------------------------------------

offwhite <- "#fefefe"
col_actual <- "#1A1A1A"

model_pal <- c(
  "Seasonal naive" = "#C2410C",
  "ETS" = "#2C7A7B",
  "ARIMA" = "#1D4ED8",
  "Linear (trend + season)" = "#9D174D"
)

font_text <- "Roboto Slab"

theme_plot <- theme_minimal(base_family = font_text) +
  theme_sub_plot(
    title = element_text(size = 16, family = "Georgia"),
    subtitle = element_textbox_simple(size = 10, color = "gray40", margin = margin(b = 12)),
    caption = element_text(size = 8, color = "gray60"),
    margin = margin(15, 14, 10, 10),
    background = element_rect(fill = offwhite, color = offwhite)
  ) +
  theme_sub_panel(
    grid.minor = element_blank(),
    grid.major.x = element_blank(),
    background = element_rect(fill = offwhite, color = offwhite)
  ) +
  theme_sub_legend(
    position = "bottom",
    title = element_blank()
  )

p <- ggplot(mapping = aes(x = date)) +
  # Forecast origin marker
  geom_vline(xintercept = origin_date, linetype = "dashed", color = "gray70", linewidth = 0.4) +
  # Per-model 95% uncertainty fans
  geom_ribbon(
    data = fc_df,
    aes(ymin = lo95, ymax = hi95, fill = model, group = model),
    alpha = 0.12
  ) +
  # Recent history
  geom_line(data = hist_df, aes(y = value), color = "gray45", linewidth = 0.7) +
  # Per-model point forecasts
  geom_line(
    data = fc_df,
    aes(y = mean, color = model, group = model),
    linewidth = 0.8
  ) +
  # The actual outcome that craters through every fan
  geom_line(data = actual_df, aes(y = value), color = col_actual, linewidth = 1.1) +
  geom_point(
    data = covid_df, aes(y = value),
    color = col_actual, fill = "#D7282F", shape = 21, size = 3, stroke = 0.8
  ) +
  annotate(
    "richtext", x = as.Date("2020-04-01"), y = 38000, vjust = 1, hjust = 0.1,
    label = "<b>COVID lockdown</b><br>Apr 2020: 1,847 units",
    family = font_text, size = 2.9, color = col_actual, fill = NA, label.color = NA
  ) +
  annotate(
    "text", x = origin_date, y = 360000, hjust = 1.03, vjust = 1,
    label = "Forecast origin\nDec 2019",
    family = font_text, size = 2.7, color = "gray45", lineheight = 0.95
  ) +
  scale_x_date(
    breaks = as.Date(paste0(seq(2016, 2021, 1), "-01-01")),
    date_labels = "%Y",
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  scale_y_continuous(
    labels = label_number(scale_cut = cut_short_scale()),
    limits = c(0, 380000),
    expand = expansion(mult = c(0, 0.02))
  ) +
  scale_color_manual(values = model_pal, aesthetics = c("color", "fill")) +
  coord_cartesian(clip = "off") +
  labs(
    title = "Every model missed the crash",
    subtitle = str_glue(
      "Four models trained on Brazilian vehicle production through <b>Dec 2019</b>, then forecasting 24 months. ",
      "All confidently project a normal 2020 (shaded bands = 95% intervals). ",
      "The <b style='color:{col_actual}'>actual</b> output plunges straight through every forecast as COVID shutters the plants."
    ),
    caption = "Source: Brazilian Central Bank (BCB) / Anfavea — monthly vehicle production (units). • @viniciusoike",
    x = NULL,
    y = NULL
  ) +
  theme_plot +
  guides(
    color = guide_legend(override.aes = list(linewidth = 1.2), order = 1),
    fill = "none"
  )

# Static poster (final frame) ---------------------------------------------

ggsave(
  here("2026/plots/27_animation.png"),
  p,
  width = 8,
  height = 5,
  dpi = 300
)

# Animate -----------------------------------------------------------------

anim <- p +
  transition_reveal(date) +
  ease_aes("linear")

gif <- animate(
  anim,
  nframes = 120,
  fps = 20,
  end_pause = 25,
  width = 8,
  height = 5,
  units = "in",
  res = 120,
  device = "ragg_png",
  renderer = magick_renderer(loop = TRUE)
)

anim_save(here("2026/plots/27_animation.gif"), gif)
