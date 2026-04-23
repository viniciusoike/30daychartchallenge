
library(ggplot2)
library(dplyr)
library(ragg)
library(readxl)
library(priceR)
library(patchwork)
library(lubridate)
import::from(here, here)

theme_elpais <- function(
    base_size = 12,
    font_text = "Lato",
    font_title = "Georgia",
    background = "white") {

  theme_minimal(base_size = base_size, base_family = font_text) +
    theme(
      plot.title = element_text(face = "bold", size = rel(1.5), family = font_title),
      plot.subtitle = element_text(size = rel(1.1), color = "#636363", margin = margin(b = 5)),
      plot.caption = element_text(size = rel(0.8), color = "#636363", hjust = 0, margin = margin(t = 10, b = 0)),

      # Axis and grid styling
      axis.title = element_text(face = "plain", size = rel(0.9), color = "#636363"),
      axis.text = element_text(size = rel(0.9)),
      panel.grid.major.y = element_line(color = "#e6e6e6", size = 0.3),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),

      # Legend styling
      legend.position = "top",
      legend.justification = "left",
      legend.title = element_text(face = "bold", size = rel(1)),
      legend.text = element_text(size = rel(1)),
      legend.margin = margin(0, 0, 0, 0, "pt"),

      # Other elements
      panel.background = element_rect(fill = background, color = background),
      plot.background = element_rect(fill = background, color = background),
      plot.margin = margin(t = 10, r = 15, b = 10, l = 15)
    )
}

elpais_palettes <- list(
  main = c("#21908C", "#F7B05B", "#DB5461", "#0F609B", "#9A348E", "#6B9E78"),
  sequential = c("#FFFFFF", "#D3E2EF", "#A7C9DF", "#7BAFD0", "#5095C0", "#247AB1", "#005FA1"),
  diverging = c("#DB5461", "#F0F0F0", "#21908C")
)

# Data --------------------------------------------------------------------


## Import sheets -----------------------------------------------------------

arabica <- read_excel(here("data/day_18/CEPEA_20250521154245.xlsx"), skip = 3)
robusta <- read_excel(here("data/day_18/CEPEA_20250521154609.xlsx"), skip = 3)

## Clean -------------------------------------------------------------------

# Format dates and adjust dollar values for inflation

arabica <- arabica |>
  rename(date = 1, spot_rs = 2, spot_us = 3) |>
  mutate(date = as.Date(date, format = "%d/%m/%Y")) |>
  arrange(date) |>
  filter(date <= as.Date("2025/04/18"))

robusta <- robusta |>
  rename(date = 1, spot_rs = 2, spot_us = 3) |>
  mutate(date = as.Date(date, format = "%d/%m/%Y")) |>
  arrange(date) |>
  filter(date <= as.Date("2025/04/18"))

series <- bind_rows(list("arabica" = arabica, "robusta" = robusta), .id = "crop")

series$usd_2022 <- adjust_for_inflation(
  series$spot_us,
  country = "US",
  from_date = series$date,
  to_date = 2022
  )

# 22-day moving average
series <- series |>
  filter(usd_2022 > 0) |>
  mutate(
    trend = RcppRoll::roll_meanr(usd_2022, n = 22, fill = NA), .by = "crop"
    )

# 1997, supply shortage, low inventory, rough winter (El Nino)
# 2025, El Nino, supply shortage

# Plot --------------------------------------------------------------------

## Plot elements -----------------------------------------------------------


font_text <- "Lato"

coffee_colors <- c("arabica" = "#D14B19", "robusta" = "#613612")

df_last_value <- series %>%
  group_by(crop) %>%
  filter(date == max(date))

dbreaks <- as.Date(paste0(c(1997, 2000, 2005, 2010, 2015, 2020, 2025), "-01-01"))

xpos <- sapply(dbreaks, \(x) which.min(abs(series$date - x)))
dfx <- series[xpos, ]

yend <- series |>
  filter(date %in% dbreaks) |>
  summarise(m = max(trend), .by = "date")


## Plot --------------------------------------------------------------------

base_plot <- ggplot() +
  geom_segment(
    data = dfx,
    aes(x = date, xend = date, y = 0, yend = trend),
    linewidth = 0.3,
    color = "#e6e6e6"
  ) +
  geom_hline(yintercept = 0) +
  geom_line(
    data = series,
    aes(x = date, y = trend, color = crop, group = crop),
    linewidth = 1.1
    ) +
  geom_label(
    data = df_last_value,
    aes(x = date + months(5), y = trend, label = paste0("US$", round(trend)), color = crop),
    nudge_x = 500,
    family = font_text,
    show.legend = FALSE
  ) +
  scale_x_date(
    breaks = dbreaks,
    date_labels = "%Y",
    expand = expansion(mult = c(0.025, 0.075))
    ) +
  scale_y_continuous(
    breaks = seq(0, 450, by = 50),
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_color_manual(
    name = "",
    values = coffee_colors,
    labels = c("Arabica", "Robusta")) +
  theme_elpais()

final_plot <- base_plot + labs(x = NULL, y = NULL) &
  plot_annotation(
    title = "For the price of a cup of coffee",
    subtitle = "Price in US$ (constant 2022). Lines show 22-day moving averages.",
    caption = "Source: CEPEA (ESALQ/USP), FRED | REstateInsights."
  ) &
  theme_elpais() +
  theme(legend.position = c(-.05, 1), legend.direction = "horizontal")

ggsave(
  here("plots/18_el_pais.png"),
  final_plot,
  width = 6 * 1.618, height = 6
)
