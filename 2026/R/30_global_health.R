# Prompt: Uncertainties
# Global Health Data Exchange -- Brazil's epidemiological transition
#
# Data: IHME Global Burden of Disease (GBD 2021), via the GBD Results Tool
#   https://vizhub.healthdata.org/gbd-results/
# Query to reproduce 2026/data/gbd_brazil_causes.csv:
#   GBD Estimate : Cause of death or injury
#   Measure      : Deaths        (or DALYs)
#   Metric       : Percent       (share of the total -- this is the y-axis)
#   Cause        : Level 1  (the three top-level groups, see `level1` below)
#   Location     : Brazil
#   Age          : All ages
#   Sex          : Both
#   Year         : 1990 .. 2021
# The download is a tidy CSV with columns: measure, location, sex, age,
# cause, metric, year, val, upper, lower.

library(dplyr)
library(ggplot2)
library(ggtext)

import::from(here, here)
import::from(janitor, clean_names)
import::from(readr, read_csv)

# Data --------------------------------------------------------------------

# GBD Level-1 cause groups. Labels are shortened for the chart; the join
# below keys on the verbatim GBD strings.
level1 <- c(
  "Communicable, maternal, neonatal, and nutritional diseases" = "Communicable, maternal & neonatal",
  "Non-communicable diseases"                                  = "Non-communicable diseases",
  "Injuries"                                                   = "Injuries"
)

raw <- read_csv(here("2026", "data", "gbd_brazil_causes.csv")) |>
  clean_names()

dat <- raw |>
  filter(
    location == "Brazil",
    sex == "Both",
    age == "All ages",
    metric == "Percent",
    cause %in% names(level1)
  ) |>
  transmute(
    year,
    cause = factor(level1[cause], levels = unname(level1)),
    share = val
  ) |>
  arrange(cause, year)

# Endpoint labels for the slope-style direct annotation on the right edge.
ends <- dat |>
  filter(year == max(year)) |>
  mutate(label = scales::percent(share, accuracy = 1))

# Theme -------------------------------------------------------------------

base_text <- "Lato"
title_text <- "Lora"
offwhite <- "#f8fbf8"

# Communicable (receding) -> NCDs (rising) -> injuries.
pal <- c(
  "Communicable, maternal & neonatal" = "#2c7c5f",
  "Non-communicable diseases"         = "#bc6c25",
  "Injuries"                          = "#6a4c93"
)

theme_plot <- theme_minimal(base_family = base_text) +
  theme_sub_panel(
    grid.minor = element_blank(),
    grid.major.x = element_blank(),
    grid.major.y = element_line(color = "gray80", linewidth = 0.3),
    background = element_rect(fill = offwhite, color = offwhite)
  ) +
  theme_sub_plot(
    background = element_rect(fill = offwhite, color = offwhite),
    margin = margin(14, 16, 10, 14),
    title = element_text(family = title_text, size = 18, hjust = 0),
    subtitle = element_markdown(
      family = title_text,
      size = 10,
      color = "gray20",
      margin = margin(2, 0, 12, 0)
    ),
    caption = element_text(
      family = base_text,
      size = 6,
      color = "gray50",
      hjust = 0
    )
  ) +
  theme_sub_axis_x(
    line = element_line(color = "gray20", linewidth = 0.3),
    text = element_text(family = base_text, size = 9, color = "gray20")
  ) +
  theme_sub_axis_y(
    text = element_text(family = base_text, size = 9, color = "gray20"),
    title = element_blank()
  ) +
  theme(legend.position = "none")

# Plot --------------------------------------------------------------------

p <- ggplot(dat, aes(year, share, color = cause)) +
  geom_line(linewidth = 1.2) +
  geom_point(
    data = ends,
    size = 2.4
  ) +
  geom_text(
    data = ends,
    aes(label = paste0(cause, ": ", label)),
    hjust = 0,
    nudge_x = 0.6,
    family = base_text,
    size = 3,
    fontface = "bold"
  ) +
  scale_color_manual(values = pal) +
  scale_x_continuous(
    breaks = seq(1990, 2020, 10),
    expand = expansion(mult = c(0.02, 0.34))
  ) +
  scale_y_continuous(
    labels = scales::label_percent(),
    limits = c(0, NA),
    expand = expansion(mult = c(0, 0.04))
  ) +
  labs(
    title = "Brazil's epidemiological transition",
    subtitle = "Share of all deaths by cause group, 1990&ndash;2021. The burden has shifted away from infectious, maternal and<br>neonatal causes toward the chronic, non-communicable diseases of an older, more urban population.",
    caption = "Source: IHME, Global Burden of Disease (GBD 2021), via the Global Health Data Exchange. @viniciusoike\nCause groups are GBD Level 1; shares are of total deaths, both sexes, all ages.",
    x = NULL,
    y = NULL
  ) +
  theme_plot

ggsave(
  here("2026/plots/30_global_health.png"),
  p,
  width = 9,
  height = 6,
  dpi = 400
)
