# Day 28 - Modelling (Uncertainties): Brazil's population fan ---------------
# UN World Population Prospects 2024. The UN projects fertility with a
# Bayesian model, simulates thousands of future paths, and runs each one
# through a cohort-component projection. The spread in fertility becomes the
# spread in population: near-certain about the peak, wide open about 2100.

library(dplyr)
library(ggplot2)
library(ggtext)
library(patchwork)
import::from(here, here)
import::from(ragg, agg_png)
import::from(tibble, as_tibble)

# Data ----------------------------------------------------------------------

data("pop1dt", "popproj1dt", "tfr1dt", "tfrproj1dt", package = "wpp2024")

# Wrangle -------------------------------------------------------------------
# Population comes in thousands; convert to millions.

pop_hist <- as_tibble(pop1dt) |>
  filter(name == "Brazil", year >= 1950) |>
  mutate(pop = pop / 1000) |>
  select(year, pop)

pop_proj <- as_tibble(popproj1dt) |>
  filter(name == "Brazil") |>
  mutate(across(starts_with("pop"), \(x) x / 1000)) |>
  select(year, pop, pop_80l, pop_80u, pop_95l, pop_95u)

tfr_hist <- as_tibble(tfr1dt) |>
  filter(name == "Brazil", year >= 1950) |>
  select(year, tfr)

tfr_proj <- as_tibble(tfrproj1dt) |>
  filter(name == "Brazil") |>
  select(year, tfr, tfr_80l, tfr_80u, tfr_95l, tfr_95u)

# Join the last observed year to the projections so lines and fans connect
pop_last <- slice_max(pop_hist, year)
tfr_last <- slice_max(tfr_hist, year)

pop_proj <- bind_rows(
  mutate(pop_last, pop_80l = pop, pop_80u = pop, pop_95l = pop, pop_95u = pop),
  pop_proj
)

tfr_proj <- bind_rows(
  mutate(tfr_last, tfr_80l = tfr, tfr_80u = tfr, tfr_95l = tfr, tfr_95u = tfr),
  tfr_proj
)

## Key numbers --------------------------------------------------------------

peak <- slice_max(pop_proj, pop)
end <- filter(pop_proj, year == 2100)

fmt_mi <- function(x) {
  return(paste0(round(x), " million"))
}

# Theme ---------------------------------------------------------------------

base_text <- "Lato"
title_text <- "Lora"
offwhite <- "#f5f5dc"

col_hist <- "gray15"
col_fan <- "#466C6F"

theme_plot <- theme_minimal(base_family = base_text) +
  theme_sub_plot(
    background = element_rect(fill = offwhite, color = offwhite),
    margin = margin(8, 14, 4, 10)
  ) +
  theme_sub_panel(
    grid.minor = element_blank(),
    grid.major.x = element_blank(),
    grid.major.y = element_line(color = "gray80", linewidth = 0.3),
    background = element_rect(fill = offwhite, color = offwhite)
  ) +
  theme_sub_axis_x(
    line = element_line(color = "gray20", linewidth = 0.3),
    ticks = element_line(color = "gray20", linewidth = 0.3),
    text = element_text(size = 8, color = "gray30")
  ) +
  theme_sub_axis_y(
    text = element_text(size = 8, color = "gray30")
  ) +
  theme(
    axis.title = element_blank(),
    plot.title = element_text(
      family = base_text,
      face = "bold",
      size = 10,
      color = "gray20",
      margin = margin(0, 0, 6, 0)
    ),
    plot.title.position = "plot"
  )

x_scale <- scale_x_continuous(
  breaks = seq(1950, 2100, 25),
  limits = c(1950, 2120),
  expand = expansion(c(0.01, 0))
)

label_size <- 2.7

# Plot ----------------------------------------------------------------------

## Population ---------------------------------------------------------------

p_pop <- ggplot() +
  annotate(
    "rect",
    xmin = pop_last$year,
    xmax = 2100,
    ymin = -Inf,
    ymax = Inf,
    fill = "white",
    alpha = 0.35
  ) +
  geom_ribbon(
    data = pop_proj,
    aes(year, ymin = pop_95l, ymax = pop_95u),
    fill = col_fan,
    alpha = 0.2
  ) +
  geom_ribbon(
    data = pop_proj,
    aes(year, ymin = pop_80l, ymax = pop_80u),
    fill = col_fan,
    alpha = 0.3
  ) +
  geom_line(
    data = pop_hist,
    aes(year, pop),
    color = col_hist,
    linewidth = 0.8
  ) +
  geom_line(data = pop_proj, aes(year, pop), color = col_fan, linewidth = 0.8) +
  # peak
  geom_point(
    data = peak,
    aes(year, pop),
    color = col_fan,
    fill = offwhite,
    shape = 21,
    size = 2.2,
    stroke = 0.9
  ) +
  annotate(
    "richtext",
    x = peak$year,
    y = peak$pop + 22,
    label = paste0(
      "**Peak in ",
      peak$year,
      "**<br>",
      fmt_mi(peak$pop)
    ),
    family = base_text,
    size = label_size,
    color = col_fan,
    fill = NA,
    label.color = NA
  ) +
  # range in 2100
  annotate(
    "segment",
    x = 2100,
    xend = 2100,
    y = end$pop_95l,
    yend = end$pop_95u,
    color = col_fan,
    linewidth = 0.4
  ) +
  annotate(
    "richtext",
    x = 2103,
    y = end$pop_95u,
    label = paste0("2100: up to<br>**", fmt_mi(end$pop_95u), "**"),
    family = base_text,
    size = label_size,
    color = "gray25",
    fill = NA,
    label.color = NA,
    hjust = 0,
    vjust = 0.8
  ) +
  annotate(
    "richtext",
    x = 2103,
    y = end$pop,
    label = paste0("median<br>**", fmt_mi(end$pop), "**"),
    family = base_text,
    size = label_size,
    color = col_fan,
    fill = NA,
    label.color = NA,
    hjust = 0
  ) +
  annotate(
    "richtext",
    x = 2103,
    y = end$pop_95l,
    label = paste0("as few as<br>**", fmt_mi(end$pop_95l), "**"),
    family = base_text,
    size = label_size,
    color = "gray25",
    fill = NA,
    label.color = NA,
    hjust = 0,
    vjust = 0.2
  ) +
  annotate(
    "text",
    x = pop_last$year + 1,
    y = 30,
    label = "Projection →",
    family = base_text,
    size = label_size,
    color = "gray40",
    hjust = 0
  ) +
  annotate(
    "text",
    x = pop_last$year - 1,
    y = 30,
    label = "← Observed",
    family = base_text,
    size = label_size,
    color = "gray40",
    hjust = 1
  ) +
  x_scale +
  scale_y_continuous(
    limits = c(0, 260),
    breaks = seq(0, 250, 50),
    expand = expansion(c(0, 0.02))
  ) +
  labs(title = "Population (millions)") +
  theme_plot

## Fertility ----------------------------------------------------------------

p_tfr <- ggplot() +
  annotate(
    "rect",
    xmin = tfr_last$year,
    xmax = 2100,
    ymin = -Inf,
    ymax = Inf,
    fill = "white",
    alpha = 0.35
  ) +
  geom_hline(
    yintercept = 2.1,
    color = "gray40",
    linewidth = 0.3,
    linetype = "22"
  ) +
  annotate(
    "text",
    x = 2103,
    y = 2.1,
    label = "Replacement\nlevel (2.1)",
    family = base_text,
    size = 2.4,
    color = "gray40",
    hjust = 0,
    lineheight = 0.9
  ) +
  geom_ribbon(
    data = tfr_proj,
    aes(year, ymin = tfr_95l, ymax = tfr_95u),
    fill = col_fan,
    alpha = 0.2
  ) +
  geom_ribbon(
    data = tfr_proj,
    aes(year, ymin = tfr_80l, ymax = tfr_80u),
    fill = col_fan,
    alpha = 0.3
  ) +
  geom_line(
    data = tfr_hist,
    aes(year, tfr),
    color = col_hist,
    linewidth = 0.7
  ) +
  geom_line(data = tfr_proj, aes(year, tfr), color = col_fan, linewidth = 0.7) +
  x_scale +
  scale_y_continuous(
    limits = c(0, 6.5),
    breaks = seq(0, 6, 2),
    expand = expansion(c(0, 0.02))
  ) +
  labs(title = "Children per woman (total fertility rate)") +
  theme_plot

## Compose ------------------------------------------------------------------

p <- p_pop /
  p_tfr +
  plot_layout(heights = c(2.4, 1)) +
  plot_annotation(
    title = "Brazil stops growing in the 2040s. Then the fan opens",
    subtitle = paste0(
      "The UN projects fertility with a Bayesian model and simulates ",
      "thousands of possible futures. Each path of births becomes a path of ",
      "population. The line is the <span style='color:#466C6F'>**median**",
      "</span>; the bands hold <span style='color:#466C6F'>**80%**</span> ",
      "and <span style='color:#466C6F'>**95%**</span> of the simulations. ",
      "The median peaks in 2041, the same year IBGE's own projection gives. ",
      "After that, the range depends on how many children Brazilians have."
    ),
    caption = paste0(
      "Source: UN, World Population Prospects 2024 (probabilistic ",
      "projections, via the wpp2024 R package) • @viniciusoike"
    ),
    theme = theme(
      plot.background = element_rect(fill = offwhite, color = offwhite),
      plot.margin = margin(14, 8, 8, 8),
      plot.title = element_text(family = title_text, size = 18, hjust = 0),
      plot.subtitle = element_textbox_simple(
        family = base_text,
        size = 9.5,
        color = "gray25",
        lineheight = 1.1,
        margin = margin(6, 0, 10, 0)
      ),
      plot.caption = element_text(
        family = base_text,
        size = 6.5,
        color = "gray50",
        hjust = 0
      )
    )
  )

# Save ----------------------------------------------------------------------

ggsave(
  here("2026", "plots", "28_modelling.png"),
  p,
  width = 8,
  height = 7,
  dpi = 400,
  device = agg_png
)
