library(dplyr)
library(ggplot2)
library(ragg)
library(rbcb)
import::from(tidyr, pivot_longer)
import::from(purrr, reduce)
import::from(scales, label_percent)
import::from(here, here)

codes <- c(25120:25126)
series_names <- c(
  paste0("total_adults_bank_", c("15_24", "25_34", "35_44", "45_54", "55_64", "65")),
  "share_adults_bank"
)
names(codes) <- series_names

series <- get_series(codes)
series <- reduce(series, inner_join, by = "date")

share <- series |>
  select(date, share_adults_bank)

total <- series |>
  select(date, starts_with("total")) |>
  pivot_longer(cols = -"date") |>
  summarise(pop = sum(value), .by = "date")

total |>
  filter(date == min(date) | date == max(date)) |>
  mutate(chg = pop / lag(pop) - 1)

pal <- c("#076a6c", "#051416", "#d5dadb", "#04454a", "#098b94", "#05947c", "#fefefe")
font_title <- "Palatino"
font <- "Avenir"

final_plot <- ggplot(data = share, aes(date, share_adults_bank),) +
  geom_area(fill = pal[1], alpha = 0.8) +
  geom_point(size = 2, color = pal[4]) +
  geom_line(linewidth = 0.5, color = pal[4]) +
  geom_hline(yintercept = 0) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.01))) +
  scale_y_continuous(
    breaks = seq(0, 100, 20),
    labels = label_percent(scale = 1),
    expand = expansion(mult = c(0, 0.05))
    ) +
  labs(
    title = "Banking for all: Brazil's leap in financial inclusion",
    subtitle = "Share of Brazilian adults (15+) with a banking relationship—i.e., holding checking accounts or financial assets in a bank.\nThe absolute number nearly doubled from 86 million in 2006 to 170 million in 2022.",
    caption = "Source: Brazilian Central Bank.",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_family = font) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.background = element_rect(fill = pal[7], color = pal[7]),
    plot.background = element_rect(fill = pal[7], color = pal[7]),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.ticks.x = element_line(),
    plot.title = element_text(size = 24, family = font_title),
    plot.margin = margin(20, 15, 10, 15)
  )
cowplot::save
ggsave(here("plots/28_inclusion.png"), final_plot, width = 6 * 1.618, height = 6)
