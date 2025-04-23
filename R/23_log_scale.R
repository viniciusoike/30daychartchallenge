library(ggplot2)
library(dplyr)
library(patchwork)
import::from(readxl, read_excel)

# Data --------------------------------------------------------------------

dat <- read_excel("data/day_8/mpd2023_web.xlsx", sheet = 5)

countries <- c(
  "BRA", "CAN", "MEX", "USA",
  "AUS", "CHN", "IND", "JPN",
  "FRA", "DEU", "POL", "GBR"
  )

countries <- c(
  "BRA", "USA",
  "CHN", "JPN",
  "DEU", "GBR"
)

dim_country <- dat |>
  filter(countrycode %in% countries) |>
  count(countrycode, country) |>
  select(1:2)

xcountries <- dim_country |>
  mutate(
    countrycode = factor(countrycode, levels = countries)
  ) |>
  arrange(countrycode) |>
  pull(country)

dim_country <- tibble(
  code_country = countries,
  name_country = xcountries
)

dim_country <- dim_country |>
  mutate(
    name_label = case_when(
      code_country == "GBR" ~ "UK",
      code_country == "USA" ~ "USA",
      TRUE ~ name_country
    )
  )

subdat <- dat |>
  rename(
    code_country = countrycode,
    name_country = country
  ) |>
  filter(year >= 1940, code_country %in% countries) |>
  mutate(
    name_country = factor(name_country, levels = dim_country$name_country, labels = dim_country$name_label),
    gdp = gdppc * pop,
    lgdp = log10(gdp)
  )

# Plot elements -----------------------------------------------------------

pal_qual <- MetBrewer::met.brewer("Hokusai1", 24)
pal_blues <- MetBrewer::met.brewer("Hokusai2", 12)

inds <- c(2, 5, 8, 14, 30, 33, 36, 39, 40, 42, 44, 48)
inds <- c(4, 8, 10, 14, 20, 22)
cores <- c(pal_blues[inds[1:2]], pal_qual[inds[3:6]])
font <- "Avenir"

offwhite <- "#fefaee"

theme_plot <- theme_minimal(base_family = font) +
  theme(
    plot.margin = margin(15, 10, 10, 10),
    panel.background = element_rect(fill = offwhite, color = offwhite),
    plot.background = element_rect(fill = offwhite, color = offwhite),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    legend.position = "none",
    axis.text = element_text(size = 12, color = "gray30"),
    axis.title.x = element_blank(),
    axis.ticks.x = element_line(color = "gray40"),
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.ticks.length = unit(7, "pt"),
    plot.subtitle = element_text(size = 9, color = "gray20", hjust = 0.5, face = "bold"),
    plot.caption = element_text(size = 9, color = "gray40")
  )


# Plots -------------------------------------------------------------------



df_text <- tibble(
  x = 1940,
  y = seq(0, 30, 5),
  label = ifelse(y == 30, paste(y, "(US$ constant 2011, Trillion)"), y)
  )

p1 <- ggplot(subdat, aes(year, gdp / 1e9, color = name_country)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 0, linewidth = 1, color = "gray20") +
  # Country labels (right)
  geom_label(
    data = filter(subdat, year == max(year)),
    aes(label = name_country),
    family = font,
    size = 4,
    hjust = 0,
    nudge_x = 1,
    nudge_y = c(0, 0, 0.8, -1, 1.6, 0)
  ) +
  # Axis text labels (left)
  geom_text(
    data = df_text,
    aes(x, y, label = label),
    family = font,
    color = "gray30",
    size = 4,
    hjust = 0,
    nudge_x = -3,
    nudge_y = 0.8,
    inherit.aes = FALSE
  ) +
  scale_x_continuous(
    breaks = seq(1920, 2020, 10),
    expand = expansion(mult = c(0.05, 0.15))
    ) +
  scale_y_continuous(
    breaks = seq(0, 30, 5),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(subtitle = "Linear scale") +
  scale_color_manual(values = cores) +
  theme_plot

df_text <- tibble(
  x = 1940,
  y = seq(8, 10.5, 0.5),
  label = ifelse(y == 10.5, paste(y, "(US$ constant 2011, log-scale)"), y)
)

p2 <- ggplot(subdat, aes(year, lgdp, color = name_country)) +
  geom_line(linewidth = 1) +
  geom_hline(yintercept = 7.6, linewidth = 1, color = "gray20") +
  # Country labels (right)
  geom_label(
    data = filter(subdat, year == max(year)),
    aes(label = name_country),
    family = font,
    size = 4,
    hjust = 0,
    nudge_x = 1,
    nudge_y = c(0, 0, 0.6, -0.6, 1.2, -0.2) / 10
  ) +
  # Axis text labels (left)
  geom_text(
    data = df_text,
    aes(x, y, label = label),
    family = font,
    color = "gray30",
    size = 4,
    hjust = 0,
    nudge_x = -5,
    nudge_y = 0.08,
    inherit.aes = FALSE
  ) +
  scale_x_continuous(
    breaks = seq(1920, 2020, 10),
    expand = expansion(mult = c(0.05, 0.15))
  ) +
  scale_y_continuous(
    breaks = seq(8, 10.5, 0.5),
    expand = expansion(mult = c(0, 0.05))
  ) +
  labs(subtitle = "Log scale") +
  scale_color_manual(values = cores) +
  theme_plot


# Panel -------------------------------------------------------------------

panel <- p1 | p2

panel <- panel + plot_annotation(
  title = "Always remember to log your (economic) variables",
  subtitle = "Whether using `log10()` or `log()`, log-transforming your data reduces variance, attenuates heteroskedasticity, linearizes exponential growth, improves model\ninterpretability, and often enchances forecast precision. Plots show GDP estimates in constant 2011 US$ for a selection of countries from 1940 to 2023.",
  caption = "Source: Maddison Project (2024).") &
  theme(
    plot.margin = margin(15, 10, 5, 10),
    panel.background = element_rect(fill = offwhite, color = offwhite),
    plot.background = element_rect(fill = offwhite, color = offwhite),
    plot.title = element_text(size = 22, family = font),
    plot.subtitle = element_text(size = 12, family = font, color = "gray30"),
    plot.caption = element_text(size = 12, family = font, color = "gray40")
  )

# Export
ggsave("plots/23_log_scale.png", panel, width = 13.5, height = 6.4)
