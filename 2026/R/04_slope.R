# Prompt: Comparisons — Slope
# Road-traffic deaths 1980 vs 2019 for six countries. Source: WHO Mortality DB.

library(dplyr)
library(ggplot2)

import::from(ragg, agg_png)
import::from(readr, read_csv)
import::from(janitor, clean_names)
import::from(stringr, str_remove, str_glue, str_wrap)
import::from(scales, number, label_number)
import::from(ggtext, geom_richtext)
import::from(ggflags, geom_flag)
import::from(here, here)

options(scales.big.mark = ".")
options(scales.decimal.mark = ",")

# Data --------------------------------------------------------------------

who <- read_csv(
  here("2025/data/day_24/WHOMortalityDatabaseRoad traffic accidents.csv"),
  skip = 6
)

who <- who |>
  clean_names() |>
  rename(
    death_rate = death_rate_per_100_000_population,
    death_rate_adj = age_standardized_death_rate_per_100_000_standard_population,
    pct_deaths = percentage_of_cause_specific_deaths_out_of_total_deaths
  ) |>
  mutate(
    death_rate = as.numeric(str_remove(death_rate, ","))
  )

# Ranking check (exploratory, not run)
# who |>
#   filter(
#     number > 0,
#     year == 1980 | year == 2019,
#     age_group == "[All]",
#     sex == "All"
#   ) |>
#   arrange(year) |>
#   summarise(
#     t0 = first(year),
#     t1 = last(year),
#     m0 = first(number),
#     m1 = last(number),
#     dm = (last(number) / first(number) - 1) * 100,
#     da = first(number) - last(number),
#     .by = "country_name"
#   ) |>
#   arrange(desc(da))

# Wrangle -----------------------------------------------------------------

# Select countries
sel_countries <- c("BRA", "USA", "EGY", "JPN", "GBR", "RUS")

# Total traffic deaths per country, 1980-2019
total_trend <- who |>
  filter(
    sex == "All",
    age_group == "[All]",
    country_code %in% sel_countries,
    year >= 1980,
    year <= 2019
  ) |>
  mutate(
    country_name = if_else(country_code == "GBR", "GBR", country_name),
    country_name = if_else(country_code == "USA", "USA", country_name),
    country_name = if_else(country_code == "RUS", "Russia", country_name),
    year = as.numeric(year)
  )

# Flags and value labels at the 2019 endpoint -----------------------------

flag_codes <- c(
  "GBR" = "gb",
  "Brazil" = "br",
  "USA" = "us",
  "Egypt" = "eg",
  "Japan" = "jp",
  "Russia" = "ru"
)

df_label <- total_trend |>
  arrange(year) |>
  summarise(
    t0 = first(year),
    t1 = last(year),
    m0 = first(number),
    m1 = last(number),
    dm = (last(number) / first(number) - 1) * 100,
    .by = "country_name"
  ) |>
  mutate(
    label_num0 = number(m0, scale = 1e-3, suffix = "k", accuracy = 0.1),
    label_num1 = number(m1, scale = 1e-3, suffix = "k", accuracy = 0.1),
    label_diff = number(dm, scale = 1, suffix = "%", accuracy = 0.1),
    label = str_glue(
      "<b>{country_name} ({label_diff})</b><br>{label_num0} ({t0}) → {label_num1} ({t1})"
    )
  )

df_ends <- total_trend |>
  filter(year == max(year)) |>
  mutate(
    code = flag_codes[country_name],
    pos_y = case_when(
      country_name == "Japan" ~ number + 1250,
      country_name == "Egypt" ~ number + 2000,
      TRUE ~ number
    )
  ) |>
  left_join(df_label, by = "country_name")

# Plot ------------------------------------------------------------------

## Plot elements -----------------------------------------------------------

offwhite <- "#fefefe"

# paletteer::paletteer_d("ggthemes::Green_Orange_Teal")

# ekioplot::ekio_blue
# "#0D1B2A" "#1B3A4B" "#1E3A5F" "#2B4C7E" "#3A6EA5" "#4A90C2" "#7EB6D8" "#A8D0E8" "#D4E8F5" "#EEF5FA"
# [1] "#1E3A5F" "#DD6B20" "#2C7A7B" "#D69E2E" "#805AD5" "#C53030"
pal_colors <- c(
  "GBR" = "#C53030",
  "Brazil" = "#2C7A7B",
  "USA" = "#1E3A5F",
  "Egypt" = "#0D1B2A",
  "Japan" = "#D69E2E",
  "Russia" = "#7EB6D8"
)

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

## Base plot ---------------------------------------------------------------

slope_plot <- ggplot(
  subset(total_trend, year %in% c(1980, 2019)),
  aes(year, number, color = country_name, fill = country_name)
) +
  geom_line(lwd = 1) +
  geom_point(size = 3, shape = 21, stroke = 1, color = "#000000") +
  geom_flag(
    data = df_ends,
    aes(x = 2021, y = pos_y, country = code),
    size = 7,
    inherit.aes = FALSE
  ) +
  geom_point(
    data = df_ends,
    aes(x = 2021, y = pos_y),
    shape = 21,
    fill = NA,
    color = "#000000",
    size = 7.2,
    inherit.aes = FALSE
  ) +
  geom_richtext(
    data = df_ends,
    aes(x = 2023, y = number, label = label),
    hjust = 0,
    family = font_text,
    size = 2,
    nudge_y = c(0, 3.75, 0, 2.2, 0, 0) * 1000,
    inherit.aes = FALSE
  ) +
  scale_x_continuous(
    breaks = c(1980, 2019),
    expand = expansion(mult = c(0.05, 0.22))
  ) +
  scale_y_continuous(
    breaks = seq(0, 50000, 10000),
    labels = label_number(big.mark = ".")
  ) +
  scale_color_manual(values = pal_colors) +
  scale_fill_manual(values = pal_colors) +
  guides(color = "none", fill = "none") +
  labs(
    title = "Crash Course: Who Actually Hit the Brakes?",
    subtitle = str_wrap(
      "Annual road-traffic deaths, 1980 vs 2019 in absolute numbers. Britain and Japan cut theirs by ~70%. Brazil (+56%) and Egypt (+137%) drove the other way.",
      111
    ),
    caption = "Source: WHO Mortality Database.",
    x = NULL,
    y = NULL
  ) +
  theme_plot

# Save --------------------------------------------------------------------

ggsave(
  here("2026/plots/04_slope.png"),
  slope_plot,
  width = 8,
  height = 5,
  dpi = 400,
  device = agg_png
)
