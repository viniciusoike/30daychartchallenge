library(tidyverse)
library(countries)

import::from(here, here)

urbanization <- read_csv(here(
  "2026/data/urbanization/urban-and-rural-population/urban-and-rural-population.csv"
))

urban_share <- read_csv(here(
  "2026/data/urbanization/share-urban-and-rural-population/share-urban-and-rural-population.csv"
))

pop <- country_info(
  unique(urban_share$code),
  fields = "population"
)


urban_share <- urban_share |>
  janitor::clean_names() |>
  pivot_longer(cols = urban:rural) |>
  separate(name, into = c("urban", "fcast"), sep = "_", fill = "right") |>
  mutate(fcast = if_else(is.na(fcast), "past", fcast))

urban_share <- left_join(urban_share, pop, by = join_by(code == countries))

urban_share |>
  filter(str_detect(entity, "World")) |>
  ggplot(aes(year, value, color = urban, linetype = fcast)) +
  geom_line()

urban_share |>
  filter(year == 1970, population > 1e5, urban == "urban") |>
  slice_max(value, n = 10)

urban_share |>
  filter(population > 1e5, urban == "urban", fcast == "past") |>
  mutate(roll_chg = value - lag(value, 10), .by = "code") |>
  filter(year == 2000) |>
  slice_max(roll_chg, n = 10)

urban_share |>
  filter(population > 1e6, urban == "urban", fcast == "past") |>
  summarise(
    total_chg = last(value, na_rm = TRUE) - first(value, na_rm = TRUE),
    .by = "entity"
  ) |>
  arrange(desc(total_chg)) |>
  head(20)


sel_countries <- c(
  "Angola",
  "Libya",
  "Turkey",
  "Brazil",
  "China",
  "India",
  "Japan",
  "South Korea",
  "Indonesia",
  "Germany",
  "United Kingdom",
  "United States"
)

country_flags <- c(
  "Angola" = "\U0001F1E6\U0001F1F4",
  "Libya" = "\U0001F1F1\U0001F1FE",
  "Turkey" = "\U0001F1F9\U0001F1F7",
  "Brazil" = "\U0001F1E7\U0001F1F7",
  "China" = "\U0001F1E8\U0001F1F3",
  "India" = "\U0001F1EE\U0001F1F3",
  "Japan" = "\U0001F1EF\U0001F1F5",
  "South Korea" = "\U0001F1F0\U0001F1F7",
  "Indonesia" = "\U0001F1EE\U0001F1E9",
  "Germany" = "\U0001F1E9\U0001F1EA",
  "United Kingdom" = "\U0001F1EC\U0001F1E7",
  "United States" = "\U0001F1FA\U0001F1F8"
)

flag_labeller <- labeller(
  entity = function(x) paste(country_flags[x], x)
)

urban_series <- urban_share |>
  filter(
    entity %in% sel_countries,
    fcast == "past",
    urban == "urban",
    !is.na(value)
  ) |>
  mutate(
    entity = factor(entity, levels = sel_countries)
  )

offwhite <- "#f8fbf8"
main_color <- "#1B3A4B"

urban_endpoints <- urban_series |>
  filter(year %in% c(1950, 2025)) |>
  mutate(
    label_num = scales::number(value, accuracy = 0.1, suffix = "%"),
    label_pos = if_else(value > 80, value - 15, value + 15)
  )

plot_final <- ggplot(urban_series, aes(year, value)) +
  geom_line(lwd = 0.6, color = main_color) +
  geom_hline(yintercept = 0) +
  geom_point(
    data = urban_endpoints,
    color = main_color
  ) +
  geom_label(
    data = urban_endpoints,
    aes(y = label_pos, label = label_num),
    family = "Roboto Slab",
    size = 2,
    color = "#000000"
  ) +
  facet_wrap(vars(entity), ncol = 3, labeller = flag_labeller) +
  scale_x_continuous(
    breaks = c(seq(1950, 2020, 10), 2025),
    labels = c("1950", "60", "70", "80", "90", "2000", "10", "20", "25"),
    expand = expansion(0.06)
  ) +
  scale_y_continuous(
    breaks = seq(0, 100, 20),
    limits = c(0, 100),
    expand = expansion(mult = c(0, 0))
  ) +
  labs(
    title = "A Giant Leap (for some more than others)",
    subtitle = "Share of total population living in urban areas, based on each country's national definition (historical estimates).",
    caption = str_wrap(
      "Source: OWID (United Nations Department of Economic and Social Affairs, Population Division (2025) – with major processing by Our World in Data (OWID) • @viniciusoike",
      width = 91
    ),
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_family = "Roboto Slab") +
  theme_sub_plot(
    title = element_text(size = 16, family = "Georgia"),
    subtitle = element_text(size = 10, color = "gray40"),
    caption = element_text(size = 8, color = "gray60", hjust = 0),
    margin = margin(15, 10, 10, 10),
    background = element_rect(fill = offwhite, color = offwhite)
  ) +
  theme_sub_panel(
    grid.minor = element_blank(),
    grid.major.x = element_blank(),
    grid.major.y = element_line(linewidth = 0.3),
    background = element_rect(fill = offwhite, color = offwhite)
  ) +
  theme_sub_strip(
    background = element_rect(fill = main_color, color = main_color),
    text = element_text(size = 8, color = "#ffffff")
  ) +
  theme_sub_axis(
    text = element_text(size = 6, color = "gray20")
  ) +
  theme_sub_axis_x(
    ticks = element_line(color = "gray20"),
    line = element_line(color = "gray20", linewidth = 0.5),
    text = element_text(color = "gray20", size = c(7, 6, 6, 6, 6, 7, 6, 6, 7))
  ) +
  theme_sub_axis_y(
    title = element_blank()
  )

ggsave(
  here::here("2026", "plots", "20_global_change.png"),
  plot_final,
  width = 8,
  height = 6,
  dpi = 400
)
