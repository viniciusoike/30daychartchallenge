library(dplyr)
library(ragg)
library(ggplot2)
import::from(readr, read_csv)
import::from(stringr, str_to_title)
import::from(tidyr, pivot_longer)
import::from(here, here)

dat <- read_csv("data/day_21/global-fossil-fuel-consumption/global-fossil-fuel-consumption.csv")

series <- dat |>
  rename(
    entity = Entity,
    code = Code,
    year = Year,
    gas = 4,
    oil = 5,
    coal = 6
  ) |>
  pivot_longer(cols = gas:coal, names_to = "fuel", values_to = "consumption")

subseries <- series |>
  filter(year >= 1920) |>
  mutate(
    fuel = factor(fuel, levels = c("oil", "gas", "coal"))
  )

font <- "Roboto Slab"
cores <- c("#0a9396", "#ee9b00", "#ae2012")
cores <- c("#22223b", "#4a4e69", "#9a8c98")
offwhite <- "#fefefe"

p_fossil <- ggplot(subseries, aes(year, consumption, fill = fuel)) +
  geom_area(alpha = 0.9) +
  # geom_point(shape = 21, position = position_stack(), color = "white") +
  geom_hline(yintercept = 0) +
  geom_label(
    data = filter(subseries, year == 2018),
    aes(year, consumption, label = str_to_title(fuel), color = fuel),
    position = position_stack(vjust = 0.5),
    family = font,
    size = 4,
    inherit.aes = FALSE
  ) +
  guides(fill = "none", color = "none") +
  scale_x_continuous(
    breaks = seq(1920, 2020, 20),
    expand = expansion(add = 0, 0)
  ) +
  scale_y_continuous(
    breaks = seq(0, 140000, 20000),
    labels = scales::label_number(big.mark = "."),
    expand = expansion(add = c(0, 0.95))
  ) +
  labs(
    x = NULL,
    y = "TWh",
    title = "The continued rise of global fossil fuel consumption",
    subtitle = "Fossil fuel consumption by fuel type (TWh) from 1920 to 2023.",
    caption = "Source: OWID - Energy Institute - Statistical Review of World Energy (2024)."
  ) +
  scale_fill_manual(values = cores) +
  scale_color_manual(values = cores) +
  theme_minimal(base_family = font) +
  theme(
    plot.margin = margin(17.5, 15, 10, 15),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.major.y = element_line(linetype = 2),
    axis.text = element_text(size = 11, color = "gray20"),
    plot.title = element_text(size = 22, color = "gray10"),
    plot.subtitle = element_text(size = 11, color = "gray40"),
    plot.caption = element_text(size = 9, color = "gray40"),
    panel.background = element_rect(fill = offwhite, color = offwhite),
    plot.background = element_rect(fill = offwhite, color = offwhite)
  )

ggsave(here("plots/21_fossils.png"), p_fossil, width = 9, height = 5.1)

# ggplot(subseries, aes(year, consumption, fill = fuel, color = fuel)) +
#   geom_area(alpha = 0.5) +
#   geom_point() +
#   geom_hline(yintercept = 0) +
#   geom_label(
#     data = filter(subseries, year == 2018),
#     aes(year, consumption, label = str_to_title(fuel), color = fuel),
#     position = position_stack(vjust = 0.5),
#     family = font,
#     size = 4,
#     inherit.aes = FALSE
#   ) +
#   facet_wrap(vars(fuel)) +
#   guides(fill = "none", color = "none") +
#   scale_x_continuous(
#     breaks = seq(1920, 2020, 20)
#   ) +
#   scale_y_continuous(
#     breaks = seq(0, 140000, 20000)
#   ) +
#   scale_fill_manual(values = cores) +
#   scale_color_manual(values = cores) +
#   theme_minimal() +
#   theme(
#     panel.grid.minor = element_blank()
#   )
#
# series <- dat |>
#   janitor::clean_names()
#
# series |>
#   filter(str_detect(entity, "income")) |>
#   pull(entity) |>
#   unique()
#
# sel <- c("Brazil", "India", "China", "High-income countries", "Upper-middle-income countries", "World")
#
# top_countries <- c("BRA", "IND", "CHN", "USA", "NGA", "IDN", "PAK", "OWID_WRL")
#
# top_countries <- c("BRA", "IND", "CHN", "OWID_WRL", "USA")
#
# top_income <- c("High-income countries", "Upper-middle-income countries")
#
# dat_series <- series |>
#   filter(year >= 1960, year <= 2022) |>
#   rename(share = 4) |>
#   mutate(
#     is_highlight = factor(if_else(entity %in% sel, 1L, 0L))
#   )
#
# ggplot() +
#   # geom_line(
#   #   data = filter(dat_series, is_highlight == 0),
#   #   aes(year, share, group = code),
#   #   alpha = 0.1
#   # ) +
#   geom_line(
#     data = filter(dat_series, is_highlight == 1),
#     aes(year, share, color = entity),
#     linewidth = 1
#   ) +
#   # geom_line(
#   #   data = filter(dat_series, code == "OWID_WRL"),
#   #   aes(year, share),
#   #   linewidth = 1,
#   #   color = "black"
#   # ) +
#   scale_color_brewer() +
#   # scale_y_continuous(limits = c(0, 100)) +
#   theme_minimal() +
#   theme(
#     legend.position = "bottom"
#   )
#
# filter(dat_series, is_highlight == 1, year == 1960)
#
# View(filter(dat_series, is_highlight == 1))
