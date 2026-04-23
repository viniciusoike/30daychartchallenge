library(dplyr)
library(ggplot2)
library(ggflags)
library(ggtext)

import::from(tidyr, pivot_wider)

api_query <- "/t/5457/n1/all/v/216,1000216/p/all/c782/0,40092,40099,40100,40101,40102,40103,40104,40105,40106,40107,40108,40109,40110,40111,40112,40113,40114,40115,40116,40117,40118,40119,40120,40121,40122,40123,40124,40125,40126,40127,40128,40129,40130,40131,40136,40137,40138,40139,40142,40143,40144,40145,40146,40147,40148,40149,40150,40151,40152,40260,40261,40262,40263,40264,40265,40266,40267,40268,40269,40270,40271,40272,40273,40274,40329,40330,40331,40468,45982/d/v1000216%202"

dat <- sidrar::get_sidra(api = api_query)

dat <- dat |>
  as_tibble() |>
  janitor::clean_names()

brazil <- dat |>
  select(
    year = ano,
    variable = variavel,
    code_crop = produto_das_lavouras_temporarias_e_permanentes_codigo,
    name_crop = produto_das_lavouras_temporarias_e_permanentes,
    value = valor
  ) |>
  mutate(year = as.numeric(year))

brazil_area <- brazil |>
  filter(name_crop == "Total", variable == "Área colhida")


# brazil |>
#   filter(name_crop != "Total", variable == "Área colhida") |>
#   group_by(year) |>
#   slice_max(value, n = 7) |>
#   mutate(rank = rank(-value)) |>
#   ungroup() |>
#   pivot_wider(
#     id_cols = "rank",
#     names_from = "year",
#     values_from = "name_crop"
#   )

crops_selected <- c(
  "Milho (em grão)",
  "Soja (em grão)",
  "Trigo (em grão)",
  "Arroz (em casca)",
  "Café (em grão) Total",
  "Cana-de-açúcar",
  "Algodão herbáceo (em caroço)"
)

crops_labels <- c(
  "Corn",
  "Soybeans",
  "Wheat",
  "Rice",
  "Coffee",
  "Sugar cane",
  "Cotton"
)

crops_labels <- setNames(crops_labels, crops_selected)

crops_labels["Arroz (em casca)"]

brazil <- brazil |>
  mutate(
    crop_label = case_when(
      name_crop %in% crops_selected ~ crops_labels[name_crop],
      name_crop == "Total" ~ "Total",
      TRUE ~ "Other"
    )
  )

# Total area

country_sizes <- tibble(
  country = c("France", "Spain", "Pakistan"),
  size = c(643801, 505370, 882363) * 1e2,
  code = c("fr", "es", "pk"),
  label = c(
    "<b>France</b><br>64.4M Ha",
    "<b>Spain</b><br>50.5M Ha",
    "<b>Pakistan</b><br>88.2M Ha"
  )
)

df_labels <- tibble(
  x = c(2000, 2022),
  label = c(
    "Just before the Commodity Boom,<br>in the early 2000's, Brazil's total crop<br>harvest area hovered around<br> <b>50 million Ha</b>: roughly the size of Spain.",
    "By 2022, harvest area had grown<br>to over <b>90 M Ha</b>, surpassing<br>the total size of Pakistan."
  )
)

df_labels <- left_join(df_labels, brazil_area, by = join_by(x == year))

color_main <- "#2c7b46"

year_breaks <- c(1974, seq(1980, 2020, 10), 2024)

theme_plot <- theme_minimal(base_family = "Roboto Slab") +
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
    text = element_text(color = "gray20", size = c(10, 8, 8, 8, 8, 8, 10))
  ) +
  theme_sub_axis_y(
    title = element_blank()
  )

base_plot <- ggplot(brazil_area, aes(year, value)) +
  # Points (all data), dimmed
  geom_point(
    shape = 21,
    size = 2,
    color = "white",
    fill = color_main,
    alpha = 0.6
  ) +
  # Smooth loess trend
  geom_smooth(
    method = "loess",
    se = FALSE,
    span = 0.35,
    color = color_main,
    lwd = 0.9
  ) +
  # Highlight points
  geom_point(
    data = df_labels,
    aes(x, value),
    shape = 21,
    color = "#000000",
    fill = color_main,
    size = 2
  )

plot_annotations <- base_plot + # Horizontal lines indicating country sizes
  geom_hline(
    data = country_sizes,
    aes(yintercept = size),
    color = "gray10",
    lwd = 0.4,
    lty = 2
  ) +
  # Flags indicating countries
  geom_flag(
    data = country_sizes,
    aes(x = 2025, y = size, country = code),
    size = 8
  ) +
  # Round border around flags
  geom_point(
    data = country_sizes,
    aes(x = 2025, y = size),
    shape = 21,
    fill = NA,
    color = "#000000",
    size = 8.2
  ) +
  # Country size labels
  geom_richtext(
    data = country_sizes,
    aes(x = 2027, y = size, label = label),
    size = 3,
    hjust = 0,
    family = "Roboto"
  ) +
  # Narrative data labels
  geom_richtext(
    data = df_labels,
    aes(x = c(2001.5, 2008), y = c(45, 93) * 1e6, label = label),
    size = 2.5,
    hjust = 0,
    family = "Roboto"
  )

final_plot <- plot_annotations +
  scale_x_continuous(
    breaks = year_breaks,
    expand = expansion(mult = c(0.025, 0.1))
  ) +
  scale_y_continuous(
    breaks = seq(4, 10, 1) * 1e7,
    labels = scales::label_number(scale = 1e-6),
    limits = c(NA, 10 * 1e7)
  ) +
  labs(
    title = "Sowing the crops: Brazil harvests over a Pakistan per year",
    subtitle = "Total crop harvest area (temporary and permanent cultures) in Brazil, 1974-2024.",
    caption = "Source: IBGE (PAM, 1974-2024) • @viniciusoike",
    x = NULL,
    y = "Hectares (millions)"
  ) +
  theme_plot

ggsave(
  "2026/plots/19_evolution.png",
  final_plot,
  width = 8,
  height = 5,
  dpi = 400
)
