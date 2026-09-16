# Prompt: Time series
# Evolution -- Brazil's total crop harvest area, 1974-2024 (IBGE PAM)

library(dplyr)
library(ggplot2)

import::from(here, here)
import::from(janitor, clean_names)
import::from(readr, write_rds)
import::from(tibble, tibble, as_tibble)
import::from(ggflags, geom_flag)
import::from(ggtext, geom_richtext)
import::from(scales, label_number)

# Data --------------------------------------------------------------------

api_query <- "https://apisidra.ibge.gov.br/values/t/5457/n1/all/v/allxp/p/all/c782/all"

api_query <- paste0(
  "https://servicodados.ibge.gov.br/api/v3/agregados/5457/periodos/all/variaveis/allxp",
  "?localidades=N1[all]&classificacao=782[all]&view=flat"
)

req <- httr::GET(api_query, httr::user_agent("Mozilla/5.0"))
httr::stop_for_status(req, task = "download PAM table 5457 from SIDRA")

raw <- jsonlite::fromJSON(
  httr::content(req, as = "text", encoding = "UTF-8"),
  simplifyDataFrame = TRUE
)

# First row holds the column labels (Ano, Variável, Valor, ...); the rest is data
dat <- raw[-1, ] |>
  setNames(unlist(raw[1, ])) |>
  as_tibble() |>
  clean_names()

brazil <- dat |>
  select(
    year = ano,
    variable = variavel,
    code_crop = produto_das_lavouras_temporarias_e_permanentes_codigo,
    name_crop = produto_das_lavouras_temporarias_e_permanentes,
    value = valor
  ) |>
  # SIDRA codes missing values as "-", "..", "..." or "X"
  mutate(
    year = as.numeric(year),
    value = as.numeric(if_else(value %in% c("-", "..", "...", "X"), NA, value))
  )

write_rds(brazil, here("2026/data/agriculture/pam_br.rds"))

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

# Stale: crop-level labels from an earlier multi-crop version of the chart;
# the final plot only uses brazil_area (the "Total" series).
# crops_selected <- c(
#   "Milho (em grão)",
#   "Soja (em grão)",
#   "Trigo (em grão)",
#   "Arroz (em casca)",
#   "Café (em grão) Total",
#   "Cana-de-açúcar",
#   "Algodão herbáceo (em caroço)"
# )
#
# crops_labels <- c(
#   "Corn",
#   "Soybeans",
#   "Wheat",
#   "Rice",
#   "Coffee",
#   "Sugar cane",
#   "Cotton"
# )
#
# crops_labels <- setNames(crops_labels, crops_selected)
#
# brazil <- brazil |>
#   mutate(
#     crop_label = case_when(
#       name_crop %in% crops_selected ~ crops_labels[name_crop],
#       name_crop == "Total" ~ "Total",
#       TRUE ~ "Other"
#     )
#   )

# Annotation data ----------------------------------------------------------

# https://en.wikipedia.org/wiki/List_of_countries_and_dependencies_by_area

country_sizes <- tibble(
  country = c("France", "Spain", "Nigeria"),
  size = c(640427, 498980, 910770) * 1e2,
  code = c("fr", "es", "ng"),
  label = c(
    "<b>France</b><br>64.0M Ha",
    "<b>Spain</b><br>49.9M Ha",
    "<b>Nigeria</b><br>91.1M Ha"
  )
)

df_labels <- tibble(
  x = c(2000, 2023),
  label = c(
    "Just before the Commodity Boom,<br>in the early 2000's, Brazil's total crop<br>harvest area hovered around<br> <b>50 million Ha</b>: roughly the size of Spain.",
    "By 2023, harvest area had grown<br>to over <b>95 M Ha</b>, surpassing<br>the land area of Nigeria."
  )
)

df_labels <- left_join(df_labels, brazil_area, by = join_by(x == year))

# Theme ---------------------------------------------------------------------

color_main <- "#2c7b46"
offwhite <- "#f8fbf8"

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

# Plot ----------------------------------------------------------------------

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
    aes(x = c(2001.5, 2009.5), y = c(44, 96) * 1e6, label = label),
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
    labels = label_number(scale = 1e-6),
    limits = c(NA, 10 * 1e7)
  ) +
  labs(
    title = "Sowing the crops: Brazil harvests over a Nigeria per year",
    subtitle = "Total crop harvest area (temporary and permanent cultures) in Brazil, 1974-2024.",
    caption = "Source: IBGE (PAM, 1974-2024). Country sizes are total land sizes (excluding inland water) • @viniciusoike",
    x = NULL,
    y = "Hectares (millions)"
  ) +
  theme_plot

ggsave(
  here("2026/plots/19_evolution.png"),
  final_plot,
  width = 8,
  height = 5,
  dpi = 400
)
