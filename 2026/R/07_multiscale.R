# Prompt: Distributions
# Multiscale -- modal share of commute trips by city size (Census 2022)

library(dplyr)
library(ggplot2)
library(stringr)
library(ggtext)

import::from(forcats, fct_reorder)
import::from(here, here)
import::from(sidrar, get_sidra)
import::from(janitor, clean_names)

# Data --------------------------------------------------------------------

cache_file <- here("2026", "data", "census_modes", "modes_muni.rds")

cols_select <- c(
  "code_muni" = "municipio_codigo",
  "mode" = "meio_de_transporte_em_que_passa_mais_tempo_para_chegar_ao_local_de_trabalho",
  "race" = "cor_ou_raca",
  "educ" = "nivel_de_instrucao",
  "value" = "valor"
)

if (file.exists(cache_file)) {
  tab_modals <- readr::read_rds(cache_file)
} else {
  # Census mobility (table 10332) is pulled one region at a time at the
  # municipality level, mirroring 05_experimental.R.
  modals <- list()
  for (i in 1:5) {
    modals[[i]] <- get_sidra(
      10332,
      variable = 13377,
      classific = "c2088",
      geo = "City",
      geo.filter = list(Region = i)
    )
  }

  tab_modals <- bind_rows(modals) |>
    clean_names() |>
    as_tibble() |>
    select(all_of(cols_select)) |>
    mutate(code_muni = as.numeric(code_muni))

  readr::write_rds(tab_modals, cache_file)
}

# Municipality population (used as density weight)
pop_muni <- get_sidra(6579, period = "2025", geo = "City")

tab_pop <- pop_muni |>
  clean_names() |>
  as_tibble() |>
  select(code_muni = municipio_codigo, pop = valor) |>
  mutate(
    code_muni = as.numeric(code_muni),
    pop_class = cut(
      pop,
      breaks = c(0, 50e3, 100e3, 500e3, Inf),
      labels = c(
        "<b>Small cities</b><br>Less than 50k (n = 4.889)",
        "<b>Mid-sized</b><br>50–100k (n = 344)",
        "<b>Big cities</b><br>100–500k (n = 290)",
        "<b>Very big cities</b><br>Over 500k inhab. (n = 48)"
      ),
      right = FALSE
    )
  )

tab_pop |>
  count(pop_class)

# Mode classification (walking-only; cycling falls into Other) ------------

dim_mode <- tibble(
  mode = c(
    "A pé",
    "Automóvel",
    "BRT ou ônibus de trânsito rápido",
    "Bicicleta",
    "Caminhonete ou caminhão adaptado (pau de arara)",
    "Embarcação de médio e grande porte (acima de 20 pessoas)",
    "Embarcação de pequeno porte (até 20 pessoas)",
    "Motocicleta",
    "Mototáxi",
    "Outros",
    "Trem ou metrô",
    "Táxi ou assemelhados",
    "Van, perua ou assemelhados",
    "Ônibus"
  ),
  group = case_when(
    mode == "A pé" ~ "Walking",
    str_detect(mode, "Embarcação") ~ "Public transit",
    mode %in%
      c(
        "BRT ou ônibus de trânsito rápido",
        "Ônibus",
        "Trem ou metrô",
        "Van, perua ou assemelhados"
      ) ~ "Public transit",
    mode %in% c("Automóvel", "Táxi ou assemelhados") ~ "Car",
    mode %in% c("Motocicleta", "Mototáxi") ~ "Motorcycle",
    TRUE ~ "Other"
  )
)

# Shares ------------------------------------------------------------------

lvls_modes <- c("Walking", "Public transit", "Car", "Motorcycle")

share_modes <- tab_modals |>
  filter(race == "Total", educ == "Total", mode != "Total") |>
  left_join(dim_mode, by = "mode") |>
  summarise(trips = sum(value, na.rm = TRUE), .by = c("code_muni", "group")) |>
  mutate(share = trips / sum(trips) * 100, .by = "code_muni")

dat <- share_modes |>
  inner_join(tab_pop, by = "code_muni") |>
  filter(group %in% lvls_modes, !is.na(pop), pop > 0) |>
  mutate(
    mode = factor(group, levels = lvls_modes)
  )

dat |>
  count(pop_class)

# Theme (matches 01_part_to_whole.R) --------------------------------------

base_text <- "Lato"
title_text <- "Lora"
offwhite <- "#f5f5dc"

colors_modes <- c(
  "Walking" = "#8EAF91",
  "Public transit" = "#466C6F",
  "Car" = "#9B4538",
  "Motorcycle" = "#EABC6D"
)
MetBrewer::met.brewer("Hokusai1")[7]

label_percent_br <- function(x) {
  scales::number(
    x,
    accuracy = 1,
    scale = 1,
    suffix = "%",
    big.mark = ".",
    decimal.mark = ","
  )
}

theme_plot <- theme_minimal(base_family = base_text) +
  theme_sub_panel(
    grid.minor = element_blank(),
    grid.major.y = element_blank(),
    grid.major.x = element_line(color = "gray80", linewidth = 0.3),
    spacing.x = unit(8, "pt"),
    spacing.y = unit(6, "pt"),
    background = element_rect(fill = offwhite, color = offwhite)
  ) +
  theme_sub_plot(
    background = element_rect(fill = offwhite, color = offwhite),
    margin = margin(12, 14, 8, 12),
    title = element_text(family = title_text, size = 16, hjust = 0),
    subtitle = element_text(
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
  theme_sub_axis_y(text = element_blank(), title = element_blank()) +
  theme_sub_axis_x(line = element_line(color = "gray20", linewidth = 0.3)) +
  theme_sub_strip(
    text.x = element_textbox(
      family = base_text,
      hjust = 0,
      size = 8,
      color = "#ffffff"
    ),
    text.y.left = element_text(
      family = base_text,
      size = 8,
      angle = 90,
      color = "#ffffff"
    )
  ) +
  theme(
    legend.position = "none",
    panel.spacing.x = unit(14, "pt"),
    panel.spacing.y = unit(6, "pt")
  )

# Plot --------------------------------------------------------------------

panel <- ggplot(dat, aes(x = share, weight = pop, fill = mode, color = mode)) +
  geom_density(
    alpha = 0.85,
    bounds = c(0, 100),
    linewidth = 0.5
  ) +
  geom_hline(yintercept = 0, color = "gray10", linewidth = 0.3) +
  facet_grid(
    rows = vars(mode),
    cols = vars(pop_class),
    switch = "y",
    scales = "free_y"
  ) +
  scale_x_continuous(
    labels = label_percent_br,
    breaks = seq(0, 100, 25),
    expand = expansion(mult = 0.05)
  ) +
  scale_y_continuous(expand = expansion(c(0, 0.05))) +
  scale_fill_manual(values = colors_modes) +
  scale_color_manual(values = colorspace::darken(colors_modes, 0.3)) +
  labs(
    title = "Getting around, scale by scale",
    subtitle = "Distribution of municipal modal share by city size in Brazil (2022). Each curve is the population-weighted\ndensity of cities; rows are city-size classes, columns are the main commute mode.",
    x = NULL,
    y = NULL,
    caption = "Source: IBGE (Census, 2022) - main transport mode when commuting to work or school. @viniciusoike\nWalking counts trips on foot only (cycling excluded); 'Car' includes cabs; 'Motorcycle' includes mototaxis;\n'Public transit' includes buses, trains/metros, shared boats, and informal public transport (e.g. vans)."
  ) +
  theme_plot +
  theme(strip.background = element_rect(fill = "#224b5e"))

ggsave(
  here("2026/plots/07_multiscale.png"),
  panel,
  width = 8,
  height = 5,
  dpi = 400
)
