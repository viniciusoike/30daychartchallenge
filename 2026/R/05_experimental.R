library(dplyr)
library(ggplot2)
library(ggtern)

import::from(tidyr, pivot_wider)
import::from(here, here)
import::from(sidrar, get_sidra)
import::from(geobr, read_municipality)
import::from(sf, st_drop_geometry)
import::from(janitor, clean_names)

cities <- geobr::read_municipality(year = 2022, showProgress = FALSE)
dim_muni <- as_tibble(sf::st_drop_geometry(cities))

pop_muni <- get_sidra(
  6579,
  period = "2025",
  geo = "City"
)

tab_pop <- pop_muni |>
  janitor::clean_names() |>
  as_tibble() |>
  select(code_muni = municipio_codigo, pop = valor) |>
  mutate(code_muni = as.numeric(code_muni))

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

tab_modals <- bind_rows(modals)

tab_modals <- tab_modals |>
  janitor::clean_names() |>
  as_tibble()

cols_select <- c(
  "code_muni" = "municipio_codigo",
  "mode" = "meio_de_transporte_em_que_passa_mais_tempo_para_chegar_ao_local_de_trabalho",
  "race" = "cor_ou_raca",
  "educ" = "nivel_de_instrucao",
  "value" = "valor"
)

tab_modals <- tab_modals |>
  select(all_of(cols_select)) |>
  mutate(
    code_muni = as.numeric(code_muni)
  )

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
  type = case_when(
    mode %in% c("A pé", "Bicicleta") ~ "Active",
    str_detect(mode, "Embarcação") ~ "Public transit",
    mode %in%
      c(
        "BRT ou ônibus de trânsito rápido",
        "Ônibus",
        "Trem ou metrô",
        "Van, perua ou assemelhados"
      ) ~ "Public transit",
    mode %in%
      c(
        "Automóvel",
        "Motocicleta",
        "Mototáxi",
        "Táxi ou assemelhados"
      ) ~ "Car or motorcycle",
    TRUE ~ NA_character_
  ),
  group = case_when(
    mode %in% c("A pé", "Bicicleta") ~ "Active",
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

tab_modes <- tab_modals |>
  filter(race == "Total", educ == "Total", mode != "Total") |>
  left_join(dim_mode, by = "mode")

share_modes <- tab_modes |>
  filter(!is.na(type)) |>
  summarise(
    total_trips = sum(value, na.rm = TRUE),
    .by = c("code_muni", "type")
  ) |>
  mutate(share = total_trips / sum(total_trips) * 100, .by = "code_muni")

share_modes <- left_join(share_modes, tab_pop, by = join_by(code_muni))
share_modes <- left_join(share_modes, dim_muni, by = join_by(code_muni))

share_modes <- share_modes |>
  mutate(
    code_region = as.numeric(str_sub(code_muni, 1, 1)),
    name_region = case_when(
      code_region == 1 ~ "North",
      code_region == 2 ~ "Northeast",
      code_region == 3 ~ "Southeast",
      code_region == 4 ~ "South",
      code_region == 5 ~ "Mid-West"
    ),
    name_region = fct_reorder(name_region, -code_region),
  )

main_cities <- subset(share_modes, pop > 1e5)

readr::write_rds(share_modes, "2026/data/census_modes/share_modes.rds")

data_tern <- main_cities |>
  pivot_wider(
    id_cols = c("code_muni", "name_region", "name_muni"),
    names_from = "type",
    values_from = "share"
  ) |>
  janitor::clean_names()

sel_cities <- c(
  4314902,
  3550308,
  4202008,
  1501709,
  3300456,
  3516309,
  1100122,
  2408003,
  5103403
)

main_cities |>
  filter(type == "Car or motorcycle") |>
  slice_max(share, n = 10)

offwhite <- "#f8fbf8"

final_plot <- ggtern(
  data_tern,
  aes(x = public_transit, y = active, z = car_or_motorcycle)
) +
  geom_point(
    aes(fill = name_region),
    size = 1.5,
    alpha = 0.7,
    shape = 21
  ) +
  # geom_point(
  #   data = subset(data_tern, code_muni %in% sel_cities),
  #   aes(fill = name_region),
  #   size = 3.5,
  #   shape = 21
  # ) +
  geom_text(
    data = subset(data_tern, code_muni %in% sel_cities),
    aes(label = name_muni),
    size = 2,
    family = "Roboto Slab",
  ) +
  scale_fill_manual(values = MetBrewer::met.brewer("Hokusai1", 5)) +
  labs(
    x = "Transit",
    y = "Active",
    z = "Car/Moto",
    title = "Modal share by city",
    subtitle = "Main transport mode when commuting to work or school by city in Brazil (2022).",
    fill = NULL
  ) +
  theme_bw(base_family = "Roboto Slab") +
  theme_showarrows() +
  theme_sub_legend(
    position = "bottom",
    background = element_rect(fill = offwhite, color = offwhite)
  ) +
  theme_sub_plot(
    margin = margin(15, 10, 15, 10),
    title = element_text(size = 16, family = "Georgia"),
    subtitle = element_text(size = 10, color = "gray40"),
    caption = element_text(size = 8, color = "gray60"),
    background = element_rect(fill = offwhite, color = offwhite)
  ) +
  theme_sub_panel(
    background = element_rect(fill = offwhite, color = offwhite),
    grid.minor = element_blank()
  )

ggsave(
  here::here("2026", "plots", "05_experimental.png"),
  final_plot,
  width = 7,
  height = 7,
  dpi = 400
)

library(plotly)
library(htmlwidgets)

# ---- Palette & helpers -------------------------------------------------------

offwhite <- "#f8fbf8"

region_lvls <- levels(data_tern$name_region)
pal_raw <- MetBrewer::met.brewer("Hokusai1", length(region_lvls))
region_pal <- setNames(as.character(pal_raw), region_lvls)

hex_to_rgba <- function(hex, alpha = 1) {
  v <- col2rgb(hex)
  sprintf("rgba(%d,%d,%d,%.2f)", v[1], v[2], v[3], alpha)
}

make_hover <- function(df) {
  paste0(
    "<b>",
    df$name_muni,
    "</b>",
    "<br><span style='color:#999999'>",
    df$name_region,
    "</span>",
    "<br>",
    "<br>\U1F68C Transit    <b>",
    sprintf("%.1f%%", df$public_transit),
    "</b>",
    "<br>\U1F6B6 Active     <b>",
    sprintf("%.1f%%", df$active),
    "</b>",
    "<br>\U1F697 Car / Moto <b>",
    sprintf("%.1f%%", df$car_or_motorcycle),
    "</b>"
  )
}

# ---- Build figure -----------------------------------------------------------

fig <- plot_ly()

for (reg in region_lvls) {
  d <- filter(data_tern, name_region == reg)
  col <- region_pal[[reg]]

  fig <- add_trace(
    fig,
    type = "scatterternary",
    a = d$active,
    b = d$public_transit,
    c = d$car_or_motorcycle,
    mode = "markers",
    name = reg,
    legendgroup = reg,
    showlegend = TRUE,
    marker = list(
      color = hex_to_rgba(col, 0.50),
      size = 10,
      line = list(color = hex_to_rgba(col, 0.80), width = 0.8)
    ),
    text = make_hover(d),
    hoverinfo = "text"
  )
}

# ---- Layout -----------------------------------------------------------------

axis_style <- function(title_text) {
  list(
    title = list(
      text = title_text,
      font = list(
        family = "Roboto Slab, sans-serif",
        size = 13,
        color = "#444444"
      )
    ),
    tickfont = list(
      family = "Roboto Slab, sans-serif",
      size = 10,
      color = "#666666"
    ),
    gridcolor = "#d4d4d4",
    gridwidth = 0.8,
    linecolor = "#aaaaaa",
    tickformat = ".0f",
    ticksuffix = "%",
    min = 0,
    showgrid = TRUE
  )
}

fig <- layout(
  fig,
  title = list(
    text = "<b>Modal share by municipality</b>",
    font = list(family = "Georgia, serif", size = 18, color = "#2d2d2d"),
    x = 0.5,
    xanchor = "center",
    y = 0.97
  ),
  ternary = list(
    bgcolor = offwhite,
    aaxis = axis_style("Active"),
    baxis = axis_style("Transit"),
    caxis = axis_style("Car / Moto"),
    sum = 100
  ),
  paper_bgcolor = offwhite,
  plot_bgcolor = offwhite,
  legend = list(
    title = list(text = ""),
    orientation = "h",
    x = 0.5,
    xanchor = "center",
    y = -0.02,
    font = list(
      family = "Roboto Slab, sans-serif",
      size = 11,
      color = "#333333"
    ),
    bgcolor = offwhite,
    bordercolor = offwhite,
    itemsizing = "constant"
  ),
  margin = list(l = 60, r = 60, t = 80, b = 90),
  hoverlabel = list(
    bgcolor = "white",
    font = list(family = "Roboto Slab, sans-serif", size = 12),
    bordercolor = "#cccccc",
    align = "left"
  )
)

# ---- Hover effect -----------------------------------------------------------
# On hover: dim all other traces to draw attention to the hovered group.
# On unhover: restore full opacity for all traces.

hover_js <- "
function(el) {
  el.on('plotly_hover', function(eventdata) {
    var hovered = eventdata.points[0].curveNumber;
    var n = el.data.length;
    var dimIdx = [], brightIdx = [];
    for (var i = 0; i < n; i++) {
      if (i === hovered) brightIdx.push(i); else dimIdx.push(i);
    }
    if (dimIdx.length)   Plotly.restyle(el, {'marker.opacity': 0.08}, dimIdx);
    if (brightIdx.length) Plotly.restyle(el, {'marker.opacity': 1.0},  brightIdx);
  });

  el.on('plotly_unhover', function() {
    var n = el.data.length;
    var all = Array.from({length: n}, function(_, i) { return i; });
    Plotly.restyle(el, {'marker.opacity': 1.0}, all);
  });
}
"

fig <- onRender(fig, hover_js)

save_image(
  fig,
  "2026/plots/05_experimental.png",
  width = 1200,
  height = 800,
  scale = 2
)
