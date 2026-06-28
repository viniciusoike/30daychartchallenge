# Prompt: Comparisons — Experimental
# Modal share by municipality on a ternary plot (static PNG + interactive HTML).
# Source: IBGE Census 2022 (SIDRA 10332), population (SIDRA 6579).

library(dplyr)
library(ggplot2)
library(ggtern)
library(plotly)

import::from(forcats, fct_reorder)
import::from(tidyr, pivot_wider)
import::from(stringr, str_detect, str_sub, str_glue)
import::from(tibble, tibble, as_tibble)
import::from(scales, number)
import::from(MetBrewer, met.brewer)
import::from(htmlwidgets, onRender, saveWidget)
import::from(sidrar, get_sidra)
import::from(geobr, read_municipality)
import::from(sf, st_drop_geometry)
import::from(janitor, clean_names)
import::from(here, here)
import::from(readr, write_rds)

# Data --------------------------------------------------------------------

cities <- read_municipality(year = 2022, showProgress = FALSE)
dim_muni <- as_tibble(st_drop_geometry(cities))

pop_muni <- get_sidra(
  6579,
  period = "2025",
  geo = "City"
)

tab_pop <- pop_muni |>
  clean_names() |>
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
  clean_names() |>
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

write_rds(share_modes, "2026/data/census_modes/share_modes.rds")

data_tern <- main_cities |>
  pivot_wider(
    id_cols = c("code_muni", "name_region", "name_muni"),
    names_from = "type",
    values_from = "share"
  ) |>
  clean_names()

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

# Top car/motorcycle cities (exploratory, not run)
# main_cities |>
#   filter(type == "Car or motorcycle") |>
#   slice_max(share, n = 10)

offwhite <- "#f8fbf8"

# Plot (static) -----------------------------------------------------------

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
  geom_text(
    data = subset(data_tern, code_muni %in% sel_cities),
    aes(label = name_muni),
    size = 2,
    family = "Roboto Slab",
  ) +
  scale_fill_manual(values = met.brewer("Hokusai1", 5)) +
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
  here("2026", "plots", "05_experimental.png"),
  final_plot,
  width = 7,
  height = 7,
  dpi = 400
)

# Interactive (plotly) ----------------------------------------------------

## Palette & helpers ----------------------------------------------------

data_tern <- data_tern |>
  left_join(select(dim_muni, code_muni, abbrev_state), by = "code_muni") |>
  left_join(tab_pop, by = "code_muni") |>
  mutate(
    city_label = str_glue("{name_muni} ({abbrev_state})"),
    pop_trunc = number(pop, accuracy = 1e3, big.mark = ","),
  )

offwhite <- "#f8fbf8"

region_lvls <- levels(data_tern$name_region)
pal_raw <- met.brewer("Hokusai1", length(region_lvls))
region_pal <- setNames(as.character(pal_raw), region_lvls)

hex_to_rgba <- function(hex, alpha = 1) {
  v <- col2rgb(hex)
  sprintf("rgba(%d,%d,%d,%.2f)", v[1], v[2], v[3], alpha)
}

make_hover <- function(df) {
  transit <- sprintf("%.1f%%", df$public_transit)
  active <- sprintf("%.1f%%", df$active)
  car <- sprintf("%.1f%%", df$car_or_motorcycle)

  str_glue(
    "<span style='color:#000000;line-height:1.15'>
    <b>{df$city_label}</b>
    <br>{df$name_region}
    <br>\U1F465 Population   <b>{df$pop_trunc}</b>
    <br>\U1F68C Transit      <b>{transit}</b>
    <br>\U1F6B6 Active       <b>{active}</b>
    <br>\U1F697 Car / Moto   <b>{car}</b>
    </span>"
  )
}

## Build figure ---------------------------------------------------------

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
      size = 9,
      line = list(color = hex_to_rgba(col, 0.80), width = 0.8)
    ),
    text = make_hover(d),
    customdata = d$city_label,
    hoverinfo = "text"
  )
}

# Hidden highlight overlay: a single-point trace the hover JS repositions onto
# the hovered bubble. It carries hoverinfo = "skip" so it never fires hover
# events itself, which avoids the flicker loop caused by resizing data points.
fig <- add_trace(
  fig,
  type = "scatterternary",
  a = numeric(0),
  b = numeric(0),
  c = numeric(0),
  mode = "markers",
  name = "highlight",
  showlegend = FALSE,
  hoverinfo = "skip",
  marker = list(
    color = "rgba(0,0,0,0)",
    size = 16,
    line = list(color = "#1a1a1a", width = 3)
  )
)

## Layout ---------------------------------------------------------------

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
  hoverdistance = 1,
  hoverlabel = list(
    bgcolor = "white",
    font = list(family = "Roboto Slab, sans-serif", size = 15),
    bordercolor = "#cccccc",
    align = "left"
  )
)

## Hover effect & city search -------------------------------------------
# Flicker-safe highlight. The data-point markers are never resized (resizing the
# point under the cursor changes its hit-area and re-triggers hover -> flicker).
# Instead we (1) dim the other regions, and (2) move a dedicated overlay trace
# (the last trace, hoverinfo = 'skip') onto the target bubble. The same helpers
# drive both hover and a search box: typing/picking a city 'pins' its highlight
# so it persists, and hovering elsewhere reverts to the pinned city on mouse-out.

hover_js <- "
function(el) {
  var overlayIdx = el.data.length - 1;

  // -- shared highlight helpers ------------------------------------------------
  function dimOthers(curve) {
    if (el._hlCurve === curve) return;   // already dimmed for this region
    el._hlCurve = curve;
    var dimIdx = [];
    for (var i = 0; i < overlayIdx; i++) { if (i !== curve) dimIdx.push(i); }
    if (dimIdx.length) Plotly.restyle(el, {'marker.opacity': 0.1}, dimIdx);
    Plotly.restyle(el, {'marker.opacity': 1.0}, [curve]);
  }

  function restoreAll() {
    el._hlCurve = null;
    var all = [];
    for (var i = 0; i < overlayIdx; i++) all.push(i);
    Plotly.restyle(el, {'marker.opacity': 1.0}, all);
  }

  function moveOverlay(curve, pt) {
    var t = el.data[curve];
    var col = (t.marker.color || '').replace(/[\\d.]+\\)$/, '1)');
    Plotly.restyle(el, {
      a: [[t.a[pt]]], b: [[t.b[pt]]], c: [[t.c[pt]]],
      'marker.color': col
    }, [overlayIdx]);
  }

  function clearOverlay() {
    Plotly.restyle(el, {a: [[]], b: [[]], c: [[]]}, [overlayIdx]);
  }

  function highlight(curve, pt) {
    el._hlKey = curve + ':' + pt;
    dimOthers(curve);
    moveOverlay(curve, pt);
  }

  // Revert to the pinned city, or to the bright baseline if nothing is pinned.
  function revert() {
    if (el._pinned) {
      highlight(el._pinned.curve, el._pinned.pt);
    } else {
      el._hlKey = null;
      restoreAll();
      clearOverlay();
    }
  }

  // -- hover -------------------------------------------------------------------
  el.on('plotly_hover', function(eventdata) {
    clearTimeout(el._hlTimer);
    var p = eventdata.points[0];
    var key = p.curveNumber + ':' + p.pointNumber;
    if (el._hlKey === key) return;       // same point: nothing to do
    highlight(p.curveNumber, p.pointNumber);
  });

  el.on('plotly_unhover', function() {
    clearTimeout(el._hlTimer);
    el._hlTimer = setTimeout(revert, 60);
  });

  // -- search box --------------------------------------------------------------
  if (!el._searchBuilt) {
    el._searchBuilt = true;
    var listId = el.id + '-cities';

    // Unique city labels (customdata) across the region traces.
    var seen = {}, opts = '';
    for (var t = 0; t < overlayIdx; t++) {
      var cd = el.data[t].customdata || [];
      for (var k = 0; k < cd.length; k++) {
        if (!seen[cd[k]]) { seen[cd[k]] = true; }
      }
    }
    Object.keys(seen).sort().forEach(function(name) {
      opts += '<option value=\"' + name + '\"></option>';
    });

    var box = document.createElement('div');
    box.style.cssText = 'text-align:center;margin:6px 0 2px;font-family:\"Roboto Slab\",sans-serif;';
    box.innerHTML =
      '<input list=\"' + listId + '\" placeholder=\"Search a city…\" ' +
      'style=\"width:260px;padding:6px 10px;border:1px solid #cccccc;' +
      'border-radius:6px;background:#f8fbf8;font-family:inherit;font-size:13px;' +
      'color:#000000;\" />' +
      '<datalist id=\"' + listId + '\">' + opts + '</datalist>';
    el.parentNode.insertBefore(box, el);

    var input = box.querySelector('input');
    input.addEventListener('input', function() {
      var val = input.value.trim().toLowerCase();
      if (!val) { el._pinned = null; revert(); return; }
      for (var t = 0; t < overlayIdx; t++) {
        var cd = el.data[t].customdata || [];
        for (var j = 0; j < cd.length; j++) {
          if (String(cd[j]).toLowerCase() === val) {
            el._pinned = {curve: t, pt: j};
            highlight(t, j);
            return;
          }
        }
      }
    });
  }
}
"

fig <- onRender(fig, hover_js)

# Static plotly export goes to its own file so it never overwrites the
# ggtern static plot saved above (2026/plots/05_experimental.png). Wrapped
# in try() because save_image() needs kaleido/reticulate + a Python env; a
# missing one must not halt the script before the HTML below is written.
try(
  save_image(
    fig,
    "2026/plots/05_experimental_plotly.png",
    width = 1200,
    height = 800,
    scale = 2
  )
)

# Interactive version (hover highlight + city search live only in the HTML).
saveWidget(
  fig,
  "2026/plots/05_experimental.html",
  selfcontained = TRUE
)
