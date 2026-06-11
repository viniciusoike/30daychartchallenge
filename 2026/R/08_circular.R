# Day 08 — Prompt: Circular ------------------------------------------------
# Income inequality inside major Brazilian cities. Census tracts are coloured
# by the household-head income decile (computed within each city) and the map
# is clipped to a circle around the city centre. Buffer/circle machinery is
# adapted from 2025/R/03_circles.R; map theming from 2026/R/13_ecossystems.R.

library(tidyverse)
library(sf)
library(patchwork)
import::from(here, here)

sf::sf_use_s2(FALSE)
options(timeout = 600)

# Warm palette tuned to the cream background from 2025/R/03_circles.R.
bg <- "#f6eee3" # warm cream background (from 2025/R/03_circles.R)
water_col <- "#9fc1d1" # soft blue for water, reads well on the cream
green_col <- "#bcc8a0" # warm sage for parks and large green areas
na_col <- "#ddd6c9" # warm light grey for tracts with no/suppressed income

# Data --------------------------------------------------------------------
# IBGE Censo 2022 - Agregados por Setores, Rendimento do Responsável.
# 2026/data/ is gitignored, so download + unzip on first run.

url <- paste0(
  "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/",
  "Agregados_por_Setores_Censitarios_Rendimento_do_Responsavel/",
  "Agregados_por_setores_renda_responsavel_BR_20260508_csv.zip"
)
data_dir <- here("2026", "data", "censo_renda")
cache_dir <- file.path(data_dir, "cache")
zip_path <- file.path(data_dir, "renda_setores.zip")

if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
}
if (!dir.exists(cache_dir)) {
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
}

# Render-ready geometry is built once and cached in render_data.rds. Leave
# REBUILD_GEOM <- FALSE to iterate fast on colours/titles/theme (only the
# drawing re-runs); set it TRUE — or delete render_data.rds — after changing the
# cities, centres, radii, income variable, or simplification.
REBUILD_GEOM <- FALSE
render_path <- file.path(cache_dir, "render_data.rds")
need_build <- REBUILD_GEOM || !file.exists(render_path)

# The income CSV and the per-city downloads are only needed when (re)building,
# so a fast colour/title pass skips reading 30 MB of data it would not use.
if (need_build) {
  csv_path <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)
  if (length(csv_path) == 0) {
    download.file(url, zip_path, mode = "wb", quiet = TRUE)
    unzip(zip_path, exdir = data_dir)
    csv_path <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)
  }
  csv_path <- csv_path[[1]]

  # V06004 = average monthly nominal income of the responsible person (head).
  # The CSV is ';'-delimited but uses DOT decimals, and CD_SETOR is a 15-digit
  # code: read every column as character (so the code is not coerced to a float
  # in scientific notation) and parse income explicitly. "X" marks suppressed
  # values -> NA.
  income_raw <- read_delim(
    csv_path,
    delim = ";",
    na = "X",
    col_types = cols(.default = col_character()),
    show_col_types = FALSE
  )

  income_setores <- income_raw |>
    transmute(
      code_tract = CD_SETOR,
      code_muni = as.numeric(str_sub(CD_SETOR, 1, 7)),
      income = as.numeric(V06004)
    )
}

# Helpers (from 2025/R/03_circles.R) --------------------------------------

get_buffer <- function(point, dist = 6000) {
  center <- sf::st_as_sf(point, coords = c("lng", "lat"), crs = 4326)

  center |>
    sf::st_transform(crs = 29101) |>
    sf::st_buffer(dist = dist) |>
    sf::st_transform(crs = 4674)
}

# Disk cache: geobr tracts and OSM layers are slow to fetch, so store each
# city's result as an .rds keyed by municipality code. Delete a city's files
# in 2026/data/censo_renda/cache/ to force a refresh (e.g. after moving a
# centre or changing its radius, since the OSM layers are clipped to the ring).

cached <- function(name, code, fun) {
  f <- file.path(cache_dir, sprintf("%s_%s.rds", name, code))
  if (file.exists(f)) {
    return(readRDS(f))
  }
  obj <- fun()
  # Only cache real results: a NULL (failed/timed-out fetch) is left uncached
  # so it is retried on the next run rather than baked in.
  if (!is.null(obj)) {
    saveRDS(obj, f)
  }
  obj
}

# Overpass worker. Run in a separate process (via callr) so a slow or blocked
# request can be hard-killed on timeout — an in-process timeout cannot interrupt
# a blocking network call. Returns OSM polygons (CRS 4326) or NULL.

osm_worker <- function(bbox, features) {
  q <- osmdata::add_osm_features(
    osmdata::opq(bbox = bbox, timeout = 50),
    features = features
  )
  res <- osmdata::osmdata_sf(q)
  parts <- Filter(
    function(x) !is.null(x) && nrow(x) > 0,
    list(res$osm_polygons, res$osm_multipolygons)
  )
  if (length(parts) == 0) {
    return(NULL)
  }
  geoms <- do.call(c, lapply(parts, sf::st_geometry))
  sf::st_sf(geometry = geoms)
}

# Fetch OR-combined OSM features, clipped to the ring (NULL if none/timeout).
# The 180s ceiling never truncates a normal fetch but guarantees no hang.

osm_fetch <- function(ring, features) {
  bb <- sf::st_bbox(sf::st_transform(ring, 4326))
  bbox <- unname(bb[c("xmin", "ymin", "xmax", "ymax")])

  polys <- tryCatch(
    callr::r(
      osm_worker,
      args = list(bbox = bbox, features = features),
      timeout = 180
    ),
    error = function(e) NULL
  )
  if (is.null(polys) || nrow(polys) == 0) {
    return(NULL)
  }

  polys <- sf::st_transform(sf::st_make_valid(polys), 4674)
  out <- tryCatch(
    suppressWarnings(sf::st_intersection(polys, ring)),
    error = function(e) NULL
  )
  if (is.null(out) || nrow(out) == 0) {
    return(NULL)
  }
  tryCatch(sf::st_collection_extract(out, "POLYGON"), error = function(e) out)
}

# Water = rivers/lakes/bays; green = only *large* green areas (parks, reserves,
# forests, woods) — grass/garden/meadow are dropped as small-area noise that
# also makes the query in big cities huge.

get_water <- function(ring) {
  osm_fetch(
    ring,
    c(
      '"natural"="water"', # lakes, reservoirs, river polygons
      '"water"', # any water=* (river, reservoir, lagoon, ...)
      '"natural"="bay"', # bays mapped as an area
      '"natural"="strait"'
    )
  )
}

get_green <- function(ring) {
  osm_fetch(
    ring,
    c(
      '"leisure"="park"',
      '"leisure"="nature_reserve"',
      '"landuse"="forest"',
      '"natural"="wood"'
    )
  )
}

# Ocean / bay fill for coastal cities. OSM maps the open sea as a coastLINE, not
# a polygon, so it never returns as water. Instead take everything in the ring
# that is NOT land — land = the union of the state's municipalities (geobr). The
# state land mass is cached per UF. Only used for cities flagged `coastal`, so
# inland circles cannot pick up false blue from neighbouring states.

get_ocean <- function(ring, code) {
  uf <- as.integer(substr(sprintf("%07d", code), 1, 2))

  land <- cached("ufland", uf, function() {
    m <- tryCatch(
      geobr::read_municipality(
        code_muni = uf,
        year = 2022,
        simplified = TRUE,
        showProgress = FALSE
      ),
      error = function(e) NULL
    )
    if (is.null(m)) {
      return(NULL)
    }
    sf::st_make_valid(sf::st_union(sf::st_transform(m, 4674)))
  })
  if (is.null(land)) {
    return(NULL)
  }

  ocean <- tryCatch(
    suppressWarnings(sf::st_difference(ring, sf::st_sf(geometry = land))),
    error = function(e) NULL
  )
  if (is.null(ocean) || nrow(ocean) == 0) {
    return(NULL)
  }
  sf::st_sf(geometry = sf::st_geometry(ocean))
}

# Stack several polygon layers (dropping NULL/empty) into one sf.

combine_layers <- function(...) {
  xs <- Filter(\(x) !is.null(x) && nrow(x) > 0, list(...))
  if (length(xs) == 0) {
    return(NULL)
  }
  geoms <- do.call(c, lapply(xs, sf::st_geometry))
  sf::st_sf(geometry = geoms)
}

# City centres + per-city circle radius (metres) --------------------------

points_center <- tribble(
  ~code_muni , ~lat       , ~lng       , ~dist , ~coastal , ~name            ,
     3550308 , -23.561289 , -46.655672 , 11000 , FALSE    , "São Paulo"      ,
     3304557 , -22.905087 , -43.185802 ,  9500 , TRUE     , "Rio de Janeiro" ,
     3106200 , -19.925923 , -43.937128 ,  9500 , FALSE    , "Belo Horizonte" ,
     5300108 , -15.797508 , -47.875682 , 11000 , FALSE    , "Brasília"       ,
     2927408 , -12.955000 , -38.450000 ,  8500 , TRUE     , "Salvador"       ,
     2611606 ,  -8.066555 , -34.879667 ,  8500 , TRUE     , "Recife"
)

# Build one city: clip tracts to the circle, decile income within-city ----

build_city <- function(code) {
  row <- subset(points_center, code_muni == code)
  ring <- get_buffer(row, dist = row$dist)

  # geobr may return code_tract as numeric; the CSV code is character -> align.
  tracts <- cached("tracts", code, function() {
    geobr::read_census_tract(
      code,
      year = 2022,
      simplified = FALSE,
      showProgress = FALSE
    ) |>
      mutate(code_tract = as.character(code_tract))
  })

  city <- filter(income_setores, code_muni == code)

  # within-city deciles, computed before clipping so they reflect the city.
  # NA-income tracts (suppressed "X" or unmatched) are kept: ntile() returns NA
  # for them, and they are drawn in grey instead of dropped to the background.
  tract_income <- tracts |>
    left_join(city, by = "code_tract") |>
    mutate(decile = factor(ntile(income, 10), levels = 1:10))

  shp <- suppressWarnings(st_intersection(tract_income, ring))

  # Water and green base layers (clipped to the ring; cached per city). For
  # coastal cities, add the ocean/bay fill and merge it into the water layer.
  water_osm <- cached("water", code, function() get_water(ring))
  green <- cached("green", code, function() get_green(ring))
  ocean <- if (isTRUE(row$coastal)) {
    cached("ocean", code, function() get_ocean(ring, code))
  } else {
    NULL
  }
  water <- combine_layers(water_osm, ocean)

  list(shp = shp, ring = ring, water = water, green = green, name = row$name)
}

# Make geometry render-ready: dissolve tracts by decile (thousands of polygons
# -> ~11 shapes; identical look since borders are not drawn) and simplify the
# vertices (invisible at this zoom). This is what makes redraws fast.

simplify_layer <- function(x, keep = 0.12) {
  if (is.null(x) || nrow(x) == 0) {
    return(NULL)
  }
  out <- tryCatch(
    rmapshaper::ms_simplify(x, keep = keep, keep_shapes = TRUE),
    error = function(e) x
  )
  sf::st_make_valid(out)
}

union_one <- function(x) {
  if (is.null(x) || nrow(x) == 0) {
    return(NULL)
  }
  sf::st_sf(geometry = sf::st_union(x))
}

prepare_city <- function(code) {
  city <- build_city(code)

  tracts <- city$shp |>
    dplyr::group_by(decile) |>
    dplyr::summarise(.groups = "drop") |>
    simplify_layer()

  list(
    tracts = tracts,
    water = simplify_layer(union_one(city$water)),
    green = simplify_layer(union_one(city$green)),
    ring = city$ring,
    name = city$name
  )
}

# Per-city plot -----------------------------------------------------------

theme_circ <- ggthemes::theme_map() +
  theme_sub_plot(
    title = element_text(size = 13, family = "Georgia", hjust = 0.5),
    background = element_rect(fill = bg, color = bg),
    margin = margin(6, 6, 6, 6)
  ) +
  theme_sub_panel(
    background = element_rect(fill = bg, color = bg)
  ) +
  theme_sub_legend(
    position = "bottom",
    title = element_text(size = 10, family = "Lora"),
    text = element_text(size = 8, family = "Lato"),
    title.position = "top",
    key.height = unit(0.35, "cm")
  ) +
  # Match the legend backgrounds to the off-white plot background.
  theme(
    legend.background = element_rect(fill = bg, color = bg),
    legend.box.background = element_rect(fill = bg, color = bg),
    legend.key = element_rect(fill = bg, color = bg)
  )

# Income palette. PuOr (diverging, orange = poorest, purple = richest) keeps
# blue + green free for the water and green map layers. Swap palette_scale for
# one of the alternatives below to change the look.
palette_scale <- scale_fill_brewer(
  name = "Income decile  (1 = lowest · 10 = highest)",
  palette = "PuOr",
  direction = 1,
  drop = FALSE,
  guide = guide_legend(nrow = 1, label.position = "bottom")
)

# Alternatives (also reserve blue + green for the map):
# magma  -> scale_fill_viridis_d(option = "magma",  begin = .12, end = .95, direction = -1, ...)
# plasma -> scale_fill_viridis_d(option = "plasma", begin = .05, end = .95, direction =  1, ...)

plot_city <- function(city) {
  # Tracts first: grey (no income data) below, then the coloured deciles.
  na_tracts <- dplyr::filter(city$tracts, is.na(decile))
  data_tracts <- dplyr::filter(city$tracts, !is.na(decile))

  p <- ggplot()
  if (nrow(na_tracts) > 0) {
    p <- p + geom_sf(data = na_tracts, fill = na_col, color = NA)
  }
  p <- p + geom_sf(data = data_tracts, aes(fill = decile), color = NA)

  # Green and water go ON TOP of the tracts: a park or river is its own kind of
  # place, so it masks the (usually no-data) tract beneath rather than hiding
  # under it. Water last, so rivers read over the green they cut through.
  if (!is.null(city$green) && nrow(city$green) > 0) {
    p <- p + geom_sf(data = city$green, fill = green_col, color = NA)
  }
  if (!is.null(city$water) && nrow(city$water) > 0) {
    p <- p + geom_sf(data = city$water, fill = water_col, color = NA)
  }

  p +
    geom_sf(data = city$ring, fill = NA, color = "gray60", linewidth = 0.3) +
    palette_scale +
    labs(title = city$name) +
    theme_circ
}

# Assemble ----------------------------------------------------------------

if (need_build) {
  render_data <- lapply(points_center$code_muni, prepare_city)
  saveRDS(render_data, render_path)
} else {
  render_data <- readRDS(render_path)
}

plots <- lapply(render_data, plot_city)

final <- wrap_plots(plots, ncol = 3) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Circles of inequality",
    subtitle = paste0(
      "Average monthly household-head income by census tract, split into ten ",
      "deciles within each city and clipped to a\ncircle around the centre. ",
      "Deciles are relative to each city, so colours are not comparable across cities. ",
      "Grey = no income data; blue = water; green = parks and forests."
    ),
    caption = "Source: IBGE (Census, 2022) — household-head income • @viniciusoike",
    theme = theme(
      plot.background = element_rect(fill = bg, color = bg),
      plot.title = element_text(family = "Georgia", size = 22, hjust = 0.5),
      plot.subtitle = element_text(
        family = "Lora",
        size = 9.5,
        hjust = 0.5,
        color = "gray30"
      ),
      plot.caption = element_text(
        family = "Lato",
        size = 8,
        color = "gray50",
        hjust = 0.5
      )
    )
  ) &
  theme(
    legend.position = "bottom",
    legend.background = element_rect(fill = bg, color = bg),
    legend.box.background = element_rect(fill = bg, color = bg),
    legend.key = element_rect(fill = bg, color = bg)
  )

ggsave(
  here("2026", "plots", "08_circular.png"),
  final,
  width = 8,
  height = 8,
  dpi = 400
)
