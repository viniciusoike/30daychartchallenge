# pop <- sidrar::get_sidra(4714, variable = c(93, 614), geo = "City")
#
# sidrapop <- pop |>
#   janitor::clean_names() |>
#   as_tibble()
#
# tbl_pop <- sidrapop |>
#   select(code_muni = municipio_codigo, variable = variavel, value = valor) |>
#   mutate(
#     variable = ifelse(variable == "População residente", "population", "pop_dens")
#   ) |>
#   pivot_wider(
#     id_cols = "code_muni",
#     names_from = "variable",
#     values_from = "value"
#   )
#
# top_pop <- tbl_pop |>
#   slice_max(population, n = 25)
#
# top_pop_dens <- tbl_pop |>
#   slice_max(pop_dens, n = 25)
#
# readr::write_csv(top_pop, here("data/day_3/top_cities_population.csv"))
# readr::write_csv(top_pop_dens, here("data/day_3/top_cities_population_density.csv"))


get_border = function(code) {
  #> Importa o shape do município
  border <- geobr::read_municipality(code, showProgress = FALSE)
  return(border)
}

get_state = function(code) {
  dim_muni |>
    dplyr::filter(code_muni == code) |>
    dplyr::pull(name_state)
}

get_streets <- function(code, border) {

  city_name <- subset(dim_muni, code_muni == code)[["name_muni"]]
  #> Encontra o nome da Unidade Federativa
  nome_uf <- get_state(code)
  #> Monta o nome do local
  name_place <- stringr::str_glue("{city_name}, {nome_uf}, Brazil")
  #> Monta a query
  place <- osmdata::opq(bbox = osmdata::getbb(name_place))

  #> Importa todas as principais vias da cidade
  streets <- osmdata::add_osm_feature(
    place,
    key = "highway",
    value = c("motorway", "trunk", "primary", "secondary", "tertiary", "residential")
  )

  #> Converte o dado

  streets <- osmdata::osmdata_sf(streets)
  streets <- osmdata::unique_osmdata(streets)
  streets <- streets[["osm_lines"]]
  streets <- dplyr::select(streets, osm_id, name, highway)
  streets <- sf::st_transform(streets, crs = 4674)

  #> Enconrtra a intersecção entre as estradas e o limites do município
  streets_border <- sf::st_intersection(streets, border)
  #> Retorna o objeto streets_border
  return(streets_border)

}

get_osm <- function(place, ls) {

  qr_osm_ft <- osmdata::add_osm_features(opq = place, features = ls)
  osm_ft <- osmdata::osmdata_sf(qr_osm_ft)

  osm_ft <- osmdata::unique_osmdata(osm_ft)

  osm_mpoly <- osm_ft[["osm_multipolygons"]]
  osm_poly <- osm_ft[["osm_polygons"]]

  if (!is.null(osm_poly)) {
    osm_poly <- sf::st_cast(osm_poly, to = "MULTIPOLYGON")
    osm_poly <- sf::st_make_valid(osm_poly)
    osm_ft <- dplyr::bind_rows(osm_mpoly, osm_poly)
  } else {
    osm_ft <- osm_mpoly
  }

  if (is.null(osm_ft)) {
    return(NULL)
  }

  osm_ft <- dplyr::select(osm_ft, osm_id, name)
  osm_ft <- sf::st_transform(osm_ft, crs = 4674)

  return(osm_ft)

}

get_osm_aesthetics <- function(code, buffer) {

  city_name <- subset(dim_muni, code_muni == code)[["name_muni"]]
  #> Encontra o nome da Unidade Federativa
  nome_uf <- get_state(code)
  #> Monta o nome do local
  name_place <- stringr::str_glue("{city_name}, {nome_uf}, Brazil")
  #> Monta a query
  bb <- osmdata::getbb(name_place)
  place <- osmdata::opq(bbox = bb)

  list_green <- list(
    landuse = "grass",
    natural = c("island", "wood"),
    leisure = "park"
  )

  list_parking <- list(
    amenity = "parking",
    highway = "pedestrian",
    man_made = "pier"
  )

  list_water <- list(
    natural = c("water", "bay", "coastline")
  )

  osm_water <- get_osm(place, list_water)
  osm_green <- get_osm(place, list_green)
  osm_parking <- get_osm(place, list_parking)

  features <- list(water = osm_water, green = osm_green, parking = osm_parking)

  features <- parallel::mclapply(features, \(x) sf::st_intersection(buffer, sf::st_make_valid(x)))

  return(features)

}


add_jenks_breaks = function(shp, k = 7, variable = NULL) {
  #> Classifica os dados de population em k grupos segundo o algo. de Jenks
  jbreaks = BAMMtools::getJenksBreaks(shp[[variable]], k = k)

  out <- shp |>
    dplyr::mutate(
      jenks_group = factor(findInterval(.data[[variable]], jbreaks))
    )

  return(out)

}

get_population_grid <- function(code, l = 100) {

  tracts <- geobr::read_census_tract(code, year = 2022, simplified = FALSE)

  pop_city <- dplyr::filter(pop_setores, code_muni == code)

  pop_tract <- dplyr::left_join(tracts, pop_city, by = "code_tract")

  grid <- pop_tract |>
    sf::st_transform(crs = 29101) |>
    sf::st_make_grid(cellsize = c(l, l)) |>
    sf::st_as_sf() |>
    sf::st_transform(crs = 4674) |>
    dplyr::mutate(gid = dplyr::row_number())

  grid_pop <- pop_tract |>
    dplyr::select(pop) |>
    # Purely for aesthetic reasons
    dplyr::mutate(
      pop = ifelse(is.na(pop), 0, pop),
      pop = sqrt(pop)
      ) |>
    sf::st_interpolate_aw(grid, extensive = TRUE, na.rm = TRUE)

  return(grid_pop)

}

get_buffer <- function(point = NULL, coords = NULL, dist = 6000) {

  center <- sf::st_as_sf(point, coords = c("lng", "lat"), crs = 4326)

  buffer_zone <- center |>
    sf::st_transform(crs = 29101) |>
    sf::st_buffer(dist = dist) |>
    sf::st_transform(crs = 4674)

}

get_streets_population = function(population, streets) {

  stopifnot(any(colnames(population) %in% "jenks_group"))

  #> Encontra todos os grupos
  groups = levels(population$jenks_group)

  #> Define uma função auxiliar

  #> Esta função filtra o grid de population e faz a sua interseção
  #> com o shape das princiapis vias
  join_streets = function(group) {

    poly = population %>%
      dplyr::filter(jenks_group == group) %>%
      sf::st_union(.) %>%
      sf::st_as_sf() %>%
      sf::st_make_valid()

    joined = suppressWarnings(sf::st_intersection(streets, poly))

    return(joined)

  }
  #> Aplica a função acima em todos os grupos em paralelo
  street_levels = parallel::mclapply(groups, join_streets)
  #> "Empilha" o objeto num único spatial data.frame
  out = dplyr::bind_rows(street_levels, .id = "level")

  return(out)

}

interpolate_buffer_streets <- function(buffer, code) {

  pop_city <- filter(pop_setores, code_muni == code)
  tracts <- geobr::read_census_tract(code, year = 2022, simplified = FALSE, showProgress = FALSE)
  pop_tract <- left_join(tracts, pop_city, by = "code_tract")
  pop_buffer <- st_interpolate_aw(select(pop_tract, pop), buffer, extensive = TRUE, na.rm = TRUE)

  return(pop_buffer)

}

get_subtitle <- function(buffer, code) {

  pop_buffer <- interpolate_buffer_streets(buffer, code)

  p0 <- subset(population, code_muni == code)[["population"]]
  p1 <- pop_buffer[["pop"]]
  s <- p1 / p0 * 100

  sub_pop_city <- format(round(p0), big.mark = ".")
  sub_pop_buffer <- format(round(p1), big.mark = ".")
  share_buffer <- round(s, 1)

  subtitle <- stringr::str_glue("Total pop.: {sub_pop_city}\nPop. inside circle: {sub_pop_buffer} ({share_buffer}%)")

  return(subtitle)
}

plot_map <- function(shp, features, buffer, streets, title, subtitle, font = "Futura") {

  cores <- viridis::inferno(n = length(unique(shp$level)) + 1)
  cores <- head(cores, length(cores) - 1)

  blues <- MetBrewer::met.brewer("Hokusai2", n = 9)
  greens <- MetBrewer::met.brewer("VanGogh3", n = 11)

  all_streets <- st_intersection(streets, buffer)

  font <- "Futura"
  offwhite <- "#F4F0E0"

  coords_buffer <- st_bbox(buffer)

  df_segment <- data.frame(
    xmin = coords_buffer["xmin"],
    xmax = coords_buffer["xmax"],
    ymin = coords_buffer["ymin"]
  )

  df_segment$midpoint <- df_segment$xmin + (df_segment$xmax - df_segment$xmin) / 2

  p <- ggplot() +
    # All streets
    geom_sf(data = all_streets, color = "gray25", linewidth = 0.2) +
    # Green
    geom_sf(data = features$green, fill = greens[6], alpha = 0.8, color = offwhite) +
    # Water
    geom_sf(data = features$water, fill = blues[7], color = offwhite) +
    # Parkings
    geom_sf(data = features$parking, fill = "#2F3737", color = offwhite) +
    # Highway
    geom_sf(
      data = dplyr::filter(shp, highway == "motorway"),
      aes(color = level, fill = level), linewidth = 0.5) +
    # Trunk
    geom_sf(
      data = dplyr::filter(shp, highway == "trunk"),
      aes(color = level, fill = level), linewidth = 0.5) +
    # Primary roads
    geom_sf(
      data = dplyr::filter(shp, highway == "primary"),
      aes(color = level, fill = level), linewidth = 0.35) +
    # Secondary roads
    geom_sf(
      data = dplyr::filter(shp, highway == "secondary"),
      aes(color = level, fill = level), linewidth = 0.3) +
    # Tertiary roads
    geom_sf(
      data = dplyr::filter(shp, highway == "tertiary"),
      aes(color = level, fill = level), linewidth = 0.25) +
    # Residential roads
    geom_sf(
      data = dplyr::filter(shp, highway == "residential"),
      aes(color = level, fill = level), linewidth = 0.25) +
    # Circular outiline
    geom_sf(data = buffer, fill = NA, color = "gray10", lwd = 1) +
    # Double arrow at the bottom
    geom_segment(
      data = df_segment,
      aes(x = xmin, xend = xmax, y = ymin - 0.005, yend = ymin - 0.005),
      arrow = grid::arrow(ends = "both", length = unit(5, "pt"))
    ) +
    # Text label
    geom_text(
      data = df_segment,
      aes(x = midpoint, y = ymin - 0.01, label = "12 km"),
      family = "Futura"
    ) +
    # Scale colors
    scale_color_manual(values = cores) +
    scale_fill_manual(values = cores) +
    guides(fill = "none", color = "none") +
    # Titles
    labs(title = title, subtitle = subtitle) +
    # Theme
    ggthemes::theme_map() +
    theme(
      plot.title = element_text(
        size = 22,
        hjust = 0.5,
        family = font
      ),
      plot.subtitle = element_text(
        size = 12,
        hjust = 0.5,
        family = font,
        color = "gray30"
      ),
      plot.background = element_rect(color = NA, fill = offwhite),
      panel.background = element_rect(color = NA, fill = offwhite),
      legend.background = element_rect(color = NA, fill = offwhite)
    )

  return(p)

}

points_center <- tibble::tribble(
  ~code_muni, ~lat, ~lng,
  3550308, -23.561289, -46.655672,
  4106902, -25.437640, -49.269854,
  3304557, -22.905087, -43.185802,
  5300108, -15.797507776165935, -47.875681717924245,
  2304400, -3.7274277550472577, -38.52898363130275,
  2927408, -12.978167982377807, -38.512008190685386,
  3106200, -19.925922770242043, -43.9371279250982,
  1302603, -3.1299468807596593, -60.020726028748896,
  2611606, -8.066554950939688, -34.87966655765099,
  5208707, -16.666485683914015, -49.25212964879797,
  4314902, -30.03656382031383, -51.21603362841124
)

# map_population <- function(code, l = 100, k = 9) {
#
#   # Get streets
#   border <- get_border(code)
#   streets <- get_streets(code, border)
#
#   # Population grid
#   pop_grid <- get_population_grid(code, l = l)
#   pop_class <- add_jenks_breaks(pop_grid, k = k, variable = "pop")
#
#   # Intersect grid with streets
#   streets_pop <- get_streets_population(pop_class, streets)
#
#   # Define buffer
#   pt <- data.frame(lng = -49.269854, lat = -25.437640)
#   pt <- subset(points_center, code_muni == code)
#
#   buffer_zone <- get_buffer(pt)
#   buffer_pop <- st_intersection(buffer_zone, streets_pop)
#   subtitle <- get_subtitle(buffer_zone, code)
#   title <- subset(dim_muni, code_muni == code)$name_muni
#   plot_pop <- plot_map(buffer_pop, title, subtitle)
#
# }

map_population <- function(code, l = 100, k = 9) {
  # Suppress all warnings
  oldw <- getOption("warn")
  options(warn = -1)
  on.exit(options(warn = oldw))

  # Create progress messages
  message("Starting population mapping process...")

  # Get streets
  message("Step 1/8: Retrieving street network data...")
  border <- get_border(code)
  streets <- get_streets(code, border)
  message("✓ Street network data retrieved successfully")

  # Population grid
  message("Step 2/8: Generating population grid...")
  pop_grid <- get_population_grid(code, l = l)
  pop_class <- add_jenks_breaks(pop_grid, k = k, variable = "pop")
  message("✓ Population grid generated successfully")

  # Intersect grid with streets
  message("Step 3/8: Intersecting grid with streets...")
  streets_pop <- get_streets_population(pop_class, streets)
  message("✓ Grid-street intersection completed")

  # Define buffer
  message("Step 4/8: Setting up buffer zone...")
  pt <- subset(points_center, code_muni == code)
  message("✓ Center point identified")

  message("Step 5/8: Creating buffer zone...")
  buffer_zone <- get_buffer(pt)
  message("✓ Buffer zone created")

  message("Step 6/8: Intersecting buffer with streets...")
  buffer_pop <- st_intersection(buffer_zone, streets_pop)
  message("✓ Buffer-streets intersection completed")

  message("Step 7/8: Retrieving OSM features data...")
  osm_features <- get_osm_aesthetics(code, buffer_zone)
  message("✓ OSM features data retrieved successfully")

  # Final plotting
  message("Step 8/8: Generating final map...")
  subtitle <- get_subtitle(buffer_zone, code)
  title <- subset(dim_muni, code_muni == code)$name_muni

  plot_pop <- plot_map(
    buffer_pop,
    features = osm_features,
    buffer = buffer_zone,
    streets = streets,
    title = title,
    subtitle = subtitle,
    font = "Futura"
    )

  message("✓ Map generated successfully")

  message("Process completed successfully!")

  return(plot_pop)
}

# test --------------------------------------------------------------------

library(sf)
library(dplyr)
library(ggplot2)
library(ragg)

import::from(here, here)
sf::sf_use_s2(FALSE)

dim_muni <- geobr::read_municipality(year = 2022)
dim_muni <- as_tibble(st_drop_geometry(dim_muni))

path <- '/Volumes/T7 Touch/github/tidyibge/data-raw/censo_2022/Agregados_por_setores_demografia_BR.csv'

pop_setores <- readr::read_csv2(path, na = "X")

pop_setores <- pop_setores |>
  select(code_tract = CD_setor, pop = V01006) |>
  mutate(code_muni = as.numeric(substr(code_tract, 1, 7)))

population <- readr::read_csv(here("data/day_3/top_cities_population.csv"))

print_plot <- function(code) {
  name_city <- subset(dim_muni, code_muni == code)$name_muni
  name_file <- here(glue::glue("plots/3_circles_{name_city}.png"))
  if (file.exists(name_file)) {
    return(NULL)
  }
  plot_map <- map_population(code, l = 100)
  readr::write_rds(plot_map, here(glue::glue("plots/map_{name_city}.rds")))
  ggsave(name_file, plot_map, width = 6, height = 5)
  return(plot_map)
}

# cities <- c(3550308, 4314902, 4106902, 3304557, 5300108)

purrr::safely(parallel::mclapply(points_center$code_muni, print_plot))

# code = 5300108
# border = get_border(code)
# streets = get_streets(code, border)
#
#
# city_name <- subset(dim_muni, code_muni == code)[["name_muni"]]
# #> Encontra o nome da Unidade Federativa
# nome_uf <- get_state(code)
# #> Monta o nome do local
# name_place <- stringr::str_glue("{city_name}, {nome_uf}, Brazil")
# #> Monta a query
# bb <- osmdata::getbb(name_place)
# place <- osmdata::opq(bbox = bb)
#
# # qr_osm_green <- add_osm_features(place, features = list_green)
# # osm_green <- osmdata_sf(qr_osm_green)
# #
# # osm_green <- unique_osmdata(osm_green)
# #
# # osm_poly <- st_cast(osm_green$osm_polygons, to = "MULTIPOLYGON")
# # osm_poly <- st_make_valid(osm_poly)
# #
# # osm_green <- bind_rows(osm_green$osm_multipolygons, osm_poly)
# #
# # osm_green <- osm_green[["osm_multipolygons"]]
# # osm_green <- dplyr::select(osm_green, osm_id, name)
# # osm_green <- sf::st_transform(osm_green, crs = 4674)
#
#
#
# # Population grid
# message("Step 2/7: Generating population grid...")
# pop_grid <- get_population_grid(code, l = 250)
# pop_class <- add_jenks_breaks(pop_grid, k = 11, variable = "pop")
# message("✓ Population grid generated successfully")
#
# # Intersect grid with streets
# message("Step 3/7: Intersecting grid with streets...")
# streets_pop <- get_streets_population(pop_class, streets)
# message("✓ Grid-street intersection completed")
#
# # Define buffer
# message("Step 4/7: Setting up buffer zone...")
# pt <- subset(points_center, code_muni == code)
# message("✓ Center point identified")
#
# message("Step 5/7: Creating buffer zone...")
# buffer_zone <- get_buffer(pt)
# message("✓ Buffer zone created")
#
# message("Step 6/7: Calculating buffer population...")
# buffer_pop <- st_intersection(buffer_zone, streets_pop)
# subtitle <- get_subtitle(buffer_zone, code)
# message("✓ Buffer population calculated")
#
# # Final plotting
# message("Step 7/7: Generating final map...")
# title <- subset(dim_muni, code_muni == code)$name_muni
#
# osm_features <- get_osm_aesthetics(place, buffer_zone)
#
#
# plot_pop <- plot_map(buffer_pop, title, subtitle)
# message("✓ Map generated successfully")
#
# cores <- viridis::inferno(n = length(unique(buffer_pop$level)) + 3)
# cores <- head(cores, length(cores) - 3)
#
# blues <- MetBrewer::met.brewer("Hokusai2", n = 9)
# greens <- MetBrewer::met.brewer("VanGogh3", n = 11)
#
# # green <- st_intersection(buffer_zone, st_make_valid(osm_green))
# # parking <- st_intersection(buffer_zone, st_make_valid(osm_parking))
# # water <- st_intersection(buffer_zone, st_make_valid(osm_water))
#
# # "motorway": 5,
# # "trunk": 5,
# # "primary": 4.5,
# # "secondary": 4,
# # "tertiary": 3.5,
# # "residential": 3,
#
# # "parking": {
# #   "fc": "#F2F4CB",
# #   "ec": "#2F3737",
#
# font = "Futura"
#
# coords_buffer <- st_bbox(buffer_zone)
#
# df_segment <- data.frame(
#   xmin = coords_buffer["xmin"],
#   xmax = coords_buffer["xmax"],
#   ymin = coords_buffer["ymin"]
#   )
#
# df_segment$midpoint <- df_segment$xmin + (df_segment$xmax - df_segment$xmin) / 2
#
# ggplot() +
#   geom_sf(data = osm_features$green, fill = greens[6], alpha = 0.8, color = "#F4F0E0") +
#   geom_sf(data = osm_features$water, fill = blues[7], color = "#F4F0E0") +
#   geom_sf(data = osm_features$parking, fill = "#2F3737", color = "#F4F0E0") +
#   geom_sf(
#     data = dplyr::filter(buffer_pop, highway == "motorway"),
#     aes(color = level, fill = level), linewidth = 0.5) +
#   geom_sf(
#     data = dplyr::filter(buffer_pop, highway == "trunk"),
#     aes(color = level, fill = level), linewidth = 0.5) +
#   geom_sf(
#     data = dplyr::filter(buffer_pop, highway == "primary"),
#     aes(color = level, fill = level), linewidth = 0.35) +
#   geom_sf(
#     data = dplyr::filter(buffer_pop, highway == "secondary"),
#     aes(color = level, fill = level), linewidth = 0.3) +
#   geom_sf(
#     data = dplyr::filter(buffer_pop, highway == "tertiary"),
#     aes(color = level, fill = level), linewidth = 0.25) +
#   geom_sf(
#     data = dplyr::filter(buffer_pop, highway == "residential"),
#     aes(color = level, fill = level), linewidth = 0.25) +
#   geom_sf(data = buffer_zone, fill = NA, color = "gray10", lwd = 1) +
#   geom_segment(
#     data = df_segment,
#     aes(x = xmin, xend = xmax, y = ymin - 0.005, yend = ymin - 0.005),
#     arrow = grid::arrow(ends = "both", length = unit(5, "pt"))
#   ) +
#   geom_text(
#     data = df_segment,
#     aes(x = midpoint, y = ymin - 0.01, label = "12 km")
#   ) +
#   scale_color_manual(values = cores) +
#   scale_fill_manual(values = cores) +
#   guides(fill = "none", color = "none") +
#   labs(title = title, subtitle = subtitle) +
#   ggthemes::theme_map() +
#   theme(
#     plot.title = element_text(
#       size = 22,
#       hjust = 0.5,
#       family = font
#     ),
#     plot.subtitle = element_text(
#       size = 12,
#       hjust = 0.5,
#       family = font,
#       color = "gray30"
#     ),
#     plot.background = element_rect(color = NA, fill = "#F4F0E0"),
#     panel.background = element_rect(color = NA, fill = "#F4F0E0"),
#     legend.background = element_rect(color = NA, fill = "#F4F0E0")
#   )
#
#
border <- get_border(5300108)
streets <- get_streets(code = 5300108, border)
dplyr::count(st_drop_geometry(streets), highway)

ggplot(streets) +
  geom_sf()

grid_bra <- get_population_grid(5300108, l = 250)
pop_class <- add_jenks_breaks(grid_bra, 9, variable = "pop")
bra_streets <- get_streets_population(pop_class, streets)

ggplot(bra_streets) +
  geom_sf(aes(color = level, fill = level))

print_plot(5300108)
