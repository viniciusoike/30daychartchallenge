library(tidyverse)
library(sf)

path <- '/Volumes/T7 Touch/github/tidyibge/data-raw/censo_2022/Agregados_por_setores_demografia_BR.csv'

pop_setores <- read_csv2(path, na = "X")

pop_setores <- pop_setores |>
  select(code_tract = CD_setor, pop = V01006) |>
  mutate(code_muni = as.numeric(str_sub(code_tract, 1, 7)))

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
    value = c("primary", "secondary", "tertiary", "residential")
  )

  #> Converte o dado

  streets <- osmdata::osmdata_sf(streets)
  streets <- streets[["osm_lines"]]
  streets <- dplyr::select(streets, osm_id, name)
  streets <- sf::st_transform(streets, crs = 4674)

  # streets <- streets |>
  #   osmdata::osmdata_sf() |>
  #   _$osm_lines |>
  #   dplyr::select(osm_id, name) |>
  #   sf::st_transform(crs = 4674)

  #> Enconrtra a intersecção entre as estradas e o limites do município
  streets_border <- sf::st_intersection(streets, border)
  #> Retorna o objeto streets_border
  return(streets_border)

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
    # Purely for aesthetical reasons
    dplyr::mutate(pop = sqrt(pop)) |>
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

dim_muni <- geobr::read_municipality(year = 2022)
dim_muni <- as_tibble(sf::st_drop_geometry(dim_muni))

population <- read_csv(here("data/day_3/top_cities_population.csv"))
density <- read_csv(here("data/day_3/top_cities_population_density.csv"))

population <- left_join(population, dim_muni, by = "code_muni")
density <- left_join(density, dim_muni, by = "code_muni")

code = 4106902

tracts <- geobr::read_census_tract(code, year = 2022, simplified = FALSE)

border <- get_border(4106902)

streets <- get_streets("Curitiba", border)

pt <- data.frame(lng = -49.269854, lat = -25.437640)

center <- st_as_sf(pt, coords = c("lng", "lat"), crs = 4326)

malha <- st_read('/Volumes/T7 Touch/github/tidyibge/data-raw/censo_2022/BR_setores_CD2022.gpkg')

malha |>
  filter(CD_MUN == 4314902) |>
  mapview::mapview()

buffer_zone <- center |>
  st_transform(29101) |>
  st_buffer(dist = 7000) |>
  st_transform(crs = 4674)

sf::sf_use_s2(FALSE)

pop_city <- filter(pop_setores, code_muni == code)
pop_tract <- left_join(tracts, pop_city, by = "code_tract")

grid <- pop_tract |>
  st_transform(29101) |>
  st_make_grid(cellsize = c(100, 100)) |>
  st_as_sf() |>
  st_transform(crs = 4674) |>
  mutate(gid = row_number())

grid_pop <- pop_tract |>
  select(pop) |>
  mutate(pop = sqrt(pop)) |>
  st_interpolate_aw(grid, extensive = TRUE, na.rm = TRUE)

pop_class <- add_jenks_breaks(grid_pop, variable = "pop", k = 9)

get_streets_population = function(population, streets) {

  stopifnot(any(colnames(population) %in% "jenks_group"))

  #> Encontra todos os grupos
  groups = levels(population$jenks_group)

  #> Define uma função auxiliar

  #> Esta função filtra o grid de population e faz a sua interseção
  #> com o shape das princiapis vias
  join_streets = function(group) {

    poly = population |>
      dplyr::filter(jenks_group == group) |>
      sf::st_union(.) |>
      sf::st_as_sf() |>
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
  pop_buffer <- st_interpolate_aw(select(pop_tract, pop), buffer_zone, extensive = TRUE, na.rm = TRUE)

  return(pop_buffer)

}

get_subtitle <- function(code) {

  pop_buffer <- interpolate_buffer_streets(buffer_zone, code)

  p0 <- subset(population, code_muni == code)[["population"]]
  p1 <- pop_buffer[["pop"]]
  s <- p1 / p0 * 100

  sub_pop_city <- format(round(p0), big.mark = ".")
  sub_pop_buffer <- format(round(p1), big.mark = ".")
  share_buffer <- round(s, 1)

  subtitle <- stringr::str_glue("Total pop.: {sub_pop_city}\nPop. inside circle: {sub_pop_buffer} ({share_buffer}%)")

  return(subtitle)
}

plot_map <- function(shp, title, subtitle, font = "Futura") {

  p <- ggplot(data = shp) +
    geom_sf(aes(color = level, fill = level), linewidth = 0.2) +
    scale_color_manual(values = cores) +
    scale_fill_manual(values = cores) +
    guides(fill = "none", color = "none") +
    labs(title = title, subtitle = subtitle) +
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
      plot.background = element_rect(color = NA, fill = "#f5f5f5"),
      panel.background = element_rect(color = NA, fill = "#f5f5f5"),
      legend.background = element_rect(color = NA, fill = "#f5f5f5")
    )

  return(p)

}


# test --------------------------------------------------------------------

library(showtext)
sysfonts::font_add("Futura", "Futura.ttc")
showtext_auto()

path <- '/Volumes/T7 Touch/github/tidyibge/data-raw/censo_2022/Agregados_por_setores_demografia_BR.csv'

pop_setores <- read_csv2(path, na = "X")

pop_setores <- pop_setores |>
  select(code_tract = CD_setor, pop = V01006) |>
  mutate(code_muni = as.numeric(str_sub(code_tract, 1, 7)))

population <- read_csv(here("data/day_3/top_cities_population.csv"))

font <- "Futura"
code <- 4106902

border <- get_border(code)
streets <- get_streets(code, border)
pop_grid <- get_population_grid(code)
pop_class <- add_jenks_breaks(pop_grid, k = 9, variable = "pop")
streets_pop <- get_streets_population(pop_class, streets)

pt <- data.frame(lng = -49.269854, lat = -25.437640)

buffer_zone <- get_buffer(pt)
buffer_pop <- st_intersection(buffer_zone, streets_pop)
subtitle <- get_subtitle(code)
title <- subset(dim_muni, code_muni == code)$name_muni

dput(population[1:10, 1]$code_muni)

points_center <- tribble(
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
  5208707, -16.666485683914015, -49.25212964879797
)

map_population <- function(code, l = 100, k = 9) {

  # Get streets
  border <- get_border(code)
  streets <- get_streets(code, border)

  # Population grid
  pop_grid <- get_population_grid(code, l = l)
  pop_class <- add_jenks_breaks(pop_grid, k = k, variable = "pop")

  # Intersect grid with streets
  streets_pop <- get_streets_population(pop_class, streets)

  # Define buffer
  pt <- data.frame(lng = -49.269854, lat = -25.437640)


  pt <- subset(points_center, code_muni == code)

  buffer_zone <- get_buffer(pt)
  buffer_pop <- st_intersection(buffer_zone, streets_pop)
  subtitle <- get_subtitle(code)
  title <- subset(dim_muni, code_muni == code)$name_muni
  plot_pop <- plot_map(buffer_pop, title, subtitle)

}





map_plot(streets_pop, labels = 1:9, title = "Curitiba")

cores = viridis::plasma(n = length(unique(streets_pop$level)))

buffer_pop <- st_intersection(buffer_zone, streets_pop)



pop_buffer <- st_interpolate_aw(select(pop_tract, pop), buffer_zone, extensive = TRUE, na.rm = TRUE)



ggplot(data = buffer_pop) +
  geom_sf(aes(color = level, fill = level), linewidth = 0.2) +
  scale_color_manual(
    name = "Altitude",
    values = cores
  ) +
  scale_fill_manual(
    values = cores
  ) +
  guides(fill = "none", color = "none") +
  labs(title = "Curitiba", subtitle = subtitle) +
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
    plot.background = element_rect(color = NA, fill = "#f6eee3"),
    panel.background = element_rect(color = NA, fill = "#f6eee3"),
    legend.background = element_rect(color = NA, fill = "#f6eee3")
  )


  guides(fill = guide_legend(nrow = 1), color = guide_legend(nrow = 1)) +
  ggtitle(title) +
  ggthemes::theme_map() +
  coord_sf() +
  theme(
    plot.title = element_text(
      size = 20,
      hjust = 0.5,
      family = font
    ),
    legend.title = element_text(
      size = 12,
      family = font,
      color = "gray10"
    ),
    legend.text = element_text(
      size = 14,
      family = font,
      color = "gray10"
    ),
    legend.position = "top",
    legend.direction = "horizontal",
    plot.background = element_rect(color = NA, fill = "#f6eee3"),
    panel.background = element_rect(color = NA, fill = "#f6eee3"),
    legend.background = element_rect(color = NA, fill = "#f6eee3")
  )

map_plot = function(shp, labels, title, showtext = TRUE) {

  cores = viridis::plasma(n = length(labels) + 1)
  cores = cores[-length(cores)]

  font = ifelse(showtext == TRUE, "Roboto Condensed", "sans")

  plot =
    ggplot(data = shp) +
    geom_sf(aes(color = level, fill = level), linewidth = 0.2) +
    scale_color_manual(
      name = "Altitude",
      labels = labels,
      values = cores
    ) +
    scale_fill_manual(
      name = "Altitude",
      labels = labels,
      values = cores
    ) +
    guides(fill = guide_legend(nrow = 1), color = guide_legend(nrow = 1)) +
    ggtitle(title) +
    ggthemes::theme_map() +
    coord_sf() +
    theme(
      plot.title = element_text(
        size = 30,
        hjust = 0.5,
        family = font
      ),
      legend.title = element_text(
        size = 20,
        family = font,
        color = "gray10"
      ),
      legend.text = element_text(
        size = 14,
        family = font,
        color = "gray10"
      ),
      legend.position = "top",
      legend.direction = "horizontal",
      plot.background = element_rect(color = NA, fill = "#f6eee3"),
      panel.background = element_rect(color = NA, fill = "#f6eee3"),
      legend.background = element_rect(color = NA, fill = "#f6eee3")
    )

  return(plot)

}


mapview::mapview(grid_pop)

st_interpolate_aw(pop_tract, to = grid, extensive = TRUE)



inter <- st_intersection(buffer_zone, streets)

ggplot(inter) +
  geom_sf()

ggplot(streets) +
  geom_sf()
