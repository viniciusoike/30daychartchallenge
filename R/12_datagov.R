library(tidyverse)
library(sf)
library(osmdata)

dat <- read_csv("//Volumes/T7 Touch/bases-de-dados/real-estate/Real_Estate_Sales_2001-2022_GL.csv")

dat <- dat |>
  janitor::clean_names() |>
  filter(!is.na(location)) |>
  mutate(
    lng = as.numeric(str_extract(location, "(?<=-)[0-9].+(?= )")),
    lng = -lng,
    lat = as.numeric(str_extract(location, "(?<=[0-9] )[0-9].+(?=\\))"))
  )

cities <- dat |>
  count(town, sort = TRUE) |>
  head(6) |>
  pull(town)

dat |>
  filter(town %in% cities) |>
  ggplot(aes(lng, lat)) +
  geom_point() +
  facet_wrap(vars(town), scales = "free")


bbox <- getbb(str_glue("{cities[1]}, Connecticut, United States"))

place <- osmdata::opq(bbox)

border <- add_osm_feature(place, key = "boundary", value = "administrative")
border <- osmdata_sf(border)

border <- border$osm_multipolygons |>
  filter(border_type == "town", name == cities[1])

subdat <- dat |>
  filter(list_year >= 2021, town == cities[1]) |>
  st_as_sf(coords = c("lng", "lat"), crs = 4326)


get_streets <- function(place, border) {

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

  #> Enconrtra a intersecção entre as estradas e o limites do município
  streets_border <- sf::st_intersection(streets, border)
  #> Retorna o objeto streets_border
  return(streets_border)

}

streets <- get_streets(place, border)

factorial(24)/(factorial(12) * factorial(12)) * (3/7)^13 * (4/7)^12

st_make_grid(border, cellsize = )

dnbinom(prob = 3/7, size = 13, x = 25 - 13)


ggplot() +
  geom_sf(data = subdat, alpha = 0.5, size = 0.1) +
  geom_sf(data = border, fill = NA) +
  geom_sf(data = streets, linewidth = 0.4)
