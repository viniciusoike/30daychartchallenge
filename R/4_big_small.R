library(geobr)
library(sf)
library(ggplot2)

library(osmdata)

bb <- getbb("London")

qr <- osmdata(bb)

name_place <- "London, England"

border <- add_osm_feature(place, key = "boundary", value = "administrative")

border <- osmdata_sf(border)

border

#> Monta a query
place <- osmdata::opq(bbox = osmdata::getbb(name_place))

#> Importa todas as principais vias da cidade
streets <- osmdata::add_osm_feature(
  place,
  key = "highway",
  value = c("motorway", "trunk", "primary", "secondary")
)

streets <- osmdata::osmdata_sf(streets)
streets <- osmdata::unique_osmdata(streets)
streets <- streets[["osm_lines"]]
streets <- dplyr::select(streets, osm_id, name, highway)


#> Enconrtra a intersecção entre as estradas e o limites do município
streets_border <- sf::st_intersection(streets, border)
#> Retorna o objeto streets_border
return(streets_border)



# Rio de Janeiro, Sao Paulo, New York, Buenos Aires, London, Madrid

rio <- read_municipality(3304557, year = 2022)
spo <- read_municipality(3550308, year = 2022)

