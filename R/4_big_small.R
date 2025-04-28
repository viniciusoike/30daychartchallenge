library(geobr)
library(sf)
library(ggplot2)
library(tmap)

import::from(janitor, clean_names)


import::from(sidrar, get_sidra)

query <- "https://apisidra.ibge.gov.br/values/t/6893/n24/all/v/allxp/p/all/c125/2932,3247/c58/95253/c86/95251"

query <- "https://apisidra.ibge.gov.br/values/t/9514/n24/all/v/allxp/p/all/c2/6794/c287/6653,49108,49109,60040,60041,93070,93084,93085,93086,93087,93088,93089,93090,93091,93092,93093,93094,93095,93096,93097,93098,100362/c286/113635"

# Importar os dados da API
req <- httr::GET(url = query)
json <- httr::content(req, as = "text", encoding = "UTF-8")
json <- jsonlite::fromJSON(json, simplifyDataFrame = TRUE)

dat <- tibble(json[-1, ])
names(dat) <- unlist(json[1, ])

pop_age <- dat |>
  as_tibble() |>
  clean_names() |>
  rename(code_intermediate = regiao_geografica_intermediaria_codigo) |>
  select(code_intermediate, idade_codigo, idade, valor) |>
  mutate(across(-one_of("idade"), as.numeric)) |>
  mutate(
    age_min = as.numeric(str_extract(idade, "[0-9]{1,3}(?= a)")),
    age_max = as.numeric(str_extract(idade, "(?<= a )[0-9]{0,2}")),
    age_group = case_when(
      age_min < 15 ~ "Young",
      age_min > 64 ~ "Elder",
      TRUE ~ "Adult"
    )
  )

inter_pop_age <- pop_age |>
  summarise(total = sum(valor), .by = c("code_intermediate", "age_group")) |>
  mutate(age_group = str_to_lower(age_group)) |>
  pivot_wider(
    id_cols = "code_intermediate",
    names_from = "age_group",
    values_from = "total"
  ) |>
  mutate(
    index = elder / young * 100,
    total_pop = young + adult + elder
    )

# dat |>
#   as_tibble() |>
#   janitor::clean_names() |>
#   rename(code_intermediate = regiao_geografica_intermediaria_codigo) |>
#   select(code_intermediate, valor)
#
# tab_apart <- dat |>
#   as_tibble() |>
#   janitor::clean_names() |>
#   rename(code_intermediate = regiao_geografica_intermediaria_codigo) |>
#   filter(
#     tipo_de_domicilio %in% c("Total", "Apartamento"),
#     grupo_de_idade == "Total",
#     cor_ou_raca == "Total") |>
#   pivot_wider(
#     id_cols = "code_intermediate",
#     names_from = "tipo_de_domicilio",
#     values_from = "valor",
#     values_fn = as.numeric
#   ) |>
#   mutate(
#     code_intermediate = as.numeric(code_intermediate),
#     share = Apartamento / Total * 100
#     )

states <- read_state()
inter <- read_intermediate_region()

# inter_apto <- left_join(inter, tab_apart, by = c("code_intermediate"))

library(ragg)

inter_pop <- left_join(inter, inter_pop_age, by = "code_intermediate")

map_choro <- tm_shape(inter_pop) +
  tm_polygons(
    fill = "index",
    fill.scale = tm_scale_intervals(style = "jenks",
                                    n = 7,
                                    values = "-orange_blue_diverging"),
    fill.legend = tm_legend(title = "Aging Index",
                            position = tm_pos_in("left", "bottom")),
    col = NULL
  ) +
  tm_shape(inter) +
  tm_borders(lwd = 1) +
  tm_shape(states) +
  tm_borders(lwd = 3) +
  tm_layout(bg.color = "gray90")

map_point <- tm_shape(inter) +
  tm_polygons(lwd = 1, fill = "#fefefe") +
  tm_shape(states) +
  tm_borders(lwd = 3) +
tm_shape(inter_pop) +
  tm_symbols(
    size = "total_pop",
    fill = "index",
    col = "black",
    size.scale = tm_scale_continuous(values.scale = 5),
    fill.scale = tm_scale_intervals(style = "jenks",
                                    n = 7,
                                    values = "-orange_blue_diverging"),
    fill.legend = tm_legend(title = "Aging Index", na.show = FALSE, position = tm_pos_in("left", "bottom")),
    size.legend = tm_legend(title = "Population", position = tm_pos_in("left", "bottom"))) +
  tm_layout(bg.color = "gray90")

tmap_save(map_choro, here::here("plots/4_big_small_choro.png"))
tmap_save(map_point, here::here("plots/4_big_small.png"), width = 10.5, height = 10)


tm_shape(inter_apto) +
  tm_polygons(
    fill = "share",
    fill.scale = tm_scale_intervals(values = "orange_blue_diverging"),
    fill.legend = tm_legend(title = "Aging Index", position = tm_pos_in("left", "bottom")),
    col = NULL
  ) +
  tm_shape(inter) +
  tm_borders(lwd = 1) +
  tm_shape(states) +
  tm_borders(lwd = 3) +
  tm_title(
    "Share of population living in apartment buildings",
    width = 15,
    position = tm_pos_in("left", "top"),
    z = 0
    )

tm_shape(inter_apto) +
  tm_symbols(
    size = "Total",
    fill = "share",
    col = "black",
    size.scale = tm_scale_continuous(values.scale = 3),
    fill.scale = tm_scale_intervals(n = 5, values = "orange_blue_diverging")) +
  tm_shape(inter) +
  tm_borders(lwd = 1) +
  tm_shape(states) +
  tm_borders(lwd = 3)


ggplot(inter) + geom_sf()



hist(states_apto$share)

ggplot(states_apto) +
  geom_sf(aes(fill = share)) +
  scale_fill_fermenter(direction = 1, palette = "Greens", breaks = c(5, 10, 15, 20, 25, 30))

sidrar::info_sidra(6326)

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

