library(sf)
library(dplyr)
library(ggplot2)

library(ggplot2)
library(sf)
library(basemapR)

ggplot() +
  base_map(st_bbox(localauth_data), increase_zoom = 2, basemap = "positron") +
  geom_sf(data = localauth_data, fill = NA)

# V01317
# V01318
# V01319
# V01320
# V01321

dat <- readr::read_csv2(
  '/Volumes/T7 Touch/github/tidyibge/data-raw/censo_2022/Agregados_por_setores_cor_ou_raca_BR.csv',
  locale = readr::locale(decimal_mark = ","),
  na = "X"
)

pop_race <- dat |>
  select(CD_SETOR, V01317:V01321) |>
  rename(
    code_tract = CD_SETOR,
    pop_branca = V01317,
    pop_preta = V01318,
    pop_amarela = V01319,
    pop_parda = V01320,
    pop_indigena = V01321
    ) |>
  mutate(
    code_muni = as.numeric(substr(code_tract, 1, 7))
  )

pop_total_city <- pop_race |>
  summarise(
    across(pop_branca:pop_indigena, ~sum(.x, na.rm = TRUE)),
    .by = "code_muni"
    )

code_cities <- pop_total_city |>
  arrange(desc(pop_amarela)) |>
  head(6) |>
  pull(code_muni)

code_cities <- c(code_cities, 4314902, 4310801)

sub_race <- filter(pop_race, code_muni %in% code_cities)

sub_race <- sub_race |>
  tidyr::pivot_longer(cols = pop_branca:pop_indigena) |>
  mutate(prop = value / sum(value, na.rm = TRUE) * 100, .by = "code_tract") |>
  tidyr::pivot_wider(id_cols = c("code_tract", "code_muni"), names_from = "name", values_from = c("value", "prop"))

borders <- lapply(code_cities, geobr::read_municipality, year = 2022, showProgress = FALSE)
tracts <- lapply(code_cities, geobr::read_census_tract, year = 2022, showProgress = FALSE)

dim_muni <- borders |>
  bind_rows() |>
  st_drop_geometry() |>
  as_tibble() |>
  mutate(name_simplified = janitor::make_clean_names(name_muni))

name_city <- dim_muni$name_simplified

names(borders) <- name_city
names(tracts) <- name_city

library(ragg)

pt_sp <- c(-23.55986778522547, -46.63589418961893)
pt_maringa <- c(-23.418493, -51.938120)

center <- st_as_sf(data.frame(lat = pt_sp[1], lng = pt_sp[2]), coords = c("lng", "lat"), crs = 4326)

bbox <- center |>
  st_transform(crs = 32722) |>
  st_buffer(dist = 10000, endCapStyle = "SQUARE") |>
  st_transform(crs = 4674) |>
  st_bbox() |>
  st_as_sfc() |>
  st_as_sf()
#
# tmap_mode(mode = "plot")
# sf::sf_use_s2(FALSE)
# shp <- bbox |>
#   st_intersection(shp)

# tm_shape(shp, bbox) +
#   tm_polygons(
#     fill = "value_pop_amarela",
#     col = NULL,
#     lwd = 0.1,
#     fill_alpha = 0.6,
#     fill.legend = tm_legend(
#       "People of Asian\norigin or descent",
#       position = tm_pos_in("right", "bottom"),
#       title.fontfamily = "Roboto Slab",
#       text.fontfamily = "Roboto Slab"
#       ),
#     fill.scale = tm_scale_intervals(
#       style = "jenks",
#       n = 5,
#       values = "inferno",
#     )) +
#   tm_title(
#     text = "São Paulo",
#     fontfamily = "Roboto Slab"
#   ) +
#   tm_shape(ct) +
#   tm_borders(lwd = 0.05) +
#   tm_shape(border) +
#   tm_borders(lwd = 1) +
#   tm_basemap("CartoDB.PositronNoLabels")


breaks_interval <- BAMMtools::getJenksBreaks(shp$value_pop_amarela, k = 6)

shp <- shp |>
  mutate(
    group = findInterval(value_pop_amarela, breaks_interval, rightmost.closed = TRUE),
    group = as.factor(group)
  )

font <- "Futura"
offwhite <- "#F4F0E0"

city <- name_city[[1]]
code_city <- dplyr::filter(dim_muni, name_simplified == city)$code_muni

ct <- tracts[[city]]
border <- borders[[city]]

pop <- dplyr::filter(sub_race, code_muni == code_city)
shp <- dplyr::left_join(ct, pop, by = "code_tract")

ggplot() +
  geom_sf(data = border, lwd = 0.8, color = "gray50", fill = "gray60") +
  geom_sf(data = shp, aes(fill = group), lwd = 0.05, color = "white") +
  scale_fill_viridis_d(
    "People of Asian\norigin or descent",
    option = "inferno",
    labels = c("0-8", "9-25", "26-57", "58-125", "126-375"),
    na.value = "gray60"
    ) +
  coord_sf(xlim = c(-46.8, -46.5), ylim = c(-23.7, -23.47)) +
  ggtitle("São Paulo") +
  theme_void(base_family = "Charter") +
  theme(
    plot.title = element_text(size = 22, hjust = 0.5, margin = margin(10, 0, 15, 0)),
    plot.background = element_rect(color = offwhite, fill = offwhite),
    panel.background = element_rect(color = "gray50", fill = offwhite),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.key.size = unit(25, "pt"),
    legend.position = c(0.8, 0.2),
    plot.margin = margin(10, 5, 10, 5),
    )

city <- name_city[[7]]
code_city <- dplyr::filter(dim_muni, name_simplified == city)$code_muni

ct <- tracts[[city]]
border <- borders[[city]]

pop <- dplyr::filter(sub_race, code_muni == code_city)
shp <- dplyr::left_join(ct, pop, by = "code_tract")

pop <- pop |>
  select(code_tract, starts_with("value")) |>
  mutate(across(starts_with("value"), ~ifelse(is.na(.x), 0, .x))) |>
  rowwise() |>
  mutate(total_pop = sum(across(starts_with("value"))))

breaks_interval <- BAMMtools::getJenksBreaks(shp$value_pop_amarela, k = 5)

breaks_interval <- c(0, 1, 10, 17)

shp <- shp |>
  mutate(
    group = findInterval(value_pop_amarela, breaks_interval, rightmost.closed = TRUE),
    group = as.factor(group)
  )

bbox <- st_bbox(shp)
bbox <- st_as_sfc(bbox)
bbox <- st_as_sf(bbox)

# border <- st_transform(border, 3857)
# shp <- st_transform(shp, 3857)

ggplot() +
  base_map(st_bbox(bbox), increase_zoom = 5, basemap = "positron") +
  geom_sf(data = border, lwd = 0.8, color = "gray50", fill = NA) +
  geom_sf(
    data = filter(shp, total_pop > 0),
    aes(fill = value_pop_amarela),
    alpha = 0.7,
    lwd = 0.05,
    color = "white") +
  scale_fill_viridis_c(
    "People of Asian\norigin or descent",
    option = "inferno",
    na.value = "gray60"
  ) +
  coord_sf(xlim = c(-51.27, -51.075), ylim = c(-30.1, -29.95)) +
  theme_void() +
  theme(
    plot.title = element_text(size = 22, hjust = 0.5, margin = margin(10, 0, 15, 0)),
    plot.background = element_rect(color = offwhite, fill = offwhite),
    panel.background = element_rect(color = "gray50", fill = offwhite),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 12),
    legend.key.size = unit(25, "pt"),
    legend.position = c(0.9, 0.15),
    plot.margin = margin(10, 5, 10, 5),
  )
