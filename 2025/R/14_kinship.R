library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ragg)
library(sf)



dat <- readr::read_csv2(
  '/Volumes/T7 Touch/github/tidyibge/data-raw/censo_2022/Agregados_por_setores_cor_ou_raca_BR.csv',
  locale = readr::locale(decimal_mark = ","),
  na = "X"
)

# V01317
# V01318
# V01319
# V01320
# V01321
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

pop_total_city <- pop_total_city %>%
  pivot_longer(cols = pop_branca:pop_indigena) %>%
  mutate(share = value / sum(value) * 100, .by = "code_muni")

code_city <- 4314902

sub_city <- subset(pop_total_city, code_muni == code_city)

sub_race <- sub_race |>
  tidyr::pivot_longer(cols = pop_branca:pop_indigena) |>
  mutate(prop = value / sum(value, na.rm = TRUE) * 100, .by = "code_tract") |>
  tidyr::pivot_wider(
    id_cols = c("code_tract", "code_muni"),
    names_from = "name",
    values_from = c("value", "prop")
    )

# borders <- lapply(code_cities, geobr::read_municipality, year = 2022, showProgress = FALSE)
# tracts <- lapply(code_cities, geobr::read_census_tract, year = 2022, showProgress = FALSE)
#
# dim_muni <- borders |>
#   bind_rows() |>
#   st_drop_geometry() |>
#   as_tibble() |>
#   mutate(name_simplified = janitor::make_clean_names(name_muni))
#
# name_city <- dim_muni$name_simplified
#
# names(borders) <- name_city
# names(tracts) <- name_city
#
# shp <- tracts$sao_paulo

font <- "Futura"
offwhite <- "#fefefe"

# city <- name_city[[7]]
# code_city <- dplyr::filter(dim_muni, name_simplified == city)$code_muni
#
# ct <- tracts[[city]]
# border <- borders[[city]]
#
# pop <- dplyr::filter(sub_race, code_muni == code_city)
# shp <- dplyr::left_join(ct, pop, by = "code_tract")
#
# get_jenks_breaks <- function(x, k = 6) {
#   stopifnot(is.numeric(x))
#   b <- BAMMtools::getJenksBreaks(x, k)
#   v <- findInterval(x, b, rightmost.closed = TRUE)
#   v <- as.factor(v)
#   return(v)
# }
#
# shp <- shp %>%
#   mutate(across(starts_with("prop"), \(x) ifelse(is.na(x), 0, x))) %>%
#   mutate(across(starts_with("value"), \(x) ifelse(is.na(x), 0, x)))
#
# shp <- shp %>%
#   mutate(
#     group_prop = get_jenks_breaks(prop_pop_amarela),
#     group_pop = get_jenks_breaks(value_pop_amarela)
#   )

breaks <- c(0, 1, 3, 5, 10, 17)

race_tract <- shp %>%
  mutate(pop = value_pop_branca + value_pop_preta + value_pop_amarela + value_pop_parda + value_pop_indigena) %>%
  filter(pop > 0) %>%
  mutate(
    group_pop = factor(findInterval(value_pop_amarela, breaks, rightmost.closed = TRUE))
  ) %>%
  st_transform(crs = 4326)

# dir.create(here("data/day_14"))
# st_write(race_tract, here("data/day_14/poa.geojson"))

poa_pop <- pop_total_city %>%
  filter(code_muni == 4314902) %>%
  mutate(is_amarelo = if_else(name == "pop_amarela", 1L, 0L)) %>%
  summarise(total = sum(value), .by = "is_amarelo")

font_text <- "DIN Alternate"
font_title <- "Charter"
offwhite <- "#fefefe"

pt <- c(-30.023465, -51.17843)
mark <- st_as_sf(st_sfc(st_point(rev(pt))), crs = 4326)
buffer_circle <- st_buffer(mark, 150)

border <- geobr::read_municipality(4314902, simplified = FALSE, showProgress = FALSE)

plot_map <- ggplot() +
  geom_sf(data = border, lwd = 0.8, color = "gray50", fill = "gray80") +
  geom_sf(data = race_tract, aes(fill = group_pop), lwd = 0.05, color = alpha("white", 0.2), alpha = 0.8) +
  geom_sf(data = buffer_circle) +
  geom_sf_label(
    data = buffer_circle,
    aes(label = "Where I lived\nmost of my life"),
    size = 3,
    family = font_text,
    nudge_x = 0.0175,
    nudge_y = 0) +
  scale_fill_manual(
    name = "People of Asian\norigin or descent",
    values = c("#3288BD", "#66C2A5", "#ABDDA4", "#FDAE61", "#D53E4F"),
    labels = c("0 - 1", "1 - 3", "3 - 5", "5 - 10", "10 - 17"),
    na.value = "gray90"
  ) +
  coord_sf(
    # xlim = c(-51.265, -51.11),
    xlim = c(-51.265, -51.09),
    ylim = c(-30.15, -29.95)) +
  ggthemes::theme_map(base_family = font_text) +
  labs(
    title = "Not many asians at all",
    subtitle = "Porto Alegre, Brazil\nPopulation 1,33 million - 1.373 residents of Asian heritage (0,1%).",
    caption = "Source: IBGE (Census, 2022)") +
  theme(
    plot.title = element_text(size = 18, hjust = 0, margin = margin(0, 0, 2.5, 0), family = font_title),
    plot.background = element_rect(color = offwhite, fill = offwhite),
    panel.background = element_rect(color = offwhite, fill = offwhite),
    legend.title = element_text(size = 12),
    legend.text = element_text(size = 10),
    legend.key.size = unit(20, "pt"),
    legend.position = c(0.8, 0),
    plot.margin = margin(15, 10, 15, 10),
  )

ggsave(
  here::here("plots/14_kinship.png"),
  plot_map,
  width = 7,
  height = 9
)

