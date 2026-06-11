library(geobr)
library(sf)
library(dplyr)
library(ggplot2)
library(patchwork)

offwhite <- "#f8fbf8"

# Data ----

query <- "https://apisidra.ibge.gov.br/values/t/10329/n6/all/v/allxp/p/all/c469/12167,12188,79176/c2/6794/c386/9680/c2087/allxt"

req <- httr::GET(query)
raw <- httr::content(req, as = "text", encoding = "UTF-8")
dat <- jsonlite::fromJSON(raw, simplifyDataFrame = TRUE)

deslocamentos <- tibble(dat[-1, ])
names(deslocamentos) <- janitor::make_clean_names(
  as.character(
    dat[1, ]
  )
)

deslocamentos <- deslocamentos |>
  filter(
    sexo == "Total",
    classes_de_rendimento_nominal_mensal_domiciliar_per_capita == "Total"
  ) |>
  select(
    code_muni = municipio_codigo,
    workplace = local_de_exercicio_do_trabalho_principal,
    returns_home = retornam_do_trabalho_para_casa_3_tres_dias_ou_mais_na_semana,
    value = valor
  ) |>
  mutate(
    code_muni = as.numeric(code_muni),
    value = as.numeric(value),
    returns_home = if_else(returns_home == "Sim", 1L, 0L)
  )

str_simplify <- function(x) {
  y <- stringi::stri_trans_general(x, "Latin-ASCII")
  y <- stringr::str_to_lower(y)
  y <- stringr::str_to_snake(y)
  return(y)
}

share_workplace <- deslocamentos |>
  summarise(
    total = sum(value, na.rm = TRUE),
    .by = c("code_muni", "workplace")
  ) |>
  filter(workplace != "Total") |>
  mutate(share = total / sum(total) * 100, .by = "code_muni") |>
  mutate(workplace = str_simplify(workplace)) |>
  tidyr::pivot_wider(
    id_cols = "code_muni",
    names_from = "workplace",
    values_from = "share"
  )

rms <- read_metro_area(2018)
munis <- read_municipality(2022)

# Lookups & theme ----

theme_plot_map <- ggthemes::theme_map() +
  theme_sub_plot(
    title = element_text(size = 14, family = "Georgia", hjust = 0.5),
    background = element_rect(fill = offwhite, color = offwhite),
    margin = margin(8, 8, 8, 8)
  ) +
  theme_sub_panel(
    background = element_rect(fill = offwhite, color = offwhite)
  ) +
  theme_sub_legend(
    position = "bottom",
    key.width = unit(2, "cm"),
    key.height = unit(0.35, "cm"),
    title = element_text(size = 10, family = "Lora"),
    text = element_text(size = 8, family = "Lato"),
    title.position = "top",
    background = element_rect(fill = offwhite, color = offwhite)
  ) +
  # Match the legend backgrounds to the off-white plot background.
  theme(
    legend.background = element_rect(fill = offwhite, color = offwhite),
    legend.box.background = element_rect(fill = offwhite, color = offwhite),
    legend.key = element_rect(fill = offwhite, color = offwhite)
  )

city_names <- c(
  "spo" = "São Paulo",
  "rio" = "Rio de Janeiro",
  "bhe" = "Belo Horizonte",
  "poa" = "Porto Alegre"
)

# A single shared palette across all four metro regions so patchwork can collect
# one legend: the metric (share working in another municipality) is on the same
# 0-70% scale everywhere, so colours ARE comparable across regions.
map_palette <- "PuBuGn"

capital_codes <- c(
  "spo" = 3550308,
  "rio" = 3304557,
  "bhe" = 3106200,
  "poa" = 4314902
)

rm_names <- c(
  "spo" = "Rm São Paulo",
  "rio" = "Rm Rio de Janeiro",
  "bhe" = "Rm Belo Horizonte",
  "poa" = "Rm Porto Alegre"
)

# Functions ----

prepare_metro <- function(key) {
  rm_name <- rm_names[[key]]
  capital_code <- capital_codes[[key]]

  cities <- rms |>
    filter(name_metro == rm_name) |>
    pull(code_muni)

  border <- rms |>
    filter(name_metro == rm_name) |>
    summarise(geometry = st_union(geometry)) |>
    st_transform(crs = 31983) |>
    nngeo::st_remove_holes(max_area = 0) |>
    st_transform(crs = 4326)

  shapes <- munis |>
    filter(code_muni %in% cities) |>
    select(code_muni, name_muni)

  capital <- filter(shapes, code_muni == capital_code)

  work <- left_join(shapes, share_workplace, by = "code_muni")

  list(work = work, border = border, capital = capital)
}

build_metro_map <- function(
  key,
  fill = "outro_municipio",
  legend_title = "Share working in another city (%)",
  limits = c(0, 70)
) {
  geo <- prepare_metro(key)

  ggplot(geo$work) +
    geom_sf(aes(fill = .data[[fill]]), lwd = 0.2, color = "gray95") +
    geom_sf(data = geo$border, fill = NA, lwd = 0.3, color = "gray40") +
    geom_sf(data = geo$capital, fill = NA, lwd = 0.4, color = "gray35") +
    scale_fill_distiller(
      name = legend_title,
      palette = map_palette,
      direction = 1,
      limits = limits
    ) +
    labs(title = paste(city_names[[key]], "Metro Region")) +
    theme_plot_map
}

# Plots ----

plots <- lapply(names(city_names), build_metro_map)

final <- wrap_plots(plots, ncol = 2) +
  plot_layout(guides = "collect") +
  plot_annotation(
    title = "Crossing city lines to work",
    subtitle = paste0(
      "Share of employed residents who work in a municipality other than the ",
      "one they live in, by municipality within each\nmetropolitan region. ",
      "The capital is outlined; darker tones mark municipalities that send more ",
      "of their workers across the line."
    ),
    caption = "Source: IBGE (Census, 2022) — main workplace of employed residents • @viniciusoike",
    theme = theme(
      plot.background = element_rect(fill = offwhite, color = offwhite),
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
    legend.background = element_rect(fill = offwhite, color = offwhite),
    legend.box.background = element_rect(fill = offwhite, color = offwhite),
    legend.key = element_rect(fill = offwhite, color = offwhite)
  )

ggsave(
  here::here("2026", "plots", "13_ecossystems.png"),
  final,
  width = 8,
  height = 9,
  dpi = 400
)
