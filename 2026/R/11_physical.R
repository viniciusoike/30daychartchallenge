# Prompt: Distributions
# Physical -- where Brazil lives, by altitude (population by elevation)

library(dplyr)
library(ggplot2)
library(ggridges)
library(sf)
library(patchwork)

import::from(here, here)
import::from(sidrar, get_sidra)
import::from(janitor, clean_names)
import::from(readr, read_rds, write_rds)
import::from(tibble, as_tibble)
import::from(geobr, read_municipal_seat, read_country, read_state)
import::from(elevatr, get_elev_point, get_elev_raster)
import::from(scales, number)

# terra stays qualified below: terra::aggregate would mask stats::aggregate.

# Data --------------------------------------------------------------------

cache_file <- here("2026", "data", "muni_elevation.rds")

if (file.exists(cache_file)) {
  seats_elev <- read_rds(cache_file)
} else {
  # Municipal seats (sede) as points; elevation is sampled at each seat.
  seats <- read_municipal_seat(year = 2022, showProgress = FALSE)

  # AWS Terrain Tiles (SRTM-derived) elevation at each of the ~5,570 seats.
  seats_elev <- get_elev_point(seats, src = "aws", z = 9) |>
    st_drop_geometry() |>
    as_tibble() |>
    select(code_muni, elevation)

  write_rds(seats_elev, cache_file)
}

# Municipality population (2025 estimate) -- the distribution weight.
pop_muni <- get_sidra(6579, period = "2025", geo = "City")

tab_pop <- pop_muni |>
  clean_names() |>
  as_tibble() |>
  select(code_muni = municipio_codigo, pop = valor) |>
  mutate(code_muni = as.numeric(code_muni))

# Macro-region from the first digit of the 7-digit municipality code.
regions <- c(
  "1" = "Norte",
  "2" = "Nordeste",
  "3" = "Sudeste",
  "4" = "Sul",
  "5" = "Centro-Oeste"
)

dat <- seats_elev |>
  mutate(code_muni = as.numeric(code_muni)) |>
  inner_join(tab_pop, by = "code_muni") |>
  filter(!is.na(elevation), !is.na(pop), pop > 0) |>
  mutate(region = regions[substr(as.character(code_muni), 1, 1)])

# Sanity checks -----------------------------------------------------------

w_median <- function(x, w) {
  ord <- order(x)
  x <- x[ord]
  w <- w[ord]
  x[which(cumsum(w) >= sum(w) / 2)[1]]
}

message("Municipios with elevation: ", nrow(dat))
message(
  "National pop-weighted median elevation (m): ",
  round(w_median(dat$elevation, dat$pop), 1)
)
dat |>
  summarise(
    med_elev = round(w_median(elevation, pop), 1),
    pop_mi = round(sum(pop) / 1e6, 1),
    .by = region
  ) |>
  arrange(med_elev) |>
  print()

# Order regions by population-weighted median elevation (lowest at top).
region_order <- dat |>
  summarise(med = w_median(elevation, pop), .by = region) |>
  arrange(desc(med)) |>
  pull(region)

dat <- dat |>
  mutate(region = factor(region, levels = region_order))

nat_median <- w_median(dat$elevation, dat$pop)

# Relief raster (balanced resolution) -------------------------------------

# z = 5 is a deliberate middle ground: enough detail to read the planalto
# and the Andes-fed west, but light enough (~1k x 1k cells) to render fast.
raster_cache <- here("2026", "data", "brazil_elev_z5.rds")

if (file.exists(raster_cache)) {
  relief <- read_rds(raster_cache)
  brazil <- read_country(year = 2020, showProgress = FALSE)
} else {
  brazil <- read_country(year = 2020, showProgress = FALSE)

  elev_rast <- get_elev_raster(brazil, z = 5, clip = "locations")

  relief <- terra::rast(elev_rast) |>
    terra::aggregate(fact = 2, fun = "mean", na.rm = TRUE) |>
    as.data.frame(xy = TRUE) |>
    setNames(c("x", "y", "elevation")) |>
    filter(!is.na(elevation)) |>
    mutate(elevation = pmax(elevation, 0))

  write_rds(relief, raster_cache)
}

# Theme (matches 07_multiscale.R) -----------------------------------------

base_text <- "Lato"
title_text <- "Lora"
offwhite <- "#f5f5dc"

# Hypsometric ramp shared by the map and the ridges: green lowlands ->
# tan -> brown highlands -> white peaks.
hypso <- c(
  "#2c7c5f", "#74c69d", "#b7e4c7", "#e9edc9",
  "#e9d8a6", "#dda15e", "#bc6c25", "#8a5a44", "#f7f7f7"
)

label_meters <- function(x) {
  number(
    x,
    accuracy = 1,
    suffix = " m",
    big.mark = ".",
    decimal.mark = ","
  )
}

theme_plot <- theme_minimal(base_family = base_text) +
  theme_sub_panel(
    grid.minor = element_blank(),
    grid.major.y = element_blank(),
    grid.major.x = element_line(color = "gray80", linewidth = 0.3),
    background = element_rect(fill = offwhite, color = offwhite)
  ) +
  theme_sub_plot(
    background = element_rect(fill = offwhite, color = offwhite),
    margin = margin(12, 16, 8, 12),
    title = element_text(family = title_text, size = 16, hjust = 0),
    subtitle = element_text(
      family = title_text,
      size = 10,
      color = "gray20",
      margin = margin(2, 0, 12, 0)
    ),
    caption = element_text(
      family = base_text,
      size = 6,
      color = "gray50",
      hjust = 0
    )
  ) +
  theme_sub_axis_x(line = element_line(color = "gray20", linewidth = 0.3)) +
  theme_sub_axis_y(
    text = element_text(family = base_text, size = 9, color = "gray20"),
    title = element_blank()
  ) +
  theme(legend.position = "none")

# Map panel ---------------------------------------------------------------

states <- read_state(year = 2020, showProgress = FALSE)

p_map <- ggplot() +
  geom_raster(data = relief, aes(x, y, fill = elevation)) +
  geom_sf(
    data = states,
    fill = NA,
    color = alpha("gray15", 0.30),
    linewidth = 0.12
  ) +
  geom_sf(data = brazil, fill = NA, color = "gray20", linewidth = 0.3) +
  scale_fill_gradientn(
    colors = hypso,
    name = "Elevation",
    labels = label_meters,
    breaks = c(0, 500, 1000, 1500, 2000),
    limits = c(0, NA),
    guide = guide_colorbar(
      direction = "vertical",
      barwidth = unit(5, "pt"),
      barheight = unit(60, "pt"),
      title.position = "top",
      ticks.colour = NA
    )
  ) +
  coord_sf(expand = FALSE) +
  theme_void(base_family = base_text) +
  theme(
    plot.background = element_rect(fill = offwhite, color = offwhite),
    panel.background = element_rect(fill = offwhite, color = offwhite),
    plot.margin = margin(6, 6, 6, 12),
    legend.position = c(0.16, 0.26),
    legend.title = element_text(size = 8, color = "gray20"),
    legend.text = element_text(size = 6.5, color = "gray30")
  )

# Ridgeline panel ---------------------------------------------------------

# Cap the x-range so a handful of high-altitude towns don't flatten the
# ridges; >99% of the population lives below this elevation.
x_cap <- 1500

p_ridge <- ggplot(dat, aes(x = elevation, y = region)) +
  geom_density_ridges_gradient(
    aes(weight = pop, fill = after_stat(x)),
    scale = 1.6,
    rel_min_height = 0.004,
    color = "gray25",
    linewidth = 0.3,
    bandwidth = 45
  ) +
  geom_vline(
    xintercept = nat_median,
    color = "gray30",
    linetype = "dashed",
    linewidth = 0.3
  ) +
  annotate(
    "text",
    x = nat_median + 30,
    y = length(region_order) + 0.85,
    label = paste0("National median: ", round(nat_median), " m"),
    family = base_text,
    size = 2.6,
    color = "gray30",
    hjust = 0
  ) +
  scale_x_continuous(
    labels = label_meters,
    breaks = seq(0, x_cap, 250),
    limits = c(0, x_cap),
    expand = expansion(mult = c(0.01, 0.02))
  ) +
  scale_y_discrete(expand = expansion(add = c(0.2, 1.4))) +
  scale_fill_gradientn(colors = hypso, limits = c(0, x_cap), guide = "none") +
  labs(x = "Elevation of municipal seat", y = NULL) +
  theme_plot

# Compose -----------------------------------------------------------------

panel <- (p_map | p_ridge) +
  plot_layout(widths = c(1, 1.1)) +
  plot_annotation(
    title = "Brazil lives close to sea level",
    subtitle = "Where Brazilians live, by altitude. The map shows terrain elevation; each ridge is the population-weighted distribution\nof municipal-seat elevations within a macro-region. The Sudeste and Centro-Oeste planalto pull the country upward.",
    caption = "Source: IBGE (municipal population estimate, 2025) and AWS Terrain Tiles / SRTM elevation sampled at municipal seats • @viniciusoike\nRidges weighted by municipal population; x-axis capped at 1.500 m, above which less than 1% of Brazilians live.",
    theme = theme(
      plot.background = element_rect(fill = offwhite, color = offwhite),
      plot.margin = margin(12, 14, 8, 12),
      plot.title = element_text(family = title_text, size = 18, hjust = 0),
      plot.subtitle = element_text(
        family = title_text,
        size = 10,
        color = "gray20",
        margin = margin(2, 0, 10, 0)
      ),
      plot.caption = element_text(
        family = base_text,
        size = 6,
        color = "gray50",
        hjust = 0
      )
    )
  )

ggsave(
  here("2026/plots/11_physical.png"),
  panel,
  width = 10,
  height = 6,
  dpi = 400
)
