library(realestatebr)
library(trendseries)
library(patchwork)
library(ragg)
library(ggtext)
library(ggplot2)
library(dplyr)
library(tidyr)
library(stringr)
import::from(here, here)
import::from(MetBrewer, met.brewer)

str_simplify <- function(x) {
  y <- stringi::stri_trans_general(x, id = "latin-ascii")
  y <- stringr::str_replace_all(y, " ", "_")
  y <- stringr::str_to_lower(y)
  return(y)
}

# Data --------------------------------------------------------------------

igmi <- get_rppi_igmi()

ivar <- data.table::fread(here("data/day_9/xgdvConsulta.csv"), encoding = "Latin-1")

names(ivar) <- c("data", "brasil", "sao_paulo", "rio_de_janeiro", "belo_horizonte", "porto_alegre")

ivar <- ivar |>
  mutate(date = readr::parse_date(data, format = "%m/%Y")) |>
  select(-data) |>
  tidyr::pivot_longer(cols = brasil:porto_alegre, names_to = "city_name", values_to = "index")

igmi <- igmi |>
  mutate(city_name = str_simplify(name_muni))

base_igmi <- igmi |>
  mutate(ano = lubridate::year(date)) |>
  filter(date == as.Date("2018-12-01")) |>
  select(name_muni, base_index = index)

igmi <- igmi |>
  left_join(base_igmi, by = "name_muni") |>
  mutate(new_index = index / base_index * 100)

subigmi <- igmi |>
  filter(date >= as.Date("2018-12-01"))

subigmi <- subigmi |>
  select(date, city_name, index = new_index) |>
  mutate(series_name = "igmi")

ivar <- ivar |>
  mutate(series_name = "ivar")

dat <- bind_rows(ivar, subigmi)

dat <- filter(dat, date <= as.Date("2025-02-01"))

cities <- c("belo_horizonte", "porto_alegre", "rio_de_janeiro", "sao_paulo")

subdat <- filter(dat, city_name %in% cities)

brasil <- dat |>
  filter(city_name == "brasil") |>
  group_by(series_name) |>
  nest() |>
  mutate(trend = purrr::map(data, add_trend, value_colname = "index", trend = "stl")) |>
  unnest(cols = c(trend)) |>
  ungroup()

# Plot elements -----------------------------------------------------------

offwhite <- "#F2F0EF"
font <- "Roboto Condensed"
cores <- MetBrewer::met.brewer("Manet", 50)[c(8, 40)]

# Main plot ---------------------------------------------------------------

plot_subtitle <- "Divergence of residential property price indices in Brazil. Data reveals that house prices are up 58,3% in relation to rental prices as of Feb/2025. Rental prices estimated using IVAR (FGV) and Sales prices estimated using IGMI-R (Abecip). Selected cities presented in panel; number in parethesis calculates the current sales-rent gap. Dots present raw values and lines are smooth trends estimated using STL decomposition."

brasil <- brasil |>
  mutate(
    highlight_point = factor(case_when(
      series_name == "ivar" & date == as.Date("2022-02-01") ~ 1L,
      series_name == "igmi" & date == as.Date("2022-11-01") ~ 1L,
      TRUE ~ 0L
    )
  ))

base_plot <- ggplot(brasil) +
  geom_point(
    aes(x = date, y = index, color = highlight_point, alpha = highlight_point, fill = series_name),
    shape = 21,
    stroke = 0.3,
    size = 2
  ) +
  geom_line(
    aes(x = date, y = trend_stl, color = series_name),
    linewidth = 1
  ) +
  geom_hline(yintercept = 100) +
  facet_wrap(vars("Brazil")) +
  scale_color_manual(values = c("#f5f5f5", "black", cores)) +
  scale_fill_manual(values = cores) +
  scale_alpha_manual(values = c(0.7, 1)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(100, 180, 20))

annotated_plot <- base_plot +
  # Gap arrow vertical segment
  geom_segment(
    data = filter(brasil, date == max(date)),
    aes(x = date, y = max(index) - 2, yend = min(index) + 2, xend = date),
    arrow = arrow(length = unit(5, "pt"), ends = "both"),
    color = "gray15"
  ) +
  # Gap label
  geom_label(
    data = filter(brasil, date == max(date)),
    aes(x = date - months(4),
        y = (max(index) - 2 - (min(index) + 2)) / 2 + min(index) + 2,
        label = stringr::str_glue("Gap: {round(max(index) - min(index), 1)}%")),
    family = font,
    size = 4
  ) +
  # Rental text
  geom_label(
    data = tibble(x = as.Date("2022-06-01"), y = 120, label = "Rents only surpassed their pre-pandemic values in Feb/22"),
    aes(x = x, y = y, label = str_wrap(label, 30)),
    family = font,
    size = 4,
    color = cores[2],
    fill = "#f5f5f5"
  ) +
  # Rental arrow
  geom_curve(
    data = data.frame(x = as.Date("2022-02-01"), xend = as.Date("2022-04-01"), y = 103, yend = 110),
    aes(x = x, xend = xend, y = y, yend = yend),
    size = 0.35,
    arrow = arrow(length = unit(0.01, "npc")),
    color = cores[2],
    curvature = -0.2
  ) +
  # Sales text
  geom_label(
    data = tibble(x = as.Date("2022-05-01"), y = 170, label = "Houses prices had increased by over 50% in Nov/22"),
    aes(x = x, y = y, label = str_wrap(label, 20)),
    family = font,
    size = 4,
    color = cores[1],
    fill = "#f5f5f5"
  ) +
  # Sales arrow
  geom_curve(
    data = data.frame(x = as.Date("2022-11-01"), xend = as.Date("2022-06-01"), y = 152, yend = 157),
    aes(x = x, xend = xend, y = y, yend = yend),
    size = 0.35,
    arrow = arrow(length = unit(0.01, "npc")),
    color = cores[1],
    curvature = -0.2
  )

p1 <- annotated_plot +
  labs(
    x = NULL,
    y = "Index (100 = 2018/12)",
    title = "In Brazil, house prices have outpaced rental prices by a wide margin since the Pandemic",
    subtitle = plot_subtitle
  ) +
  theme_minimal(base_family = font) +
  theme(
    legend.position = "none",
    panel.background = element_rect(color = offwhite, fill = offwhite),
    plot.background = element_rect(color = offwhite, fill = offwhite),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12),
    plot.title = element_text(size = 20),
    plot.subtitle = element_textbox_simple(family = font, margin = unit(c(10, 0, 10, 0), "pt"), size = 12),
    strip.text = element_text(size = 12),
    plot.margin = margin(10, 5, 5, 5, unit = "pt")
  )



# Facet plot --------------------------------------------------------------



trend_cities <- subdat |>
  group_by(city_name, series_name) |>
  nest() |>
  mutate(trend = purrr::map(data, add_trend, value_colname = "index", trend = "stl")) |>
  unnest(trend) |>
  ungroup()

trend_cities |>
  filter(date == max(date)) |>
  summarise(
    gap = max(index) - min(index),
    .by = "city_name"
  )

trend_cities <- trend_cities |>
  mutate(
    city_label = case_when(
      city_name == "belo_horizonte" ~ "Belo Horizonte (30,2%)",
      city_name == "rio_de_janeiro" ~ "Rio de Janeiro (31,7%)",
      city_name == "porto_alegre" ~ "Porto Alegre (34,9%)",
      city_name == "sao_paulo" ~ "São Paulo (85,4%)"
    )
  )

p2 <- ggplot(trend_cities, aes(date, trend_stl, color = series_name)) +
  geom_point(
    aes(x = date, y = index, fill = series_name),
    color = "gray90",
    shape = 21,
    stroke = 0.8,
    size = 1.5,
    alpha = 0.7
  ) +
  geom_line(linewidth = 0.8) +
  geom_hline(yintercept = 100) +
  facet_wrap(vars(city_label)) +
  scale_color_manual(values = cores) +
  scale_fill_manual(values = cores) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(breaks = seq(100, 200, 20)) +
  labs(
    x = NULL,
    y = "Index (100 = 2018/12)"
  ) +
  theme_minimal(base_family = "Roboto Condensed") +
  theme(
    legend.position = "none",
    panel.background = element_rect(color = offwhite, fill = offwhite),
    plot.background = element_rect(color = offwhite, fill = offwhite),
    panel.grid.minor = element_blank(),
    axis.text = element_text(size = 12),
    strip.text = element_text(size = 12),
    plot.title = element_text(size = 14),
    plot.subtitle = element_textbox_simple(family = "Roboto Condensed")
  )


# Panel -------------------------------------------------------------------



panel <- p1 / p2

panel <- panel + plot_annotation(caption = "Sources: Sales prices (IGMI-R/Abecip). Rent prices (IVAR/FGV).") &
  theme(
    panel.background = element_rect(color = offwhite, fill = offwhite),
    plot.background = element_rect(color = offwhite, fill = offwhite),
    plot.caption = element_text(family = font)
  )

ggsave(here("plots/9_diverging.png"), panel, width = 11, height = 10)
