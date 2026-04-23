library(realestatebr)
library(trendseries)
library(ggtext)
library(tidyr)
library(ggplot2)
library(dplyr)
library(ragg)
library(waffle)
library(stringr)


fipe <- get_rppi_fipezap()

dat <- fipe |>
  filter(
    market == "residential",
    variable == "chg",
    rooms == "total",
    date >= as.Date("2019-01-01"),
    date <= as.Date("2024-12-01")
  )

dat <- dat |>
  filter(name_muni != "Índice Fipezap") |>
  mutate(is_rise = ifelse(value > 0, 1L, 0L))

count_dat <- dat |>
  summarise(
    count_rising = sum(is_rise, na.rm = TRUE),
    count_cities = n(),
    .by = c("date", "rent_sale")
  ) |>
  mutate(share_rising = count_rising / count_cities)

trend_dat <- count_dat |>
  group_by(rent_sale) |>
  nest() |>
  mutate(
    trend = purrr::map(
      data,
      add_trend,
      value_colname = "share_rising",
      trend = "stl"
    )
  ) |>
  unnest(trend)

plot_subtitle = "<p>Share of major Brazilian cities suffering monthly increases in <b><span style = 'color:#6096ba'>rental</span></b> or <b><span style = 'color:#274c77'>house prices</span></b> (%).<br/>In the post-pandemic period, <b><span style = 'color:#6096ba'>rental prices</span></b> have consistently risen in over 50% of cities, while <b><span style = 'color:#274c77'>house prices</span></b> have increased in more than 80% of cities every month. Smooth trend estimated using STL decomposition.</p>"

p_line <- ggplot(trend_dat) +
  geom_point(
    aes(x = date, y = share_rising, color = rent_sale),
    shape = 21,
    stroke = 1,
    alpha = 0.7
  ) +
  geom_line(
    aes(x = date, y = trend_stl, color = rent_sale),
    linewidth = 1
  ) +
  geom_hline(yintercept = 0) +
  geom_label(
    data = tibble(
      x = as.Date("2021-06-01"),
      y = 1,
      label = "In Jan/22, 96,4% of cities had an increase in house prices."
    ),
    aes(x = x, y = y, label = str_wrap(label, 18)),
    family = "Lato",
    size = 3,
    color = "#274c77",
    fill = "#f5f5f5"
  ) +
  geom_curve(
    data = data.frame(
      x = as.Date("2022-01-01"),
      xend = as.Date("2021-10-01"),
      y = 0.974,
      yend = 1
    ),
    aes(x = x, xend = xend, y = y, yend = yend),
    size = 0.45,
    arrow = arrow(length = unit(0.01, "npc")),
    color = "#1E3B5C"
  ) +
  geom_label(
    data = tibble(
      x = as.Date("2021-10-01"),
      y = 0.2,
      label = "Due to the Covid-19 Pandemic, in Sep/20, only 17,4% of cities had rent price increases."
    ),
    aes(x = x, y = y, label = str_wrap(label, 31)),
    family = "Lato",
    size = 3,
    color = "#6096ba",
    fill = "#f5f5f5"
  ) +
  geom_curve(
    data = data.frame(
      x = as.Date("2020-09-01"),
      xend = as.Date("2021-03-01"),
      y = 0.17,
      yend = 0.2
    ),
    aes(x = x, xend = xend, y = y, yend = yend),
    size = 0.45,
    arrow = arrow(length = unit(0.01, "npc")),
    curvature = -0.4,
    color = "#386380"
  ) +
  scale_color_manual(
    values = c("#6096ba", "#274c77")
  ) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") +
  scale_y_continuous(
    breaks = seq(0.2, 1, 0.2),
    labels = seq(20, 100, 20),
    limits = c(NA, 1.1)
  ) +
  guides(color = "none") +
  labs(
    title = "Houses prices are soaring across Brazil",
    subtitle = plot_subtitle,
    caption = "Souce: FipeZap (online listings).",
    x = NULL,
    y = "Share of cities (%)"
  ) +
  theme_minimal(base_family = "Lato") +
  theme(
    panel.background = element_rect(color = "#F5F5F5", fill = "#F5F5F5"),
    plot.background = element_rect(color = "#F5F5F5", fill = "#F5F5F5"),
    panel.grid.minor = element_blank(),
    plot.title = element_text(size = 14),
    plot.subtitle = element_textbox_simple(family = "Lato")
  )

ipca <- rbcb::get_series(433, start_date = as.Date("2010-01-01"), as = "tibble")

ipca_ano <- ipca |>
  rename(value = `433`) |>
  mutate(ano = lubridate::year(date)) |>
  summarise(
    acum = prod(1 + value / 100) - 1,
    .by = c("ano")
  )

fipe_ano <- fipe |>
  filter(
    market == "residential",
    variable == "acum12m",
    rooms == "total",
    date >= as.Date("2019-01-01"),
    date <= as.Date("2024-12-01"),
    name_muni != "Índice Fipezap"
  ) |>
  mutate(month = lubridate::month(date), ano = lubridate::year(date)) |>
  filter(month == 12)

fipe_ano <- fipe_ano |>
  select(ano, name_muni, rent_sale, fipe = value)

fipe_ano <- fipe_ano |>
  left_join(ipca_ano, by = "ano") |>
  mutate(is_higher_ipca = ifelse(fipe > acum, 1, 0))

fipe_comp <- fipe_ano |>
  filter(!is.na(fipe)) |>
  pivot_wider(
    id_cols = c("ano", "name_muni"),
    names_from = "rent_sale",
    values_from = "is_higher_ipca"
  ) |>
  filter(!is.na(rent), !is.na(sale)) |>
  mutate(
    category = case_when(
      sale == 1 & rent == 1 ~ "both_rising",
      sale == 1 & rent == 0 ~ "sale_rising",
      sale == 0 & rent == 1 ~ "rent_rising",
      sale == 0 & rent == 0 ~ "none_rising"
    )
  ) |>
  count(ano, category) |>
  mutate(share = n / sum(n), .by = "ano") |>
  mutate(
    category = factor(
      category,
      levels = c("none_rising", "rent_rising", "sale_rising", "both_rising")
    )
  )

p_col <- ggplot(fipe_comp, aes(ano, y = share, fill = category)) +
  geom_col() +
  geom_text(
    aes(label = paste0(round(share * 100), "%")),
    position = position_stack(0.5),
    family = "Lato",
    color = "white",
    size = 4
  ) +
  scale_x_reverse(breaks = 2019:2024, expand = expand_scale(0)) +
  scale_y_continuous(expand = expand_scale(0)) +
  coord_flip() +
  scale_fill_manual(
    name = "Houses prices rising above inflation",
    values = c("#778da9", "#ee9b00", "#bb3e03", "#9b2226"),
    labels = stringr::str_wrap(
      c(
        "Both below inflation",
        "Only rental prices above inflation",
        "Only sales prices above inflation",
        "Both rising above inflation"
      ),
      18
    )
  ) +
  labs(
    title = "In 2024, 81% of cities had both rental prices and sales prices rising above inflation",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_family = "Lato") +
  theme(
    panel.background = element_rect(color = "#F5F5F5", fill = "#F5F5F5"),
    plot.background = element_rect(color = "#F5F5F5", fill = "#F5F5F5"),
    plot.title = element_text(size = 14),
    legend.position = "top",
    legend.title.position = "top",
    panel.grid = element_blank(),
    axis.text.y = element_text(size = 12),
    axis.text.x = element_blank()
  )

subdat <- fipe_ano |>
  filter(!is.na(fipe)) |>
  pivot_wider(
    id_cols = c("ano", "name_muni"),
    names_from = "rent_sale",
    values_from = "is_higher_ipca"
  ) |>
  mutate(
    category = case_when(
      sale == 1 & rent == 1 ~ "both_rising",
      sale == 1 & (rent == 0 | is.na(rent)) ~ "sale_rising",
      (sale == 0 | is.na(sale)) & rent == 1 ~ "rent_rising",
      (sale == 0 | is.na(sale)) & (rent == 0 | is.na(rent)) ~ "none_rising"
    )
  ) |>
  count(ano, category) |>
  mutate(
    category = factor(
      category,
      levels = c("none_rising", "rent_rising", "sale_rising", "both_rising")
    )
  ) |>
  arrange(category)

base_plot <- ggplot(subdat, aes(fill = category, values = n)) +
  geom_waffle(color = "white", flip = TRUE, size = .25, n_rows = 10) +
  facet_wrap(vars(ano), nrow = 2) +
  scale_y_continuous(
    breaks = c(2, 4, 6, 8, 10, 12),
    labels = 5 * c(2, 4, 6, 8, 10, 12)
  ) +
  scale_fill_manual(
    name = "Houses prices rising above inflation",
    values = c("#2a9d8f", "#ee9b00", "#e9c46a", "#9b2226"),
    labels = stringr::str_wrap(
      c(
        "Both below inflation",
        "Only rental prices above inflation",
        "Only sales prices above inflation",
        "Both rising above inflation"
      ),
      18
    )
  ) +
  coord_equal() +
  theme_bw(base_family = "Lato") +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(size = 12)
  )

grid <- expand_grid(
  x = 1:10,
  y = 1:7
)

grid$z <- 1:nrow(grid)


base_plot <- ggplot(
  subset(subdat, ano == 2020),
  aes(fill = category, values = n)
) +
  geom_waffle(color = "white", flip = TRUE, size = .25, n_rows = 10) +
  scale_fill_manual(
    name = "Houses prices rising above inflation",
    values = c("#2a9d8f", "#ee9b00", "#e9c46a", "#9b2226"),
    labels = stringr::str_wrap(
      c(
        "Both below inflation",
        "Only rental prices above inflation",
        "Only sales prices above inflation",
        "Both rising above inflation"
      ),
      18
    )
  ) +
  coord_equal() +
  theme_bw(base_family = "Lato") +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(size = 12)
  )

# Single square highlight
base_plot +
  geom_tile(
    data = tibble(x = 8:10, y = 5),
    aes(x, y),
    linewidth = 0.7,
    color = "black",
    fill = NA,
    inherit.aes = FALSE
  )


# Full segment highlight
base_plot <- ggplot(
  subset(subdat, ano == 2019),
  aes(fill = category, values = n)
) +
  geom_waffle(color = "white", flip = TRUE, size = .25, n_rows = 10) +
  scale_fill_manual(
    name = "Houses prices rising above inflation",
    values = c("#2a9d8f", "#ee9b00", "#e9c46a", "#9b2226"),
    labels = stringr::str_wrap(
      c(
        "Both below inflation",
        "Only rental prices above inflation",
        "Only sales prices above inflation",
        "Both rising above inflation"
      ),
      18
    )
  ) +
  coord_equal() +
  theme_bw(base_family = "Lato") +
  theme(
    panel.grid = element_blank(),
    strip.text = element_text(size = 12)
  )

df_lines <- tibble(
  x = c(0.5, 3.5, 0.5, 0.5, 10.5, 3.5),
  xend = c(10.5, 10.5, 3.5, 0.5, 10.5, 3.5),
  y = c(0.5, 3.5, 4.5, 0.5, 0.5, 3.5),
  yend = c(0.5, 3.5, 4.5, 4.5, 3.5, 4.5)
)

library(ggtext)

font_text = "Lato"

base_plot +
  geom_segment(
    data = df_lines,
    aes(x = x, xend = xend, y = y, yend = yend),
    linewidth = 0.7,
    color = "black",
    inherit.aes = FALSE
  ) +
  geom_richtext(
    data = tibble(
      x = 5.5,
      y = 7.5,
      label = "In 2019, 66.7% of cities saw <span style='color:#2a9d8f'>rental and<br>house price increases below inflation</span>."
    ),
    aes(x = x, y = y, label = label),
    family = font_text,
    size = 3,
    inherit.aes = FALSE
  ) +
  scale_y_continuous(limits = c(NA, 8)) +
  theme(
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank()
  )


ggplot(grid, aes(x, y, fill = z)) +
  geom_tile(width = 0.9, height = 0.9)

library(plotly)


ggsave("plots/1_fraction_lineplot.png", p_line, width = 9.9, height = 5.7)
ggsave("plots/1_fraction_column.png", p_col, width = 9, height = 6)

fipe_comp <- fipe_ano |>
  filter(!is.na(fipe)) |>
  summarise(
    count_rising = sum(is_higher_ipca),
    count_cities = n(),
    .by = c("ano", "rent_sale")
  ) |>
  mutate(
    share_rising = count_rising / count_cities,
  )

share_rising <- dat |>
  pivot_wider(
    id_cols = c("date", "name_muni"),
    names_from = "rent_sale",
    values_from = "is_rise"
  ) |>
  filter(!is.na(rent), !is.na(sale)) |>
  mutate(
    category = case_when(
      sale == 1 & rent == 1 ~ "both_rising",
      sale == 1 & rent == 0 ~ "sale_rising",
      sale == 0 & rent == 1 ~ "rent_rising",
      sale == 0 & rent == 0 ~ "none_rising"
    )
  ) |>
  count(date, category) |>
  mutate(share = n / sum(n), .by = "date")


dfcol <- share_rising |>
  filter(date == max(date)) |>
  mutate(
    category = factor(
      category,
      levels = c("none_rising", "rent_rising", "sale_rising", "both_rising")
    )
  )

ggplot(dfcol, aes(x = 1, y = share, fill = category)) +
  geom_col(color = "white") +
  geom_text(
    aes(label = round(share * 100, 1)),
    position = position_stack(0.5),
    color = "white",
    family = "Lato",
    size = 5
  ) +
  annotate(
    "text",
    x = 1.6,
    y = 0.3,
    label = "In 66.7% of cities, both rental and sales prices are rising."
  ) +
  annotate(
    "text",
    x = 0.3,
    y = 0.5,
    label = "Only sales prices are rising."
  ) +
  coord_flip() +
  scale_fill_manual(values = c("#778da9", "#ee9b00", "#bb3e03", "#9b2226")) +
  theme_void() +
  theme(
    plot.margin = margin(5, 0, 5, 0)
  )


# Convert to plotly
ggplotly(base_plot, tooltip = c("text", "")) %>%
  layout(
    title = list(
      text = "<b>Interactive Housing Market Heat Map</b><br><sup>Price Growth vs Inflation (2019-2024)</sup>",
      font = list(size = 16)
    ),
    showlegend = TRUE,
    margin = list(t = 80, b = 50, l = 50, r = 150)
  ) %>%
  config(
    displayModeBar = TRUE,
    displaylogo = FALSE,
    modeBarButtonsToRemove = c(
      "pan2d",
      "select2d",
      "lasso2d",
      "autoScale2d",
      "hoverClosestCartesian",
      "hoverCompareCartesian"
    )
  )

library(plotly)
data(canada.cities, package = "maps")
viz <- ggplot(canada.cities, aes(long, lat)) +
  borders(regions = "canada") +
  coord_equal() +
  geom_point(aes(text = name, size = pop), colour = "red", alpha = 1 / 2)
ggplotly(viz, tooltip = c("text", "size"))

head(canada.cities)
