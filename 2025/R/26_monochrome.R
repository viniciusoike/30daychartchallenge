library(ggplot2)
library(dplyr)
library(tidyr)
import::from(purrr, map)
import::from(stringr, str_wrap)

codes <- tribble(
  ~code, ~name_series,
  1406, "energia_br_todos",
  24364, "ibcbr_dessaz"
)

code_energy <- 1406
code_gdp_dessaz <- 24364

series <- lapply(codes$code, rbcb::get_series)
names(series) <- codes$name_series
series <- lapply(series, \(d) {
  d <- mutate(d, id = names(d)[2])
  names(d)[2] <- "value"
  return(d)
})

tbl_series <- bind_rows(series, .id = "name_series")

tbl_max_date <- tbl_series |>
  summarise(m = max(date), .by = "name_series")

length(unique(tbl_max_date$m)) == 1

max_date <- unique(tbl_max_date$m)

tbl_series <- tbl_series |>
  filter(
    date >= as.Date("2003-01-01"),
    date <= max_date
  )

energ_nest <- tbl_series |>
  filter(id %in% code_energy) |>
  select(date, id, name_series, value) |>
  group_by(name_series, id) |>
  nest() |>
  mutate(
    y = map(data, \(d) stats::ts(log(d$value), start = c(2003, 1), frequency = 12)),
    model_stl = map(y, forecast::mstl, s.window = 51, robust = TRUE),
    decomp = map(model_stl, as.data.frame)
  )

energ_trend <- energ_nest |>
  select(name_series, id, data, decomp) |>
  unnest(cols = c(data, decomp)) |>
  ungroup() |>
  select(-Data) |>
  rename_with(tolower)

energ_trend <- energ_trend |>
  select(name_series, id, date, value = trend) |>
  mutate(value = exp(value))

dat <- energ_trend |>
  bind_rows(filter(tbl_series, id %in% code_gdp_dessaz))

# Index values in a common year
base_index <- dat |>
  mutate(ano = lubridate::year(date)) |>
  filter(ano == 2005) |>
  summarise(base_index = mean(value), .by = "name_series")

dat <- dat |>
  left_join(base_index, by = "name_series") |>
  mutate(index = value / base_index * 100)

# Auxiliar table to show original series
tbl_energ <- tbl_series |>
  filter(id == "1406", date >= as.Date("2003-01-01"))

base_index_energ <- tbl_energ |>
  mutate(ano = lubridate::year(date)) |>
  filter(ano == 2005) |>
  summarise(base_index = mean(value), .by = "name_series")

tbl_energ <- tbl_energ |>
  left_join(base_index_energ, by = "name_series") |>
  mutate(index = value / base_index * 100)

dbreaks <- c(
  as.Date("2003-01-01"),
  seq(as.Date("2005-01-01"), as.Date("2025-01-01"), by = "5 year")
)

library(ragg)

font <- "Futura"

codace <- tribble(
  ~rec_start,               ~rec_end, ~label,
  #-------------------#----------------------#----------#
  as.Date("1997-10-01"), as.Date("1999-01-01"), "FHC-1",
  as.Date("2001-01-01"), as.Date("2001-10-01"), "FHC-2",
  as.Date("2002-10-01"), as.Date("2003-04-01"), "LULA",
  as.Date("2008-07-01"), as.Date("2009-01-01"), "GREAT FINANCIAL RECESSION",
  as.Date("2014-01-01"), as.Date("2016-10-01"), "2014-16 ECONOMIC CRISIS",
  as.Date("2019-10-01"), as.Date("2020-04-01"), "COVID-19 CRISIS"
)

colors <- c("gray40", "gray40", "gray15")
offwhite <- "#ffffff"

plot_line <- ggplot() +
  geom_hline(yintercept = 100) +
  geom_rect(
    data = dplyr::filter(codace, rec_end >= as.Date("2003-06-01")),
    aes(xmin = rec_start, xmax = rec_end, ymin = -Inf, ymax = Inf, group = label),
    alpha = 0.4
  ) +
  geom_text(
    data = dplyr::filter(codace, rec_end >= as.Date("2003-06-01")),
    aes(x = (rec_end - rec_start) / 2 + rec_start, y = 160, label = str_wrap(label, 7)),
    size = 3,
    family = font
  ) +
  geom_line(
    data = tbl_energ,
    aes(date, index),
    color = colors[1],
    alpha = 0.5
  ) +
  geom_point(
    data = tbl_energ,
    aes(date, index, color = name_series),
    #color = colors[1],
    alpha = 0.5,
    size = 0.5,
    shape = 21
  ) +
  geom_line(
    data = filter(dat, id %in% c("1406", code_gdp_dessaz), date >= as.Date("2003-01-01")),
    aes(date, index, color = name_series),
    lwd = 0.9
  ) +
  geom_text(
    data = tibble(x = as.Date("2002-06-01"), y = seq(90, 160, 10)),
    aes(x, y + 2.5, label = y),
    family = font,
    size = 4,
    color = "gray30"
  ) +
  geom_label(
    data = filter(dat, date == max(date)),
    aes(as.Date("2025-06-01"), index, label = c("ENERGY", "GDP")),
    size = 3,
    family = font,
    hjust = 0
  ) +
  scale_x_date(
    breaks = dbreaks,
    date_labels = "%Y",
    expand = expansion(mult = c(0.05, 0.1))) +
  scale_y_continuous(breaks = seq(90, 160, 10)) +
  scale_color_manual(
    name = "",
    values = colors[c(2, 3)],
    labels = c("ELECTRIC ENERGY CONSUMPTION", "GDP PROXY (IBC-BR)")
  ) +
  labs(
    x = NULL,
    y = "INDEX (100 = AVG. 2005)",
    title = "ENERGY CONSUMPTION AND ECONOMIC GROWTH",
    caption = "SOURCE: BRAZILIAN CENTRAL BANK, ELETROBRAS"
    ) +
  theme_minimal(base_family = "Futura") +
  theme(
    plot.background = element_rect(fill = offwhite, color = offwhite),
    panel.background = element_rect(fill = offwhite, color = offwhite),
    plot.margin = margin(20, 10, 5, 10),
    legend.position = "top",
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(linetype = 2, linewidth = 0.25),
    axis.text.y = element_blank(),
    plot.title = element_text(hjust = 0.5, size = 22),
    axis.text.x = element_text(size = 12, color = "#6B6865")
  )

ggsave("plots/26_monochrome.png", plot_line, width = 9, height = 5.3)
