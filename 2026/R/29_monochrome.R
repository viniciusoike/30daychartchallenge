library(ggplot2)
library(dplyr)
library(patchwork)
library(ggtext)
library(forecast)
import::from(scales, number, percent)
options(scales.big.mark = ".")
options(scales.decimal.mark = ",")

pam <- readr::read_rds("2026/data/agriculture/pam_br.rds")

gray_scale <- c("#2D3748", "#4A5568", "#718096", "#A0AEC0", "#CBD5E0")
offwhite <- "#f8fbf8"
font_text <- "Roboto Slab"

theme_plot <- theme_minimal(base_family = font_text) +
  theme_sub_plot(
    title = element_text(size = 12, family = "Georgia", hjust = 0.5),
    subtitle = element_text(size = 10, color = "gray40", hjust = 0.5),
    caption = element_text(size = 8, color = "gray60"),
    margin = margin(15, 10, 10, 10),
    background = element_rect(fill = offwhite, color = offwhite)
  ) +
  theme_sub_panel(
    grid.minor = element_blank(),
    grid.major.x = element_blank(),
    grid.major.y = element_line(color = "gray70", linewidth = 0.5),
    background = element_rect(fill = offwhite, color = "gray20")
  ) +
  theme_sub_axis_x(
    ticks = element_line(color = "gray20"),
    line = element_line(color = "gray20", linewidth = 0.5),
    text = element_text(color = "gray20")
  )

total_area <- pam |>
  filter(
    year >= 1974,
    year <= 1980,
    name_crop == "Total",
    !is.na(value),
    variable == "Área colhida"
  )

series <- ts(log(total_area$value), start = c(1974), frequency = 1)
model <- tslm(series ~ trend)
fcast <- forecast(model, h = 2)
fcast <- forecast(auto.arima(series), h = 2)
forecast(model, h = 2, include = 6)

total_area <- total_area |>
  bind_rows(tibble(year = 1981:1982, value = exp(as.numeric(fcast$mean)))) |>
  mutate(is_fcast = factor(if_else(year > 1980, 1L, 0L)))


p1 <- ggplot(total_area, aes(year, value, fill = is_fcast)) +
  geom_col(width = 0.6) +
  geom_text(
    aes(label = number(value, scale = 1e-6, accuracy = 0.1), color = is_fcast),
    family = font_text,
    size = 4,
    nudge_y = -3e6
  ) +
  scale_x_continuous(breaks = 1974:1982) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.05)),
    labels = scales::label_number(scale = 1e-6)
  ) +
  scale_color_manual(values = c("#ffffff", "#000000")) +
  scale_fill_manual(values = gray_scale[c(1, 4)]) +
  guides(fill = "none", color = "none") +
  labs(
    title = "AUMENTO DA ÁREA COLHIDA",
    subtitle = "ÁREA COLHIDA DE LAVOURAS, MILHÕES DE HECTARES, (1974-1980)\nVALORES PREVISTOS PARA 1981-1982",
    x = NULL,
    y = NULL,
    caption = "FONTE: IBGE (PESQUISA AGRÍCOLA MUNICIPAL)"
  ) +
  theme_plot

sel_crops <- c(
  "Milho (em grão)",
  "Soja (em grão)",
  "Cana-de-açúcar",
  "Arroz (em casca)"
)

short_crop_names <- c(
  "Milho",
  "Soja",
  "Cana-de-açúcar",
  "Arroz"
)

dat_slope <- pam |>
  filter(
    year %in% c(1974, 1980),
    variable == "Área colhida",
    name_crop != "Total",
    !is.na(value)
  ) |>
  summarise(
    t0 = first(year),
    t1 = last(year),
    m0 = first(value),
    m1 = last(value),
    dm = (last(value) / first(value) - 1) * 100,
    da = last(value) - first(value),
    .by = "name_crop"
  )

crop_growth <- pam |>
  filter(
    name_crop %in% sel_crops,
    variable == "Área colhida",
    !is.na(value),
    year %in% c(1974, 1980)
  ) |>
  left_join(dat_slope) |>
  mutate(
    label_num = stringr::str_glue(
      "<b>{name_crop}</b><br>Ganho: +{percent(dm, scale = 1, accuracy = 0.1)}"
    ),
    name_crop = factor(
      name_crop,
      levels = c(
        "Soja (em grão)",
        "Milho (em grão)",
        "Cana-de-açúcar",
        "Arroz (em casca)"
      )
    )
  )

p2 <- ggplot(
  crop_growth,
  aes(
    year,
    value,
    fill = name_crop,
    color = name_crop,
    linetype = name_crop
  )
) +
  geom_line(lwd = 1) +
  geom_point(size = 2, shape = 21, stroke = 1) +
  geom_richtext(
    data = subset(crop_growth, year == 1980),
    aes(x = year, y = value, label = label_num),
    size = 3,
    family = font_text,
    hjust = 0,
    nudge_x = 0.2,
    inherit.aes = FALSE
  ) +
  scale_x_continuous(breaks = c(1974, 1980), expand = expansion(c(0.05, 0.2))) +
  scale_y_continuous(
    breaks = seq(2, 12, 2) * 1e6,
    labels = scales::label_number(scale = 1e-6),
    limits = c(NA, 12) * 1e6
  ) +
  scale_color_manual(name = NULL, values = gray_scale) +
  scale_fill_manual(name = NULL, values = gray_scale) +
  scale_linetype_discrete(name = NULL) +
  labs(
    title = "LAVOURAS COM MAIORES GANHOS",
    subtitle = "ÁREA COLHIDA DE LAVOURAS, MILHÕES DE HECTARES (1974-1980)",
    x = NULL,
    y = NULL,
    caption = "FONTE: IBGE (PESQUISA AGRÍCOLA MUNICIPAL)"
  ) +
  theme_plot +
  theme(
    plot.title = element_text(hjust = 0.5),
    legend.position = "bottom"
  )

grouped_value <- pam |>
  filter(
    variable == "Valor da produção",
    year >= 1974,
    year <= 1980,
    name_crop != "Total",
    !is.na(value)
  ) |>
  mutate(
    crop_trunc = if_else(name_crop %in% sel_crops, name_crop, "Outros"),
    crop_trunc = forcats::fct_reorder(crop_trunc, -value),
    crop_trunc = forcats::fct_relevel(crop_trunc, "Outros", after = Inf)
  ) |>
  summarise(total = sum(value, na.rm = TRUE), .by = c("year", "crop_trunc"))

p3 <- ggplot(grouped_value, aes(year, total, fill = crop_trunc)) +
  geom_area(color = "#000000", lwd = 0.25) +
  scale_x_continuous(breaks = 1974:1980, expand = expansion(0)) +
  scale_y_continuous(
    breaks = seq(0, 1000, 200) * 1e6,
    labels = scales::label_number(scale = 1e-6),
    expand = expansion(mult = c(0, 0.1))
  ) +
  scale_fill_manual(name = NULL, values = gray_scale) +
  labs(
    title = "VALOR DA PRODUÇÃO",
    subtitle = "BILHÕES DE CRUZEIROS, NOMINAL (1974-1980)",
    x = NULL,
    y = NULL,
    caption = "FONTE: IBGE (PESQUISA AGRÍCOLA MUNICIPAL)"
  ) +
  theme_plot +
  theme(
    legend.position = "bottom"
  )


panel <- p1 + p2 + p3 + plot_layout(ncol = 1)
panel <- panel +
  plot_annotation(title = "BOLETIM AGRÍCOLA BRASILEIRO", theme = theme_plot)

ggsave("2026/plots/29_monochrome.png", panel, width = 8, height = 12)
