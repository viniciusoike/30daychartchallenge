library(sf)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggtext)
library(ragg)
library(patchwork)

offwhite <- "#FFFAF0"

import::from(sidrar, get_sidra)

muni <- geobr::read_municipality(year = 2020, showProgress = FALSE)
dim_muni <- as_tibble(sf::st_drop_geometry(muni))
analf <- get_sidra(9543, geo = "City", classific = "c2")

tbl_gender <- analf |>
  janitor::clean_names() |>
  as_tibble() |>
  filter(sexo != "Total") |>
  select(code_muni = municipio_codigo, sex = sexo, rate = valor) |>
  mutate(code_muni = as.numeric(code_muni))

analf_gender <- tbl_gender |>
  mutate(sex = factor(sex)) |>
  pivot_wider(id_cols = "code_muni", names_from = "sex", values_from = "rate") |>
  rename_with(tolower) |>
  mutate(gender_gap = homens - mulheres) |>
  arrange(gender_gap)

analf_gender <- analf_gender |>
  left_join(dim_muni) |>
  mutate(
    is_nordeste = factor(if_else(code_region == 2, 1L, 0L))
  )

tbl_analf <- analf |>
  janitor::clean_names() |>
  as_tibble() |>
  filter(sexo == "Total") |>
  select(code_muni = municipio_codigo, rate = valor) |>
  mutate(code_muni = as.numeric(code_muni))

analf_city <- left_join(muni, tbl_analf, by = "code_muni")

analf_city <- analf_city |>
  mutate(analf_rate = 100 - rate)

breaks_jenks <- BAMMtools::getJenksBreaks(analf_city$analf_rate, k = 9)[-1]
breaks_jenks <- ceiling(breaks_jenks)

labels <- c("<5%", "5-8%", "8-11%", "11-15%", "15-19%", "19-23%", "23-27%", "27-37%")

analf_city <- analf_city |>
  mutate(analf_group = factor(findInterval(analf_rate, breaks_jenks, left.open = TRUE)))

p1 <- ggplot(analf_city) +
  geom_sf(aes(fill = analf_group), lwd = 0.04, color = "gray90") +
  scale_fill_brewer(
    name = "",
    type = "div",
    direction = -1,
    labels = labels
  ) +
  scale_color_brewer(
    name = "",
    type = "div",
    direction = -1,
    labels = labels
  ) +
  labs(
    title = "Illiteracy Rate in Brazil",
    subtitle = "Illiteracy rate, people aged 15 and over, by municipality."
  ) +
  coord_sf(xlim = c(NA, -35)) +
  ggthemes::theme_map(base_family = "Gill Sans") +
  theme(
    plot.background = element_rect(fill = offwhite, color = offwhite),
    panel.background = element_rect(fill = offwhite, color = offwhite),
    legend.background = element_rect(fill = offwhite, color = offwhite),
    legend.position.inside = c(0.1, 0.1),
    plot.title = element_text(
      hjust = 0.5,
      size = 22,
      margin = margin(5, 0, 5, 0)
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      margin = margin(2.5, 0, 0, 0)
    ),
    plot.margin = margin(0, 0, 0, 0)
  )

analf_city <- analf_city |>
  mutate(is_nordeste = factor(if_else(code_region == 2, 1L, 0L)))

tbl_summary <- analf_city |>
  st_drop_geometry() |>
  summarise(avg = mean(analf_rate), .by = "is_nordeste")

p2 <- ggplot() +
  geom_density(
    data = analf_city,
    aes(x = analf_rate, fill = is_nordeste),
    alpha = 0.6
  ) +
  geom_vline(
    data = tbl_summary,
    aes(xintercept = avg, color = is_nordeste),
    lty = 2
  ) +
  geom_hline(yintercept = 0) +
  guides(fill = "none", color = "none") +
  scale_x_continuous(breaks = seq(0, 35, 5)) +
  scale_y_continuous(expand = expansion(add = c(0, .01))) +
  scale_fill_manual(values = c("#01665e", "#8c510a")) +
  scale_color_manual(values = c("#01665e", "#8c510a")) +
  labs(
    title = "Illiteracy rate is nearly 3x higher in the <b><span style='color:#8c510a'>Northeast</span></b> than in the rest of <b><span style='color:#01665e'>Brazil</span></b>.",
    subtitle = "Dashed lines indicate average rates.",
    x = "Illiteracy rate (%)",
    y = NULL
  ) +
  theme_minimal(base_family = "Roboto Slab") +
  theme(
    plot.background = element_rect(fill = offwhite, color = offwhite),
    panel.background = element_rect(fill = offwhite, color = offwhite),
    panel.grid.minor = element_blank(),
    plot.margin = margin(10, 2.5, 5, 2.5),
    plot.title = element_textbox_simple(
      size = 16,
      padding = margin(5.5, 5.5, 5.5, 5.5),
      margin = margin(5.5, 0, 5.5, 0),
      family = "Gill Sans"
    ),
    plot.subtitle = element_textbox_simple(
      size = 8,
      padding = margin(5.5, 5.5, 5.5, 5.5),
      margin = margin(0, 0, 5.5, 0)
    )
  )

label_1 <- "Em 97,4% das cidades do <b><span style='color:#8c510a'>Nordeste</span></b>, as pessoas do <i>sexo feminino têm taxas de alfabetização menores</i>. Em alguns casos, a diferença supera 10 pontos percentuais."

label_2 <- "Na maior parte das cidades do <b><span style='color:#01665e'>Brasil</span></b>, não há diferença grande na taxa de alfabetização entre pessoas do sexo masculino e do sexo feminino."

label_1 <- "In 97.4% of cities in the <b><span style='color:#8c510a'>Northeast</span></b>, <i>females have lower literacy rates</i>. In some cases, the gap exceeds 10 percentage points."

label_2 <- "In most cities across <b><span style='color:#01665e'>Brazil</span></b>, there is little difference in literacy rates between males and females."


text_labels <- tibble(
  x = c(-10, 7.5), y = c(0.2, 0.25), label = c(label_1, label_2)
)

p3 <- ggplot() +
  geom_density(
    data = analf_gender,
    aes(x = gender_gap, fill = is_nordeste),
    alpha = 0.7
  ) +
  geom_textbox(
    data = text_labels,
    aes(x, y, label = label),
    family = "Gill Sans",
    size = 3
  ) +
  scale_x_continuous(breaks = seq(-15, 15, 5)) +
  scale_y_continuous(expand = expansion(add = c(0, .05))) +
  scale_fill_manual(values = c("#01665e", "#8c510a")) +
  guides(fill = "none") +
  labs(
    title = "Gender Inequality in Literacy",
    subtitle = "Difference in percentage points between male and female literacy rates. Values close to zero indicate small differences; negative values indicate that women have lower literacy rates than men.",
    caption = "Source: IBGE (2022 Census). @viniciusoike",
    y = NULL,
    x = "Gender gap"
  ) +
  theme_minimal(base_family = "Roboto Slab") +
  theme(
    plot.background = element_rect(fill = offwhite, color = offwhite),
    panel.background = element_rect(fill = offwhite, color = offwhite),
    panel.grid.minor = element_blank(),
    plot.title = element_textbox_simple(
      size = 16,
      padding = margin(5.5, 5.5, 5.5, 5.5),
      margin = margin(0, 0, 5.5, 0),
      family = "Gill Sans"
    ),
    plot.subtitle = element_textbox_simple(
      size = 8,
      padding = margin(5.5, 5.5, 5.5, 5.5),
      margin = margin(0, 0, 5.5, 0)
    )
  )

panel <- p1 | (p2 / p3)
panel <- panel + plot_layout(widths = c(0.55, 0.45))

ggsave(here::here("plots/9_diverging.png"), panel, width = 13.5, height = 10)

ggsave(here::here("plots/9_diverging_map.png"), p1, width = 8, height = 6)
cowplot::save_plot(here::here("plots/9_diverging_hist_1.png"), p2, base_width = 5, base_height = 4)
cowplot::save_plot(here::here("plots/9_diverging_hist_2.png"), p3, base_width = 5, base_height = 4)
