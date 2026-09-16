library(ggplot2)
library(dplyr)
library(insperplot)


modos <- c("Transporte Público Coletivo", "Veículos Privados", "Modos Ativos")
portes <- c(
  "Pequeno\n(Até 500 mil hab.)",
  "Médio\n(501 mil a 1 milhão)",
  "Grande\n(1M a 4M hab.)",
  "Extragrande\n(Mais de 4M hab.)"
)

dat <- tibble(
  modo = rep(modos, each = length(portes)),
  porte = rep(portes, length(modos))
)

dat <- dat |>
  mutate(
    modo = factor(modo, levels = rev(modos)),
    porte = factor(porte, levels = portes)
  )

vls <- c(
  15.6,
  32.9,
  35.2,
  51.1,
  52.8,
  48.9,
  45.2,
  34.3,
  31.1,
  18.4,
  19.5,
  14.6
)

dat$value <- vls

insper_cols <- insper_palette()
insper_cols_muted <- insper_palette("muted")
insper_grays <- insper_palette("grays")

theme_plot <- theme_insper(base_size = 8) +
  theme_sub_plot(margin = margin(15, 15, 15, 15)) +
  theme_sub_panel(
    grid.major = element_blank(),
    background = element_rect(color = "#0E171D")
  ) +
  theme_sub_axis(text = element_text(size = 6)) +
  theme_sub_axis_y(text = element_text(hjust = 0.5)) +
  theme_sub_axis_x(text = element_blank()) +
  theme_sub_strip(
    text = element_text(
      family = "Acumin Pro ExtraCondensed Bold",
      size = 10,
      color = "#FFFFFF",
      margin = margin(5, 0, 5, 0)
    ),
    background = element_rect(fill = "#0E171D")
  )

grafico <- ggplot(dat, aes(value, modo, fill = modo)) +
  geom_col(width = 0.6) +
  geom_vline(xintercept = 0) +
  geom_text(
    aes(label = scales::percent(value, scale = 1)),
    size = 2,
    family = "Inter 18pt",
    nudge_x = 10
  ) +
  facet_wrap(vars(porte), nrow = 1, labeller = ) +
  scale_x_continuous(
    labels = scales::label_number(suffix = "%"),
    expand = expansion(c(0, 0.15))
  ) +
  scale_y_discrete(labels = \(x) stringr::str_wrap(x, 10)) +
  scale_fill_manual(
    name = NULL,
    values = insper_cols[c(3, 2, 1)]
    # values = c(insper_grays[c(3, 3)], insper_cols[1])
  ) +
  labs(
    title = "Distribuição Modal por Porte de Cidade",
    subtitle = "Percentual de viagens por modo principal nas cidades brasileiras, classificadas por porte populacional",
    x = NULL,
    y = NULL,
    caption = "Fonte: IBGE (Censo 2022)"
  ) +
  guides(fill = "none") +
  theme_plot


ggsave(
  "grafico.png",
  grafico,
  width = 14,
  height = 8,
  dpi = 400,
  units = "cm"
)

show_insper_palettes()
