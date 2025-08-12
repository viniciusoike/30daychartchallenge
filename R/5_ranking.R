library(dplyr)
library(tidyr)
library(ggplot2)
library(ggbump)
library(ragg)
import::from(stringr, str_wrap)
import::from(here, here)

dat <- readr::read_csv(here("data/day_5/ips_brasil_municipios.csv"))

dict <- tibble(
  original_names = names(dat),
  col_names = janitor::make_clean_names(original_names)
)

inds <- c(
  "indice_de_progresso_social",
  "pib_per_capita_2021",
  "agua_e_saneamento",
  "moradia",
  "seguranca_pessoal",
  "saude_e_bem_estar",
  "nota_media_no_enem",
  "empregados_com_ensino_superior"
  )

labels <- c(
  "Social Progress Index",
  "GDP per capita",
  "Water and Sanitation",
  "Housing Conditions",
  "Safety",
  "Healthcare and Wellbeing",
  "Avg. ENEM scores(*)",
  "Share College Educ."
)

subdat <- dat |>
  janitor::clean_names() |>
  select(
    codigo_ibge, municipio, uf, populacao_2022, all_of(inds)
  )

ranked <- subdat |>
  slice_max(populacao_2022, n = 25) |>
  mutate(
    across(all_of(inds), ~rank(-.x))
  )

codes <- c(3550308, 5300108, 3304557, 1501402, 4314902, 2304400, 2611606)

ranking <- ranked |>
  pivot_longer(all_of(inds), names_to = "measure", values_to = "rank") |>
  mutate(
    measure = factor(measure, levels = inds),
    highlight = if_else(codigo_ibge %in% codes, municipio, ""),
    is_highlight = factor(if_else(codigo_ibge %in% codes, 1L, 0L)),
    rank_labels = if_else(rank %in% c(1, 5, 10, 15, 20, 25), rank, NA),
    is_highlight = factor(ifelse(codigo_ibge %in% codes, 1L, 0L)),
    rank_labels = if_else(rank %in% c(1, 5, 10, 15, 20, 25), paste0(rank, "°"), NA)
  )

ranking |>
  select(measure, rank, municipio, highlight, is_highlight)


cores <- c(
  "#c7c7c7", "#101010",  "#225d9f", "#f7443e", "#2a9d8f", "#386641", "#fb8500",
  "#f781bf"
  )

p <- ggplot(ranking, aes(measure, rank, group = municipio)) +
  geom_bump(aes(color = highlight, linewidth = is_highlight, alpha = is_highlight)) +
  geom_point(aes(fill = highlight), shape = 21, color = "white", size = 3) +
  geom_text(
    data = filter(ranking, is_highlight == 1, measure == inds[length(inds)]),
    aes(label = municipio),
    hjust = 0,
    nudge_x = 0.1,
    fontface = "bold",
    family = "Helvetica"
  ) +
  geom_text(
    data = filter(ranking, !is.na(rank_labels), measure == inds[1]),
    aes(label = rank_labels),
    hjust = 0,
    nudge_x = -0.3,
    fontface = "bold",
    family = "Helvetica"
  ) +
  scale_y_reverse() +
  scale_x_discrete(
    labels = str_wrap(labels, 11),
    position = "top",
    expand = expansion(add = c(0.7, 2))
  ) +
  scale_color_manual(values = cores) +
  scale_fill_manual(values = cores) +
  scale_linewidth_manual(values = c(0.5, 1.2)) +
  scale_alpha_manual(values = c(0.5, 1)) +
  labs(
    x = NULL,
    y = NULL,
    title = "Ranking Brazilian Cities",
    subtitle = "Ranking of the 25 most populous Brazilian cities based on economic, infrastructure, health, safety, and education indicators.",
    caption = "Source: IPS Brasil (2024)\n(*) - ENEM is a national standardized test for college admission."
  ) +
  theme_minimal(base_family = "Helvetica Neue") +
  theme(
    plot.title = element_text(size = 16, color = "black"),
    plot.subtitle = element_text(color = "gray25"),
    plot.caption = element_text(hjust = 0, color = "gray25"),
    axis.text.y = element_blank(),
    axis.text.x = element_text(color = "gray10", size = 12),
    panel.background = element_rect(fill = "#fefefe", color = "#fefefe"),
    plot.background = element_rect(fill = "#fefefe", color = "#fefefe"),
    panel.grid = element_blank(),
    legend.position = "none",
  )

ggsave(here("plots/5_ranking.png"), p, width = 12.5, height = 6.5)
