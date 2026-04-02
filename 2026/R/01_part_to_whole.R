# Prompt: Comparisons
# Part to Whole

import::from(sidrar, get_sidra)

library(tidyverse)
library(sf)
library(patchwork)

states <- geobr::read_state(showProgress = FALSE)
dim_state <- as_tibble(st_drop_geometry(states))
dim_region <- distinct(dim_state, code_region, name_region)

dat <- get_sidra(10332, variable = 13377, geo = "State")

cols_select <- c(
  "code_state" = "unidade_da_federacao_codigo",
  "mode" = "meio_de_transporte_em_que_passa_mais_tempo_para_chegar_ao_local_de_trabalho",
  "race" = "cor_ou_raca",
  "educ" = "nivel_de_instrucao",
  "value" = "valor"
)

dat <- as_tibble(dat)

tab <- dat |>
  janitor::clean_names() |>
  select(all_of(cols_select)) |>
  mutate(
    code_state = as.numeric(code_state)
  )

dim_mode <- tibble(
  mode = c(
    "A pé",
    "Automóvel",
    "BRT ou ônibus de trânsito rápido",
    "Bicicleta",
    "Caminhonete ou caminhão adaptado (pau de arara)",
    "Embarcação de médio e grande porte (acima de 20 pessoas)",
    "Embarcação de pequeno porte (até 20 pessoas)",
    "Motocicleta",
    "Mototáxi",
    "Outros",
    "Trem ou metrô",
    "Táxi ou assemelhados",
    "Van, perua ou assemelhados",
    "Ônibus"
  ),
  type = case_when(
    mode %in% c("A pé", "Bicicleta") ~ "Active",
    str_detect(mode, "Embarcação") ~ "Public transit",
    mode %in%
      c(
        "BRT ou ônibus de trânsito rápido",
        "Ônibus",
        "Trem ou metrô",
        "Van, perua ou assemelhados"
      ) ~ "Public transit",
    mode %in%
      c(
        "Automóvel",
        "Motocicleta",
        "Mototáxi",
        "Táxi ou assemelhados"
      ) ~ "Car or motorcycle",
    TRUE ~ NA_character_
  ),
  group = case_when(
    mode %in% c("A pé", "Bicicleta") ~ "Active",
    str_detect(mode, "Embarcação") ~ "Public transit",
    mode %in%
      c(
        "BRT ou ônibus de trânsito rápido",
        "Ônibus",
        "Trem ou metrô",
        "Van, perua ou assemelhados"
      ) ~ "Public transit",
    mode %in% c("Automóvel", "Táxi ou assemelhados") ~ "Car",
    mode %in% c("Motocicleta", "Mototáxi") ~ "Motorcycle",
    TRUE ~ "Other"
  )
)

label_percent_br <- function(x) {
  scales::number(
    x,
    accuracy = 1,
    scale = 1,
    suffix = "%",
    big.mark = ".",
    decimal.mark = ","
  )
}

lvls_modes <- c("Other", "Motorcycle", "Active", "Public transit", "Car")

tab_modes <- tab |>
  filter(race == "Total", educ == "Total", mode != "Total") |>
  left_join(dim_mode, by = "mode")

tab_brazil <- tab_modes |>
  summarise(
    trips = sum(value, na.rm = TRUE),
    .by = "group"
  ) |>
  mutate(
    share = trips / sum(trips) * 100,
    group = factor(
      group,
      levels = lvls_modes
    ),
    label_num = case_when(
      share > 5 ~ label_percent_br(share),
      TRUE ~ ""
    )
  )

tab_region <- tab_modes |>
  mutate(code_region = as.numeric(str_sub(code_state, 1, 1))) |>
  summarise(
    trips = sum(value, na.rm = TRUE),
    .by = c("code_region", "group")
  ) |>
  mutate(
    share = trips / sum(trips) * 100,
    group = factor(group, levels = lvls_modes),
    .by = "code_region"
  )

tab_region <- left_join(tab_region, dim_region, by = "code_region")

tab_region <- tab_region |>
  mutate(
    name_region = case_when(
      code_region == 1 ~ "North",
      code_region == 2 ~ "Northeast",
      code_region == 3 ~ "Southeast",
      code_region == 4 ~ "South",
      code_region == 5 ~ "Mid-West",
    ),
    name_region = fct_reorder(name_region, -code_region),
    label_num = case_when(
      share > 5 ~ label_percent_br(share),
      TRUE ~ ""
    )
  )

tab_state <- tab_modes |>
  summarise(
    trips = sum(value, na.rm = TRUE),
    .by = c("code_state", "group")
  ) |>
  mutate(
    share = trips / sum(trips) * 100,
    code_region = as.numeric(str_sub(code_state, 1, 1)),
    group = factor(group, levels = lvls_modes),
    .by = c("code_state")
  )

fake_grid <- expand_grid(
  code_state = c(
    seq(11, 19, 1),
    seq(21, 29, 1),
    seq(31, 39, 1),
    seq(41, 49, 1),
    seq(51, 59, 1)
  ),
  group = unique(tab_brazil$group)
)

tab_full_state <- fake_grid |>
  mutate(code_region = as.numeric(str_sub(code_state, 1, 1))) |>
  left_join(select(tab_state, -code_region), by = c("code_state", "group"))

# fmt: skip
fake_codes <- c(18, 19, 34, 36, 37, 38, 39, 44, 45, 46, 47, 48, 49, 54, 55, 56, 57, 58, 59)

tab_full_state <- left_join(
  tab_full_state,
  dim_state,
  by = c("code_state", "code_region")
)

tab_full_state <- tab_full_state |>
  mutate(
    abbrev_state = case_when(
      code_state == 18 ~ "",
      code_state == 19 ~ " ",
      code_state == 34 ~ "",
      code_state == 36 ~ " ",
      code_state == 37 ~ "  ",
      code_state == 38 ~ "   ",
      code_state == 39 ~ "    ",
      code_state == 44 ~ "",
      code_state == 45 ~ " ",
      code_state == 46 ~ "  ",
      code_state == 47 ~ "   ",
      code_state == 48 ~ "    ",
      code_state == 49 ~ "     ",
      code_state == 54 ~ "",
      code_state == 55 ~ " ",
      code_state == 56 ~ "  ",
      code_state == 57 ~ "   ",
      code_state == 58 ~ "    ",
      code_state == 59 ~ "     ",
      TRUE ~ abbrev_state
    ),
    share = case_when(
      code_state %in% fake_codes ~ 0.0001,
      TRUE ~ share
    )
  )

base_text <- "Lato"
title_text <- "Lora"

#  [1] "#6D2F20" "#843A2C" "#9B4538" "#B35144" "#C15E4F" "#CE6B58" "#DA7962"
#  [8] "#DF8261" "#DF895A" "#DF8F54" "#E29B56" "#E6AB62" "#EABC6D" "#E3C578"
# [15] "#C7BF82" "#ABB98B" "#8EAF91" "#6A8D80" "#466C6F" "#224B5E"

colors_modes <- c(
  #"Active" = "#94b594",
  "Active" = "#8EAF91",
  "Car" = "#9B4538",
  "Motorcycle" = "#EABC6D",
  "Public transit" = "#466C6F",
  "Other" = "#999999"
)

offwhite <- "#f5f5dc"


theme_plot <- theme_minimal(base_family = base_text) +
  theme_sub_panel(
    grid.major = element_blank(),
    grid.minor = element_blank(),
    background = element_rect(fill = offwhite, color = offwhite)
  ) +
  theme_sub_plot(
    title = element_text(family = title_text, size = 14, hjust = 0.5)
  )

p_brazil <- ggplot(tab_brazil, aes(x = 1, y = share, fill = group)) +
  geom_col() +
  geom_text(
    aes(x = 1, y = share, label = label_num, group = group),
    inherit.aes = FALSE,
    position = position_stack(vjust = 0.5),
    family = base_text,
    size = 4,
    color = "#000000"
  ) +
  scale_x_continuous(expand = expansion(0.05)) +
  scale_y_continuous(expand = expansion(c(0, 0.05))) +
  scale_fill_manual(values = colors_modes) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(
    title = "Brazil",
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  theme_plot +
  theme_sub_axis(text = element_blank()) +
  theme_sub_axis_x(line = element_line()) +
  theme_sub_plot(
    background = element_rect(fill = offwhite, color = offwhite)
  )

p_region <- ggplot(tab_region, aes(share, name_region, fill = group)) +
  geom_col(width = 0.45) +
  geom_text(
    data = subset(tab_region, group == "Active"),
    aes(x = 0, y = name_region, label = name_region),
    hjust = 0,
    size = 4,
    family = title_text,
    color = "#000000",
    nudge_y = 0.35
  ) +
  geom_text(
    aes(share, name_region, label = label_num, group = group),
    inherit.aes = FALSE,
    position = position_stack(vjust = 0.5),
    family = base_text,
    size = 3.5
  ) +
  scale_x_continuous(expand = c(0), breaks = c(0, 100)) +
  scale_y_discrete(expand = expansion(c(0.1, 0.2))) +
  scale_fill_manual(values = colors_modes) +
  guides(fill = guide_legend(reverse = TRUE)) +
  labs(
    title = "Regions",
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  theme_plot +
  theme_sub_plot(
    margin = margin(0, 8, 0, 8),
    background = element_rect(fill = offwhite)
  ) +
  theme_sub_axis(text = element_blank()) +
  theme_sub_axis_x(line = element_line(), ticks = element_line())

region_agg <- tab_region |>
  summarise(total = sum(trips), .by = "code_region") |>
  mutate(
    label_num = scales::number(
      total,
      scale = 1e-6,
      suffix = "M",
      decimal.mark = ","
    ),
  ) |>
  arrange(code_region)

region_agg <- region_agg |>
  mutate(
    y = c(1.4, 2.31, 3.15, 4, 4.85)
  )

p_region_total <- ggplot(region_agg, aes(x = 1, y = y)) +
  geom_point(
    aes(size = log(total)),
    fill = "#224B5E",
    color = "#ffffff",
    shape = 21
  ) +
  geom_text(
    aes(label = label_num),
    family = base_text,
    size = c(2, 4, 6, 4, 2.5),
    color = "#ffffff"
  ) +
  scale_size_continuous(range = c(5, 30)) +
  scale_y_reverse(expand = expansion(c(0.05)), limits = c(1, 5)) +
  labs(title = "Total Trips") +
  guides(size = "none") +
  theme_plot +
  theme_sub_panel(
    background = element_rect(fill = offwhite, color = offwhite)
  ) +
  theme_sub_axis(text = element_blank(), title = element_blank()) +
  theme_sub_axis_x(line = element_line()) +
  theme_sub_plot(
    background = element_rect(fill = offwhite),
    title = element_text(family = title_text, size = 14, hjust = 0.5),
    margin = margin(0, 8, 0, 0)
  )

panel <- p_brazil +
  p_region +
  p_region_total +
  plot_layout(widths = c(0.1, 0.75, 0.15), guides = "collect") +
  plot_annotation(
    title = "Getting around in Brazil",
    subtitle = "Main transport mode when commuting to work or school by regions in Brazil (2022).",
    caption = "Source: IBGE (Census, 2022) - when multiple modes are used, considers the 'main' mode. @viniciusoike\n'Car' includes cabs (and similar shared taxis, e.g., Uber); 'Motorcycle' includes mototaxis; 'Active' includes walking and cycling;\n 'Public transit' includes buses, trains/metros, shared boats, and informal public transportation (e.g. vans).",
    theme = theme(
      plot.background = element_rect(fill = offwhite, color = offwhite),
      panel.background = element_rect(fill = offwhite, color = offwhite),
      plot.title = element_text(
        family = title_text,
        size = 16,
        hjust = 0,
        margin = margin(10, 0, 0, 0)
      ),
      plot.subtitle = element_text(
        family = title_text,
        size = 10,
        hjust = 0,
        color = "gray20",
        margin = margin(0, 0, 10, 0)
      ),
      plot.caption = element_text(
        family = base_text,
        size = 6,
        hjust = 0,
        color = "gray50"
      ),
    )
  ) &
  theme(legend.position = "bottom")

ggsave(
  here::here("2026/plots/01_part_to_whole.png"),
  panel,
  width = 9,
  height = 5,
  dpi = 400
)


plot_state <- function(region) {
  subdat <- tab_full_state |>
    filter(code_region == !!region) |>
    mutate(
      label_num = case_when(
        share > 20 ~ label_percent_br(share),
        TRUE ~ NA_character_
      )
    )

  states_abb_order <- subdat |>
    filter(group == "Car") |>
    arrange(desc(share)) |>
    pull(abbrev_state)

  subdat <- subdat |>
    mutate(abbrev_state = factor(abbrev_state, levels = states_abb_order))

  ggplot(
    subdat,
    aes(abbrev_state, share, fill = group)
  ) +
    geom_col(width = 0.55) +
    geom_label(
      aes(x = abbrev_state, y = share, label = label_num, group = group),
      inherit.aes = FALSE,
      position = position_stack(vjust = 0.5),
      family = base_text,
      size = 2.5,
      fill = "white",
      color = "#000000"
    ) +
    scale_y_continuous(
      breaks = seq(0, 100, 25),
      expand = expansion(c(0, 0.1)),
      position = "right"
    ) +
    scale_fill_manual(values = colors_modes) +
    labs(
      title = NULL,
      x = NULL,
      y = NULL,
      fill = NULL
    ) +
    theme_minimal(base_family = base_text) +
    theme_sub_axis_x(line = element_line()) +
    theme_sub_panel(
      grid.minor = element_blank(),
      grid.major.x = element_blank()
    )
}

p_states <- map(1:5, plot_state)

p_brazil +
  p_region +
  plot_layout(widths = c(0.2, 0.8), guides = "collect") &
  theme(legend.position = "bottom")

library(patchwork)


p2 +
  (p_states[[1]] /
    p_states[[2]] /
    p_states[[3]] /
    p_states[[4]] /
    p_states[[5]]) +
  plot_layout(guides = "collect")


subdat <- tab_full_state |>
  filter(code_region == 1)


states_abb_order <- subdat |>
  filter(group == "Car") |>
  arrange(desc(share)) |>
  pull(abbrev_state)

subdat <- subdat |>
  mutate(abbrev_state = factor(abbrev_state, levels = states_abb_order))

ggplot(
  subdat,
  aes(abbrev_state, share, fill = group)
) +
  geom_col(width = 0.5) +
  scale_y_continuous(
    breaks = seq(0, 100, 20),
    expand = expansion(c(0, 0.05))
  ) +
  scale_fill_manual(values = colors_modes) +
  labs(
    title = "Brazil",
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  theme_minimal(base_family = base_text) +
  theme_sub_axis_x(line = element_line()) +
  theme_sub_panel(
    grid.minor = element_blank(),
    grid.major.x = element_blank()
  )

plot_state(1)


p2 <- ggplot(tab_region, aes(share, 1, fill = group)) +
  geom_col(orientation = "y") +
  facet_wrap(vars(code_region), ncol = 1) +
  scale_y_continuous(expand = expansion(0.35)) +
  scale_fill_manual(values = colors_modes) +
  labs(
    title = "Brazil",
    x = NULL,
    y = NULL,
    fill = NULL
  ) +
  theme_plot


xbreaks <- unique(fake_grid$code_state)

swap_names <- c(states$abbrev_state)
names(swap_names) <- states$code_state
xlabels <- str_replace_all(xbreaks, swap_names)
xlabels <- str_replace_all(xlabels, "[0-9]{2}", "")

ggplot(subset(tab_full_state, code_region == 1)) +
  geom_col()

p3 = ggplot(tab_full_state, aes(as.factor(code_state), share, fill = group)) +
  geom_col() +
  facet_wrap(vars(code_region), ncol = 1, scales = "free") +
  scale_x_discrete(labels = xlabels) +
  scale_fill_manual(values = colors_modes) +
  theme_minimal() +
  theme_sub_panel(
    grid.minor = element_blank(),
    grid.major = element_blank()
  ) +
  theme_sub_axis(
    title = element_blank()
  )

library(patchwork)

panel <- p1 | p2 | p3

(p1 | p2 | p3) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")


# tab_grouped_educ <- tab |>
#   filter(
#     mode != "Total",
#     race != "Total",
#     educ != "Total",
#     race %in% c("Branca", "Parda")
#   ) |>
#   mutate(
#     group_educ = case_when(
#       educ == "Sem instrução e fundamental incompleto" ~ "No Education",
#       educ == "Superior completo" ~ "College Degree",
#       educ %in%
#         c(
#           "Fundamental completo e médio incompleto",
#           "Médio completo e superior incompleto"
#         ) ~ "High School"
#     )
#   ) |>
#   summarise(
#     n = sum(value, na.rm = TRUE),
#     .by = c("code_state", "mode", "race", "group_educ")
#   )

# tab_grouped <- tab_grouped_educ |>
#   left_join(dim_mode, by = "mode") |>
#   summarise(
#     trips = sum(n, na.rm = TRUE),
#     .by = c("code_state", "group", "race", "group_educ")
#   ) |>
#   mutate(code_region = as.factor(str_sub(code_state, 1, 1))) |>
#   summarise(
#     trips_region = sum(trips, na.rm = TRUE),
#     .by = c("code_region", "group", "race", "group_educ")
#   ) |>
#   mutate(share = trips_region / sum(trips_region) * 100, .by = "code_region")

# tab_alluvial <- tab |>
#   filter(
#     mode != "Total",
#     race != "Total",
#     educ != "Total",
#     race %in% c("Branca", "Parda")
#   ) |>
#   left_join(dim_mode, by = "mode") |>
#   summarise(
#     n = sum(value, na.rm = TRUE),
#     .by = c("code_state", "type", "race", "educ")
#   ) |>
#   mutate(
#     share = n / sum(n, na.rm = TRUE) * 100,
#     .by = c("code_state")
#   )

# library(ggalluvial)
# ggplot(
#   subset(tab_grouped, code_region == 3),
#   aes(y = share, axis1 = race, axis2 = group_educ)
# ) +
#   geom_alluvium(aes(fill = group)) +
#   geom_stratum(width = 1 / 12, fill = "black", color = "grey") +
#   geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
#   theme_minimal()
# is_alluvia_form(tab_alluvial, axes = c(3, 4, 6))

# ggplot(
#   subset(tab_alluvial, code_state == 35),
#   aes(y = share, axis1 = race, axis2 = educ)
# ) +
#   geom_alluvium(aes(fill = type)) +
#   geom_stratum(width = 1 / 12, fill = "black", color = "grey") +
#   geom_label(stat = "stratum", aes(label = after_stat(stratum))) +
#   theme_minimal()

mode_state <- tab |>
  filter(
    mode != "Total",
    race != "Total",
    educ != "Total",
    race %in% c("Branca", "Parda")
  ) |>
  left_join(dim_mode, by = "mode") |>
  summarise(
    n = sum(value, na.rm = TRUE),
    .by = c("code_state", "group")
  ) |>
  mutate(
    share = n / sum(n, na.rm = TRUE) * 100,
    .by = c("code_state")
  )

grouped_mode_state <- mode_state |>
  group_by(code_state) |>
  mutate(group_label = fct_lump_n(group, n = 3, w = share)) |>
  group_by(code_state, group_label) |>
  summarise(grouped_share = sum(share)) |>
  arrange(code_state, desc(grouped_share)) |>
  ungroup()

df <- grouped_mode_state |>
  filter(code_state == 35) |>
  mutate(
    group_label = fct_reorder(group_label, grouped_share)
  )

# ggplot(df, aes(grouped_share, 1, fill = group_label)) +
#   geom_col(orientation = "y") +
#   scale_y_continuous(expand = expansion(0.35)) +
#   scale_fill_manual(values = colors_modes) +
#   theme_void() +
#   theme(
#     legend.position = "none"
#   )

# ggplot(states) +
#   geom_sf()

# ggplot(
#   subset(grouped_mode_state, code_state == 11),
#   aes(x = 1, y = grouped_share, fill = group_label)
# ) +
#   geom_col() +
#   coord_flip()

# tab_wide <- mode_state |>
#   mutate(
#     code_brazil = 1,
#     code_region = as.integer(str_sub(code_state, 1, 1))
#   ) |>
#   mutate(across(starts_with("code"), as.factor)) |>
#   mutate(share = n / sum(n, na.rm = TRUE) * 100)

# ggplot(
#   tab_wide,
#   aes(y = share, axis2 = code_region, axis3 = code_state)
# ) +
#   geom_alluvium(aes(fill = group)) +
#   geom_stratum(width = 1 / 12, fill = "black", color = "grey") +
#   facet_wrap(vars(code_region))

# is_alluvia_form(tab_wide, axes = c(1, 4, 5, 6))
