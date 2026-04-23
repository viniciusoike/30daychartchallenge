library(readxl)
library(dplyr)
library(here)
import::from(sidrar, get_sidra)

frota00 <- read_excel(here("data/day_2/Frota Tipo-UF 2000.xls"),
                      range = "A7:C33",
                      col_names = c("name_state", "total", "cars")
                      )

frota10 <- read_excel(
  here("data/day_2/frota_2010/Frota_Tipo_Placa/Frota Regi‰es Tipo UF dez2010.xls"),
  sheet = 2,
  range = "A5:C36",
  col_names = c("name_state", "total", "cars")
)

frota25 <- read_excel(
  here("data/day_2/Frota_por_UF_Tipo_de_veiculo_Fevereiro_2025.xlsx"),
  range = "A5:C36",
  col_names = c("name_state", "total", "cars")
  )

dim_state <- geobr::read_state(year = 2010)
dim_state <- as_tibble(sf::st_drop_geometry(dim_state))
dim_state <- dim_state |>
  mutate(name_state = ifelse(name_state == "Espirito Santo", "Espírito Santo", name_state))


frota00 <- frota00 |>
  mutate(
    name_state = stringr::str_to_title(name_state)) |>
  left_join(dim_state, by = "name_state") |>
  filter(!is.na(code_state))

frota10 <- frota10 |>
  mutate(
    name_state = stringr::str_to_title(name_state)) |>
  left_join(dim_state, by = "name_state") |>
  filter(!is.na(code_state))

frota25 <- frota25 |>
  mutate(
    name_state = stringr::str_to_title(name_state)) |>
  left_join(dim_state, by = "name_state") |>
  filter(!is.na(code_state))

pop22 <- get_sidra(
  4709,
  93,
  geo = "State"
)

pop00 <- get_sidra(
  136,
  93,
  period = "2000-2010",
  geo = "State",
  classific = "c86",
  category = list(c(0))
)

pop22 <- pop22 |>
  janitor::clean_names() |>
  as_tibble() |>
  select(
    code_state = unidade_da_federacao_codigo,
    pop = valor
  ) |>
  mutate(code_state = as.numeric(code_state))

pop00 <- pop00 |>
  janitor::clean_names() |>
  as_tibble() |>
  select(
    code_state = unidade_da_federacao_codigo,
    year = ano_codigo,
    pop = valor
  ) |>
  mutate(code_state = as.numeric(code_state))

pop22 <- pop22 |>
  mutate(year = 2025)

population <- pop00 |>
  mutate(year = as.numeric(year)) |>
  bind_rows(pop22)

frota <- bind_rows(
  list(`2000` = frota00, `2010` = frota10, `2025` = frota25),
  .id = "year"
  )

frota <- mutate(frota, year = as.numeric(year))

dat <- left_join(frota, population, by = c("code_state", "year"))

wide_dat <- dat |>
  mutate(
    year = as.numeric(year),
    ratio_car = cars / pop,
    ratio_vehicles = total / pop) |>
  tidyr::pivot_wider(
    id_cols = c("code_state", "abbrev_state"),
    names_from = "year",
    values_from = c("ratio_car", "cars")
  ) |>
  mutate(
    ratio = ratio_car_2025 / ratio_car_2000
  )

library(ggplot2)
library(ggrepel)

ggplot(wide_dat, aes(x = log(cars_2000), ratio)) +
  geom_point() +
  geom_text_repel(aes(label = abbrev_state)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_abline(slope = 1)

ggplot(wide_dat, aes(x = ratio_car_2000, ratio_car_2025)) +
  geom_point() +
  geom_text_repel(aes(label = abbrev_state)) +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  geom_abline(slope = 1)

dat <- dat |>
  mutate(ratio_car = cars / pop * 1000)

cars_br <- dat |>
  summarise(
    total_cars = sum(cars),
    total_pop = sum(pop),
    .by = "year"
  ) |>
  mutate(
    ratio_car = total_cars / total_pop * 1000,
    code_state = 1)

highlight_uf <- c(21, 35, 29, 53, 51, 31, 41)

dat <- dat |>
  mutate(
    is_highlight = factor(ifelse(code_state %in% highlight_uf, 1L, 0L)),
    highlight_state = ifelse(is_highlight == 1, abbrev_state, NA)
  )


plot_line <- ggplot(dat, aes(x = year, y = ratio_car, group = code_state)) +
  geom_hline(yintercept = 0) +
  geom_line(
    data = filter(dat, is_highlight == 0),
    color = "gray50",
    linewidth = 0.6,
    alpha = 0.5
  ) +
  geom_line(
    data = filter(dat, is_highlight == 1),
    aes(color = highlight_state),
    linewidth = 1
  ) +
  geom_point(
    data = filter(dat, is_highlight == 0),
    color = "gray50",
    size = 1
  ) +
  geom_point(
    data = filter(dat, is_highlight == 1),
    aes(color = highlight_state),
    size = 2) +
  geom_label(
    data = filter(dat, is_highlight == 1, year == 2025),
    aes(x = year + 1, y = ratio_car, label = abbrev_state, color = abbrev_state),
    nudge_y = c(0, 0, 0, 10, -10, 0, 0)
  ) +
  scale_x_continuous(breaks = c(2000, 2010, 2025)) +
  scale_color_manual(values = MetBrewer::met.brewer("Hokusai1")) +
  guides(color = "none") +
  labs(
    x = NULL,
    y = "Cars per 1.000 persons",
    title = "The Rise of Cars in Brazil",
    subtitle = "Number of private automobiles per 1.000 inhabitantes in each Brazilian state.",
    caption = "Sources: SENATRAN and IBGE (Census)."
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 14),
    plot.background = element_rect(fill = "#f5f5f5", color = "#f5f5f5"),
    panel.background = element_rect(fill = "#f5f5f5", color = "#f5f5f5"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linetype = 2),
    axis.text = element_text(size = 12)
  )

ggsave(here("plots/2_slope.png"), plot_line, width = 8, height = 8/1.618)
