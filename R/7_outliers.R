library(ggplot2)
library(dplyr)
library(ragg)
library(ggdist)
library(ggdist)
import::from(here, here)
import::from(stringr, str_wrap)
import::from(sidrar, get_sidra)
import::from(MetBrewer, met.brewer)


# Data --------------------------------------------------------------------

dim_city <- as_tibble(sf::st_drop_geometry(geobr::read_municipality(year = 2021)))

gdp <- get_sidra(5938, variable = 37, period = "2021", geo = "City")
pop <- get_sidra(6579, variable = 9324, period = "2021", geo = "City")

gdp_sidra <- gdp |>
  janitor::clean_names() |>
  as_tibble() |>
  select(
    code_muni = municipio_codigo,
    gdp = valor
  ) |>
  mutate(code_muni = as.numeric(code_muni))

pop_sidra <- pop |>
  janitor::clean_names() |>
  as_tibble() |>
  select(
    code_muni = municipio_codigo,
    pop = valor
  ) |>
  mutate(code_muni = as.numeric(code_muni))

dat <- inner_join(pop_sidra, gdp_sidra, by = "code_muni")

dat <- dat |>
  mutate(gdppc = gdp / pop * 1000) |>
  left_join(dim_city, by = "code_muni")

subdat <- dat |>
  filter(pop > 5*1e4) |>
  mutate(lgdp = log10(gdppc))

# Plot --------------------------------------------------------------------

# fill color
col1 <- met.brewer("Hokusai2", n = 12)[8]
# line color
col2 <- met.brewer("Hokusai2", n = 12)[11]

text_avg <- str_wrap("Lages (SC), Alegrete (RS), and Belo Horizonte (MG) are all close to the average R$ 42,5k", 25)
text_lowest <- str_wrap("Vargem Grande (MA) has lowest GDP per capita: R$ 7,5k.", 21)
text_highest <- str_wrap("Maricá (RJ) has the highest GDP per capita: R$ 551,9k, almost 74 times greater than the lowest value on the list", 21)

# Maricá 511,9
# Vargem Grande 7500

df_label <- tibble(
  x = c(5.95, 3.7, 5.175),
  y = c(0.36, 0.19, 0.52),
  label = c(text_highest, text_lowest, text_avg)
)

x_label <- c(10000, 25000, 50000, 100000, 200000, 300000, 500000)
x_label_fm <- format(x_label, big.mark = ".")

p2 <- ggplot(subdat, aes(x = log10(gdppc))) +
  geom_hline(yintercept = 0) +
  geom_label(
    data = df_label,
    aes(x, y, label = label),
    family = "Futura",
    size = 3.5
  ) +
  geom_curve(
    data = data.frame(x = log10(500000), xend = 5.875, y = 0.01, yend = 0.15),
    aes(x, y, xend = xend, yend = yend),
    arrow = arrow(length = unit("5", "pt")),
    curvature = 0.45
  ) +
  geom_curve(
    data = data.frame(x = log10(7500), xend = 3.75, y = 0.01, yend = 0.09),
    aes(x, y, xend = xend, yend = yend),
    arrow = arrow(length = unit("5", "pt")),
    curvature = -0.45
  ) +
  geom_curve(
    data = data.frame(x = log10(mean(subdat$gdppc)), xend = 4.92, y = 0.63, yend = 0.58),
    aes(x + 0.015, y, xend = xend, yend = yend),
    arrow = arrow(length = unit("5", "pt")),
    curvature = -0.3
  ) +
  geom_dotplot(binwidth = 0.03, fill = col1, color = col2, method = "histodot") +
  scale_x_continuous(
    breaks = log10(x_label),
    labels = x_label_fm,
    limits = c(3.5, 6.05)) +
  scale_y_continuous(NULL, breaks = NULL) +
  labs(
    title = "Wealth distribution across Brazilian cities",
    subtitle = "GDP per capita across Brazilian cities with over 50.000 inhabitants. Each dot represents, roughly, 2 cities.",
    x = "GDP per capita (R$, log scale)",
    y = NULL
  ) +
  theme_minimal(base_family = "Futura") +
  theme(
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = offwhite, color = offwhite),
    plot.background = element_rect(fill = offwhite, color = offwhite),
    panel.grid.major.x = element_line(color = "gray45", linetype = 2),
    plot.margin = margin(10, 10, 10, 10, unit = "pt"),
    plot.title = element_text(size = 22)
  )

offwhite <- "#f5f5f5"

p1 <- ggplot() +
  geom_boxplot(
    data = subdat,
    aes(x = 1, y = lgdp),
    fill = col1,
    size = 0.5,
    alpha = 0.7,
    outlier.shape = 21,
    outlier.colour = "white",
    outlier.fill = col2,
    outlier.alpha = 1,
    outlier.size = 3) +
  geom_jitter(
    data = sample_n(dplyr::filter(subdat, lgdp < 5), 100),
    aes(x = 1, y = lgdp),
    shape = 21,
    color = "white",
    fill = col1,
    size = 2,
    alpha = 0.7,
    height = 0.3,
    width = 0.3) +
  coord_flip() +
  geom_hline(yintercept = log10(x_label), color = c("gray45", "white", "white", rep("gray45", 4)), linetype = 2) +
  scale_y_continuous(
    breaks = x_label,
    labels = x_label_fm,
    limits = c(3.5, 6.05)) +
  theme_minimal(base_family = "Futura") +
  theme(
    panel.grid = element_blank(),
    # panel.grid.major.y = element_blank(),
    # panel.grid.major.x = element_line(color = "gray35"),
    panel.grid.minor = element_blank(),
    panel.background = element_rect(fill = offwhite, color = offwhite),
    plot.background = element_rect(fill = offwhite, color = offwhite),
    # plot.margin = margin(15, 5, 15, 5, unit = "pt"),
    axis.title = element_blank(),
    axis.text = element_blank()
  )


library(patchwork)

panel <- (p2 / p1)

final_plot <- panel + plot_layout(heights = c(0.8, 0.2))
final_plot

ggsave(here("plots/7_outlier.png"), final_plot, width = 7.32, height = 6.64, dpi = 300)

cowplot::save_plot(here("plots/8_outlier.png"), final_plot, base_width = 8, base_height = 5.5)
