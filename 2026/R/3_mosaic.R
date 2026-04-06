library(maddison)
library(dplyr)
library(ggplot2)
library(MetBrewer)

dat <- maddison::maddison

max(dat$year)

subdat <- dat |>
  filter(year >= 1920) |>
  mutate(
    gdp = gdppc * pop,
    chg_gdp = (gdp / lag(gdp) - 1) * 100,
    chg_gdppc = (gdppc / lag(gdppc) - 1) * 100,
    is_growth = factor(if_else(chg_gdp > 0, 1L, 0L)),
    .by = "countrycode"
  )

subdat <- subdat |>
  mutate(
    trunc_chg_gdp = case_when(
      chg_gdp < -5 ~ -5,
      chg_gdp > 5 ~ 5,
      TRUE ~ chg_gdp
    ),
    trunc_chg_gdppc = case_when(
      chg_gdppc < -5 ~ -5,
      chg_gdppc > 5 ~ 5,
      TRUE ~ chg_gdppc
    )
  )

# Manual check of countries
subdat |>
  filter(year == max(year)) |>
  arrange(desc(gdp)) |>
  mutate(
    acumgdp = cumsum(gdp),
    share = acumgdp / sum(gdp),
    rank = rank(-gdp)
  ) |>
  filter(share > 0.95)

top_economies <- subdat |>
  filter(year == max(year)) |>
  slice_max(gdp, n = 65) |>
  pull(countrycode)

dat_mosaic <- subdat |>
  filter(countrycode %in% top_economies, year >= 1921) |>
  arrange(continent, desc(countrycode))

lvls_countrycode <- dat_mosaic |>
  distinct(countrycode, continent) |>
  arrange(continent, desc(countrycode)) |>
  pull(countrycode)

dat_mosaic <- dat_mosaic |>
  mutate(
    countrycode = factor(countrycode, levels = lvls_countrycode)
  )


n_na <- sum(is.na(dat_mosaic$chg_gdppc))
n <- length(dat_mosaic$chg_gdppc)

# Generate some variation in the size of each tile
noise_y <- rnorm(n, 0.75, 0.075)
noise_x <- rnorm(n, 0.75, 0.075)
# Generate some variation in the colour missing value tiles
colors_na <- paste0("gray", sample(50:70, size = n_na, replace = TRUE))

offwhite <- "#f8fbf8"

mosaic <- ggplot(dat_mosaic, aes(year, countrycode)) +
  geom_tile(
    aes(fill = trunc_chg_gdppc),
    height = noise_y,
    width = noise_x
  ) +
  facet_grid(
    rows = vars(continent),
    scales = "free",
    space = "free"
  ) +
  scale_fill_gradientn(
    name = "GDP per capita (YoY %)",
    breaks = seq(-5, 5, 1),
    colors = met.brewer("Hiroshige"),
    na.value = colors_na
  ) +
  scale_x_continuous(
    breaks = c(1921, seq(1930, 2010, 10), 2022),
    expand = c(0, 0),
    sec.axis = dup_axis()
  ) +
  labs(
    title = "Over a Century of Booms and Busts",
    subtitle = "Year-on-year GDP per capita change for countries representing ~95% of the world economy in 2022. Values are truncated to +/- 5% to facilitate visualization.",
    caption = "Source: Maddison Project Database. @viniciusoike"
  ) +
  theme_minimal(base_family = "Lato") +
  theme_sub_plot(
    title = element_text(size = 18, family = "Georgia"),
    subtitle = element_text(size = 10),
    caption = element_text(size = 6, hjust = 0),
    background = element_rect(fill = offwhite, color = offwhite),
    title.position = "plot",
    margin = margin(t = 15, r = 10, b = 15, l = 10),
  ) +
  theme_sub_panel(
    background = element_rect(fill = offwhite, color = offwhite),
    grid.major = element_blank(),
    grid.minor = element_blank()
  ) +
  theme_sub_strip(
    background = element_rect(fill = offwhite, color = offwhite),
    text = element_text(size = 10, family = "Georgia")
  ) +
  theme_sub_legend(
    title = element_text(size = 8, family = "Lato"),
    title.position = "top",
    justification = "left",
    background = element_rect(fill = offwhite, color = offwhite),
    position = "top",
    key.width = unit(1.5, "cm"),
    key.height = unit(0.5, "cm")
  ) +
  theme_sub_axis(
    title = element_blank()
  ) +
  theme_sub_axis_y(
    text = element_text(size = 6, color = "gray20", family = "Fira Code")
  ) +
  theme_sub_axis_x(
    text = element_text(size = 11, color = "gray20", family = "Georgia")
  )

ggsave(
  here::here("2026", "plots", "03_mosaic.png"),
  mosaic,
  width = 10,
  height = 8,
  dpi = 400
)
