library(maddison)
library(dplyr)
library(ggplot2)
library(tidyr)
library(countrycode)

dat <- maddison |>
  filter(
    year >= 1900,
    # Russia with a weird countrycode
    countrycode != "SUN"
  ) |>
  mutate(
    gdp = gdppc * pop,
    region_wb = countrycode(country, "country.name", "region")
  )

og_series <- dat |>
  filter(year >= 1900, !is.na(gdp))

grid <- expand_grid(
  year = unique(og_series$year),
  country = unique(og_series$country)
)

dim_countries <- og_series |>
  filter(year >= 2018) |>
  distinct(iso3c, country, region, continent, region_wb)

full_series <- grid |>
  left_join(dim_countries, by = "country") |>
  left_join(select(og_series, year, country, gdp), by = c("year", "country")) |>
  mutate(year = lubridate::make_date(year, 1, 1))

interpolate_kalman <- function(df) {
  # browser()
  # Find first non-NA value
  date_start <- min(subset(df, !is.na(gdp))$year, na.rm = TRUE)

  gap_lengths <- function(x) {
    r <- rle(is.na(x))
    r$lengths[r$values] # lengths of each NA run
  }

  active <- df |>
    filter(year >= date_start) |>
    select(year, gdp) |>
    arrange(year)

  if (length(gap_lengths(active$gdp)) == 0) {
    interp_vals <- active$gdp
    out <- tibble(
      year = df$year,
      interpolated = df$gdp,
      gdp = df$gdp
    )
    return(out)
  }
  xseries <- trendseries::df_to_ts(
    active,
    date_col = "year",
    value_col = "gdp",
    frequency = 1
  )

  xseries <- log(xseries)
  interp <- tryCatch(
    imputeTS::na_kalman(xseries),
    error = function(e) imputeTS::na_interpolation(xseries, option = "linear")
  )
  interp_vals <- as.numeric(exp(interp))

  interp_active <- tibble(
    year = active$year,
    interpolated = interp_vals,
    gdp = active$gdp
  )

  pre <- df |>
    filter(year < date_start) |>
    select(year, gdp) |>
    mutate(interpolated = NA_real_) |>
    arrange(year)

  out <- bind_rows(pre, interp_active)

  return(out)
}

interpolated_series <- full_series |>
  group_by(country, continent, region, region_wb) |>
  nest() |>
  mutate(interpolated = purrr::map(data, interpolate_kalman)) |>
  unnest(cols = interpolated) |>
  ungroup()

series <- interpolated_series |>
  mutate(
    region_wb = countrycode(country, "country.name", "region"),
    region = case_when(
      continent == "Americas" ~ "Americas",
      region_wb == "Europe & Central Asia" ~ "Europe & Central Asia",
      region_wb == "East Asia & Pacific" ~ "East Asia & Pacific",
      region_wb == "South Asia" ~ "South Asia",
      continent == "Africa" |
        region_wb == "Middle East & North Africa" ~ "Middle East & Africa",
      TRUE ~ "other"
    )
  )

final <- series |>
  mutate(
    region_wb = countrycode(country, "country.name", "region"),
    region = if_else(
      country %in% c("China", "United States", "Germany", "India"),
      country,
      region
    )
  ) |>
  summarise(
    total = sum(interpolated, na.rm = TRUE),
    .by = c("year", "region")
  ) |>
  mutate(
    share = total / sum(total, na.rm = TRUE),
    .by = "year"
  )

sub_region_levels <- c(
  "East Asia & Pacific",
  "China",
  "Americas",
  "United States",
  "Europe & Central Asia",
  "Germany",
  "South Asia",
  "India",
  "Middle East & Africa"
)

final_df <- final |>
  filter(year >= "1950-01-01") |>
  mutate(
    region = factor(region, levels = sub_region_levels)
  )

pos_i <- c(2, 4, 6, 8)
pos_color <- c(1, 2, 3, 4)
color_palette <- ekioplot::ekio_pal("contrast")[1:5]

exp_color_palette <- color_palette[c(1, 1, 2, 2, 3, 3, 4, 4, 5)]
new_color_palette <- colorspace::darken(exp_color_palette, 0.35)
new_color_palette[pos_i] <- exp_color_palette[pos_i]

alpha_vals <- c(0.7, 0.9, 0.7, 0.9, 0.7, 0.9, 0.7, 0.9, 0.7)

main_color <- "#1B3A4B"
offwhite <- "#f8fbf8"
font_text <- "Roboto Slab"

theme_plot <- theme_minimal(base_family = font_text) +
  theme_sub_plot(
    title = element_text(size = 16, family = "Georgia"),
    subtitle = element_text(size = 10, color = "gray40"),
    caption = element_text(size = 8, color = "gray60"),
    margin = margin(15, 10, 10, 10),
    background = element_rect(fill = offwhite, color = offwhite)
  ) +
  theme_sub_panel(
    grid.minor = element_blank(),
    grid.major.x = element_blank(),
    background = element_rect(fill = offwhite, color = offwhite)
  ) +
  theme_sub_axis_x(
    ticks = element_line(color = "gray20"),
    line = element_line(color = "gray20", linewidth = 0.5),
    text = element_text(color = "gray20")
  )

final_plot <- ggplot(
  final_df,
  aes(year, share, group = region, alpha = region)
) +
  geom_area(aes(fill = region), color = "white", lwd = 0.5) +
  geom_text(
    data = subset(final_df, year == "1952-01-01"),
    aes(x = year, y = share, label = region),
    family = "Lato",
    position = position_stack(vjust = 0.5),
    hjust = 0,
    size = c(2.5, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5, 3.5),
    show.legend = FALSE,
  ) +
  geom_label(
    data = subset(final_df, year == max(year)),
    aes(
      x = as.Date("2022-01-01"),
      y = share,
      label = scales::number(share, accuracy = 0.1, scale = 100, suffix = "%")
    ),
    family = "Lato",
    position = position_stack(vjust = 0.5),
    hjust = 1,
    size = 3,
    show.legend = FALSE
  ) +
  scale_fill_manual(values = new_color_palette) +
  scale_alpha_manual(values = alpha_vals) +
  scale_x_date(
    breaks = seq(as.Date("1950-01-01"), as.Date("2020-01-01"), by = "10 years"),
    date_labels = "%Y",
    expand = expansion(c(0, 0.1))
  ) +
  scale_y_continuous(
    breaks = seq(0, 1, 0.2),
    labels = scales::label_percent(),
    expand = expansion(0),
    position = "left"
  ) +
  labs(
    title = "The Share of Global Wealth",
    subtitle = "Share of World GDP (%) per year by region, highlighting the biggest country in certain regions.",
    caption = "Source: Maddison Project • @viniciusoike",
    x = NULL,
    y = NULL
  ) +
  guides(fill = "none", alpha = "none") +
  theme_plot

ggsave(
  "2026/plots/09_wealth.png",
  final_plot,
  width = 8,
  height = 5,
  dpi = 400
)
