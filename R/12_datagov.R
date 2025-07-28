library(dplyr)
library(patchwork)
library(ggplot2)
library(ragg)
import::from(stringr, str_extract)
import::from(readr, read_csv)

dat <- read_csv(
  "//Volumes/T7 Touch/bases-de-dados/real-estate/Real_Estate_Sales_2001-2022_GL.csv"
)

subdat <- dat |>
  janitor::clean_names() |>
  filter(!is.na(location)) |>
  mutate(
    lng = as.numeric(str_extract(location, "(?<=-)[0-9].+(?= )")),
    lng = -lng,
    lat = as.numeric(str_extract(location, "(?<=[0-9] )[0-9].+(?=\\))"))
  )

subdat <- subdat |>
  filter(
    !(property_type %in% c("Commercial", "Industrial", "Vacant Land"))
  ) |>
  filter(!is.na(property_type)) |>
  filter(sale_amount > 25000, sale_amount < 1e7)

main_cities <- subdat |>
  count(town, sort = TRUE) |>
  head(5) |>
  pull(town)

main_cities <- c(main_cities, "Hartford")

subdat |>
  count(property_type, residential_type)

subdat <- subdat |>
  mutate(
    house_type = case_when(
      property_type == "Apartments" ~ "Apartment",
      TRUE ~ residential_type
    ),
    property_type = "Residential",
    residential_type = if_else(
      is.na(residential_type),
      "Apartments",
      residential_type
    )
  )

subdat <- subdat |>
  filter(town %in% main_cities) |>
  mutate(
    date = as.Date(date_recorded, format = "%m/%d/%Y"),
    year = lubridate::year(date),
    month = lubridate::month(date)
  )

# subdat |>
#   filter(town == "Waterbury") |>
#   summarise(
#     avg = mean(sale_amount),
#     med = median(sale_amount),
#     .by = c("year", "property_type", "residential_type")
#   ) |>
#   View()

series <- subdat |>
  filter(house_type %in% c("Single Family", "Two Family", "Condo")) |>
  summarise(
    count = n(),
    avg = mean(sale_amount),
    med = median(sale_amount),
    .by = c("year", "month", "town")
  ) |>
  mutate(date = lubridate::make_date(year, month, 1))

min(series$year)
max(series$year)

series <- series |>
  arrange(date) |>
  arrange(town)

series <- series |>
  mutate(
    adjusted_price = priceR::adjust_for_inflation(
      med,
      from_date = date,
      to_date = as.Date("2022-01-01"),
      country = "US"
    )
  )

main_year_breaks <- c(2007, seq(2010, 2020, 5), 2023)
year_breaks <- 2007:2023
dbreaks <- lubridate::make_date(year_breaks)

offwhite <- "#fefefe"
pal_col_axis <- c(offwhite, "#000000")
col_axis_inds <- as.integer(year_breaks %in% main_year_breaks) + 1
col_axis <- pal_col_axis[col_axis_inds]
size_axis <- c(0, 12)[col_axis_inds]

pal_colors <- MetBrewer::met.brewer("Hokusai1", n = 6)
pal_colors[4] <- MetBrewer::met.brewer("Hokusai1", n = 24)[5]
pal_colors[2] <- MetBrewer::met.brewer("Hokusai1", n = 24)[15]

font_text <- "DIN Alternate"
font_title <- "Rockwell"

base_plot <- ggplot(series, aes(date, adjusted_price / 1e3, color = town)) +
  geom_point(alpha = 0.15, shape = 21, aes(fill = town)) +
  geom_smooth(method = "gam", se = FALSE) +
  geom_hline(yintercept = 0, lwd = 1) +
  geom_label(
    data = filter(series, date == max(date), .by = "town"),
    aes(x = date, y = adjusted_price / 1e3, label = town),
    size = 3,
    family = font_text,
    hjust = 0,
    nudge_x = 100,
    nudge_y = c(0, 0, -40, 40, 0, 0)
  ) +
  guides(color = "none", fill = "none") +
  scale_x_date(
    breaks = dbreaks,
    date_label = "%Y",
    expand = expansion(c(0.025, 0.2))
  ) +
  scale_y_continuous(
    breaks = seq(0, 700, by = 100),
    limits = c(NA, 700),
    expand = expansion(c(0, 0.05))
  ) +
  scale_color_manual(name = "", values = pal_colors) +
  scale_fill_manual(name = "", values = pal_colors) +
  labs(x = NULL, y = "Median House Price ($ thous.)") +
  theme_minimal(base_family = font_text) +
  theme()


theme_plot <- theme_minimal(base_size = 10, base_family = font_text) +
  theme(
    panel.background = element_rect(fill = offwhite, color = offwhite),
    plot.background = element_rect(fill = offwhite, color = offwhite),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(size = size_axis),
    axis.ticks.x = element_line(color = "gray20")
  )

theme_annotation <- theme(
  plot.title = element_text(
    size = 18,
    color = "#000000",
    family = font_title
  ),
  plot.subtitle = element_text(size = 11, color = "gray30"),
  plot.caption = element_text(hjust = 0, color = "gray40")
)

base_plot +
  plot_annotation(
    title = "House Prices in Connecticut",
    subtitle = "Median monthly house sale price in constant 2022 USD (thousands) of main cities in Conneticut.\nSample inclues only Single and Two Family Homes, and Condo units in the 25k-10M range.",
    caption = "Source: Real Estate Sales 2001-2022 GL (data.gov). Smooth line is a GAM trend.\nNote: data has missing values.",
    theme = theme_annotation
  )
