library(ggplot2)
library(dplyr)
import::from(rbcb, get_series)
import::from(tidyr, pivot_longer)

codes <- c("commercial" = 1402, "residential" = 1403, "industrial" = 1404)

dat <- rbcb::get_series(codes, as = "tibble")

energy <- purrr::reduce(dat, ~left_join(.x, .y, by = "date"))

series <- energy |>
  pivot_longer(cols = -date, names_to = "source", values_to = "consumption")

sub <- series |>
  filter(date >= as.Date("2020-01-01"), date <= as.Date("2020-12-01")) |>
  mutate(
    month = lubridate::month(date, label = TRUE, abbr = TRUE),
    source = factor(source, levels = c("commercial", "residential", "industrial")),
    lcons = log(consumption)
  )



# Define appropriate colors for electricity sources
electricity_colors <- c(
  "residential" = "#E8B64C",  # Warm yellow/gold for homes
  "commercial" = "#4E79A7",   # Blue for business
  "industrial" = "#A15A58"    # Reddish brown for industry
)

# Create Nightingale rose diagram with authentic Victorian styling
ggplot(sub, aes(x = month, y = consumption, fill = source)) +
  # Use thick black borders as in original diagrams
  geom_col(width = 1, position = "stack", color = "black", linewidth = 0.5, alpha = 0.85) +
  # Use a coordinate system similar to Nightingale's original
  coord_polar(clip = "off") +

  # Use thematic colors for electricity sources
  scale_fill_manual(values = electricity_colors,
                    labels = c(
                      "residential" = "Residential Usage",
                      "commercial" = "Commercial Usage",
                      "industrial" = "Industrial Usage"
                    )) +

  # Create Victorian-style labeling
  labs(
    title = "DIAGRAM OF THE DISTRIBUTION OF ELECTRIC POWER",
    subtitle = "THE COMPARATIVE CONSUMPTION OF ELECTRICITY\nACROSS THE TWELVE MONTHS OF 2020 IN BRAZIL",
    caption = "The areas of the coloured segments are proportional to the quantities they represent.\nConstructed on the principles established by Florence Nightingale (1820-1910).",
    fill = "CLASSIFICATION OF USAGE"
  ) +

  # Create a period-appropriate theme
  theme(
    # Set background to mimic aged paper
    panel.background = element_rect(fill = "#F8F3E6"),
    plot.background = element_rect(fill = "#F8F3E6", color = "#6D5C41", linewidth = 1.5),

    # Use period-appropriate typography
    text = element_text(family = "Didot", color = "#2B2522"),
    plot.title = element_text(family = "Baskerville", face = "bold", size = 16,
                              hjust = 0.5, margin = margin(t = 20, b = 10)),
    plot.subtitle = element_text(family = "Baskerville", size = 12, hjust = 0.5,
                                 lineheight = 1.2, margin = margin(b = 20)),
    plot.caption = element_text(family = "Baskerville", size = 9, hjust = 0.5,
                                margin = margin(t = 15, b = 10)),

    # Grid lines similar to hand-drawn style of the era
    panel.grid.major = element_line(color = "#BDB5A5", linewidth = 0.3, linetype = "dotted"),
    panel.grid.minor = element_blank(),

    # Axis styling reminiscent of period diagrams
    axis.text.x = element_text(family = "Didot", size = 10, face = "bold"),
    axis.text.y = element_text(family = "Didot", size = 9),
    axis.title = element_blank(),

    # Legend styling
    legend.background = element_rect(fill = "#F8F3E6", color = "#6D5C41"),
    legend.title = element_text(family = "Baskerville", face = "bold", size = 10),
    legend.text = element_text(family = "Didot", size = 9),
    legend.position = "bottom",
    legend.key.size = unit(0.8, "cm"),

    # Add Victorian-style margin around entire plot
    plot.margin = margin(t = 20, r = 45, b = 20, l = 20)
  )


# library(ggplot2)
# library(dplyr)
# library(ragg)
# library(ggtext)
#
# weather <- readr::read_csv("data/day_6/porto_alegre.csv")
#
# poa <- weather |>
#   mutate(data_ano = lubridate::year(data_ymd)) |>
#   select(
#     data_ymd, data_ano, data_mes, hora_utc, temp_bulbo_hora, precipitacao_total_mm
#   )
#
# tbl <- poa |>
#   summarise(
#     avg_temp = mean(temp_bulbo_hora, na.rm = TRUE),
#     total_rain = sum(precipitacao_total_mm, na.rm = TRUE),
#     .by = c("data_ano", "data_mes")
#   )
#
# tbl <- tbl |>
#   mutate(
#     month_abb = lubridate::month(lubridate::make_date(data_ano, data_mes, 1), label = TRUE, abbr = TRUE)
#   )
#
# # Add custom Victorian-style fonts
# font_add("Baskerville",
#          regular = "/System/Library/Fonts/Baskerville.ttc",
#          bold = "/System/Library/Fonts/Baskerville.ttc")
# font_add("Didot",
#          regular = "/System/Library/Fonts/Didot.ttc",
#          bold = "/System/Library/Fonts/Didot.ttc")
#
#
# # Create more detailed monthly weather data
# weather_data <- data.frame(
#   month = factor(month.abb, levels = month.abb),
#   rainfall = c(120, 100, 80, 60, 40, 20, 10, 15, 30, 60, 90, 110),
#   temperature = c(5, 6, 8, 12, 16, 20, 22, 21, 18, 14, 9, 6)
# )
#
# # Convert to long format for proper Nightingale-style display
# weather_long <- tidyr::pivot_longer(
#   weather_data,
#   cols = c(rainfall, temperature),
#   names_to = "measurement",
#   values_to = "value"
# )
#
# # Scale temperature to make it visible alongside rainfall
# weather_long <- weather_long %>%
#   mutate(
#     value = ifelse(measurement == "temperature", value * 5, value),
#     measurement = ifelse(measurement == "temperature", "Temperature (°C) × 5", "Rainfall (mm)")
#   )
#
# ggplot(tbl, aes(x = data_mes, y = ))
#
# # Create Nightingale rose diagram with authentic Victorian styling
# p <- ggplot(weather_long, aes(x = month, y = value, fill = measurement)) +
#   # Use thick black borders as in original diagrams
#   geom_col(width = 1, position = "identity", color = "black", size = 0.5, alpha = 0.8) +
#
#   # Use a coordinate system similar to Nightingale's original
#   coord_polar() +
#
#   # Use colors reminiscent of original diagrams
#   scale_fill_manual(values = c("Rainfall (mm)" = "#B3D7ED",
#                                "Temperature (°C) × 5" = "#EFA586")) +
#
#   # Create Victorian-style labeling
#   labs(
#     title = "DIAGRAM OF THE CAUSES OF METEOROLOGICAL PHENOMENA",
#     subtitle = "REPRESENTING THE RELATIVE PROPORTIONS OF RAINFALL & TEMPERATURE\nIN THE TWELVE MONTHS OF THE YEAR",
#     caption = "The areas of the coloured segments are proportional to the quantities they represent.\nConstructed on the principles established by Florence Nightingale (1820-1910).",
#     fill = "CLASSIFICATION"
#   ) +
#
#   # Create a period-appropriate theme
#   theme(
#     # Set background to mimic aged paper
#     panel.background = element_rect(fill = "#F8F3E6"),
#     plot.background = element_rect(fill = "#F8F3E6", color = "#6D5C41", size = 1.5),
#
#     # Use period-appropriate typography
#     text = element_text(family = "Didot", color = "#2B2522"),
#     plot.title = element_text(family = "Baskerville", face = "bold", size = 16,
#                               hjust = 0.5, margin = margin(t = 20, b = 10)),
#     plot.subtitle = element_text(family = "Baskerville", size = 12, hjust = 0.5,
#                                  lineheight = 1.2, margin = margin(b = 20)),
#     plot.caption = element_text(family = "Baskerville", size = 9, hjust = 0.5,
#                                 margin = margin(t = 15, b = 10)),
#
#     # Grid lines similar to hand-drawn style of the era
#     panel.grid.major = element_line(color = "#BDB5A5", size = 0.3, linetype = "dotted"),
#     panel.grid.minor = element_blank(),
#
#     # Axis styling reminiscent of period diagrams
#     axis.text.x = element_text(family = "Didot", size = 10, face = "bold"),
#     axis.text.y = element_text(family = "Didot", size = 9),
#     axis.title = element_blank(),
#
#     # Legend styling
#     legend.background = element_rect(fill = "#F8F3E6", color = "#6D5C41"),
#     legend.title = element_text(family = "Baskerville", face = "bold", size = 10),
#     legend.text = element_text(family = "Didot", size = 9),
#     legend.position = "bottom",
#     legend.key.size = unit(0.8, "cm"),
#
#     # Add Victorian-style margin around entire plot
#     plot.margin = margin(t = 20, r = 20, b = 20, l = 20)
#   )
#
# # Display the diagram
# # For proper rendering of custom fonts, use ragg device
# ragg::agg_png("nightingale_weather_diagram.png", width = 1800, height = 1800, res = 144)
# print(p)
# invisible(dev.off())

# Also display in R
# print(p)


# library(microdatasus)
# library(dplyr)
#
# mortalidade <-
#   fetch_datasus(year_start = 2022,
#                 year_end = 2022,
#                 information_system = "SIM-DO",
#   )
#
#
# head(mortalidade_RN)
