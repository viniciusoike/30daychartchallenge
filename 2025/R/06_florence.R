### OBS: this was the first draft of this prompt
# I thought it would be interesting to see the drop in both
# industrial and commercial usage in 2020 while residential would
# continue to grow. Turns out the plot is very uninteresting.

library(ggplot2)
library(patchwork)
library(dplyr)
import::from(here, here)
import::from(rbcb, get_series)
import::from(tidyr, pivot_longer)

codes <- c("commercial" = 1402, "residential" = 1403, "industrial" = 1404)

dat <- rbcb::get_series(codes, as = "tibble")

energy <- purrr::reduce(dat, ~ left_join(.x, .y, by = "date"))

series <- energy |>
  pivot_longer(cols = -date, names_to = "source", values_to = "consumption")

sub <- series |>
  filter(date >= as.Date("2020-01-01"), date <= as.Date("2020-12-01")) |>
  mutate(
    month = lubridate::month(date, label = TRUE, abbr = TRUE),
    source = factor(
      source,
      levels = c("commercial", "residential", "industrial")
    ),
    lcons = log(consumption)
  )


# Define appropriate colors for electricity sources
electricity_colors <- c(
  "residential" = "#E8B64C",
  "commercial" = "#4E79A7",
  "industrial" = "#A15A58"
)

# Create Nightingale rose diagram with authentic Victorian styling
base_plot <- ggplot(sub, aes(x = month, y = consumption, fill = source)) +
  # Use thick black borders as in original diagrams
  geom_col(
    width = 1,
    position = "stack",
    color = "black",
    linewidth = 0.5,
    alpha = 0.85
  ) +
  # Use a coordinate system similar to Nightingale's original
  coord_polar(clip = "off") +
  # Use thematic colors for electricity sources
  scale_fill_manual(
    name = "CLASSIFICATION",
    values = electricity_colors,
    labels = c(
      "residential" = "Residential",
      "commercial" = "Commercial",
      "industrial" = "Industrial"
    )
  ) +
  # Create a period-appropriate theme
  theme(
    # Set background to mimic aged paper
    panel.background = element_rect(fill = "#F8F3E6", color = "#F8F3E6"),
    plot.background = element_rect(fill = "#F8F3E6", color = "#F8F3E6"),

    # Use period-appropriate typography
    text = element_text(family = "Copperplate", color = "#2B2522"),

    # Grid lines similar to hand-drawn style of the era
    panel.grid.major = element_line(
      color = "#BDB5A5",
      linewidth = 0.3,
      linetype = "dotted"
    ),
    panel.grid.minor = element_blank(),

    # Axis styling reminiscent of period diagrams
    axis.text.x = element_text(
      family = "Copperplate",
      size = 10,
      face = "bold"
    ),
    axis.text.y = element_text(family = "Copperplate", size = 9),
    axis.title = element_blank(),

    # Legend styling
    legend.background = element_rect(fill = "#F8F3E6", color = "#6D5C41"),
    legend.title = element_text(
      family = "Baskerville",
      face = "bold",
      size = 10
    ),
    legend.text = element_text(family = "Copperplate", size = 9),
    legend.position = "bottom",
    legend.key.size = unit(0.8, "cm"),

    plot.margin = margin(t = 10, r = 5, b = 10, l = 5)
  )

theme_annotation <- theme(
  panel.background = element_rect(fill = "#F8F3E6", color = "#F8F3E6"),
  plot.background = element_rect(fill = "#F8F3E6", color = "#F8F3E6"),
  plot.title = element_text(
    family = "Copperplate",
    face = "bold",
    color = "gray10",
    size = 14,
    hjust = 0.5,
    margin = margin(b = 5)
  ),
  plot.subtitle = element_text(
    family = "Charter",
    size = 8,
    hjust = 0.5,
    margin = margin(b = 5)
  ),
  plot.caption = element_text(
    family = "Charter",
    size = 8,
    hjust = 0,
    margin = margin(t = 5)
  ),
)


final_plot <- base_plot +
  plot_annotation(
    title = "DIAGRAM OF THE COMSUMPTION\nOF ELECTRIC POWER",
    subtitle = "THE COMPARATIVE CONSUMPTION OF ELECTRICITY\nACROSS THE TWELVE MONTHS OF 2020 IN BRAZIL",
    caption = "SOURCE: ELETROBRAS.",
    theme = theme_annotation
  )

ggsave(
  here("plots/6_florence_electricity.png"),
  final_plot,
  width = 8,
  height = 8 / 1.618
)
