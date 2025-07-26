library(ggplot2)
library(dplyr)

# Your dataset (replace this with your actual data)
data <- tibble(
  ano = c(
    2019,
    2020,
    2021,
    2022,
    2023,
    2024,
    2019,
    2020,
    2021,
    2022,
    2023,
    2024,
    2019,
    2020,
    2021,
    2022,
    2023,
    2024,
    2020,
    2021,
    2022,
    2023,
    2024
  ),
  category = c(
    rep("none_rising", 6),
    rep("rent_rising", 6),
    rep("sale_rising", 6),
    rep("both_rising", 5)
  ),
  n = c(
    33,
    23,
    31,
    8,
    4,
    3,
    15,
    9,
    5,
    9,
    8,
    6,
    2,
    15,
    9,
    13,
    17,
    18,
    3,
    5,
    26,
    27,
    29
  )
)

# Function to create waffle data from your dataset
create_waffle_from_data <- function(data) {
  # Create waffle data for each year
  waffle_data <- data %>%
    group_by(ano) %>%
    arrange(factor(
      category,
      levels = c("none_rising", "rent_rising", "sale_rising", "both_rising")
    )) %>%
    mutate(
      end_pos = cumsum(n),
      start_pos = lag(end_pos, default = 0) + 1
    ) %>%
    rowwise() %>%
    do({
      year <- .$ano
      cat <- .$category
      start <- .$start_pos
      end <- .$end_pos

      # Create tiles for this category
      tiles <- start:end
      x_coords <- (tiles - 1) %% 10
      y_coords <- 9 - floor((tiles - 1) / 10) # Flip y to match waffle orientation

      data.frame(
        year = year,
        x = x_coords,
        y = y_coords,
        category = cat
      )
    }) %>%
    ungroup()

  return(waffle_data)
}

# Generate waffle data
waffle_data <- create_waffle_from_data(subdat)

# Define colors to match your plot
category_colors <- c("#2a9d8f", "#ee9b00", "#e9c46a", "#9b2226")

offwhite <- "#fefefe"

waffle_data %>%
  mutate(
    category_label = case_when(
      category == "none_rising" ~ "None rising above inflation",
      category == "rent_rising" ~ "Rent rising above inflation",
      category == "sale_rising" ~ "Sale rising above inflation",
      category == "both_rising" ~ "Both rising above inflation"
    )
  )

# Create the plot
base_plot <- ggplot(waffle_data, aes(x = x, y = y, fill = category)) +
  geom_tile(color = "white", size = 0.8, linewidth = 0.25) +
  scale_fill_manual(values = category_colors) +
  facet_wrap(~year, ncol = 3) +
  coord_equal() +
  scale_x_continuous(
    breaks = seq(0, 10, 2.5),
    labels = seq(0.0, 10.0, 2.5),
    expand = c(0.05, 0.05)
  ) +
  scale_y_continuous(breaks = seq(0, 30, 10), expand = c(0.05, 0.05)) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.title = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    plot.background = element_rect(fill = offwhite, color = offwhite),
    panel.background = element_rect(fill = offwhite, color = offwhite),
    strip.background = element_rect(
      fill = "gray90",
      color = "black",
      size = 0.5
    ),
    strip.text = element_text(
      size = 14,
      face = "bold",
      margin = margin(5, 5, 5, 5)
    ),
    axis.text = element_blank(),
    panel.spacing = unit(0.8, "lines"),
    plot.margin = margin(10, 10, 10, 10)
  )


# If you want to see the data summary for verification
cat("Data summary by year and category:\n")
data %>%
  group_by(ano, category) %>%
  summarise(total = sum(n), .groups = "drop") %>%
  arrange(ano, category) %>%
  print()
