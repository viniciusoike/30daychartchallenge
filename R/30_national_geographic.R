library(ggplot2)
library(dplyr)
library(showtext)
library(ggtext)
library(sf)
library(rvest)

import::from(zoo, rollmean)
import::from(geobr, read_state)
import::from(tidyr, pivot_longer)


# Define font
# Candidates:
# PT SANS
# JOST
# REEM KUFI
sysfonts::font_add_google("Jost", "Jost")
showtext_auto()
font <- "Jost"

# Data --------------------------------------------------------------------

# Scrape the data from INPE
url <- "http://www.obt.inpe.br/OBT/assuntos/programas/amazonia/prodes"

# Parss site and get table
html_prodes <- read_html(url)
# Get tables from HTML
tab_prodes <- prodes |>
  html_table() %>%
  .[[2]]

# Remove first and last row
dat <- as_tibble(as.matrix(tab_prodes[c(-1, -nrow(tab_prodes)), ]))
# Use first row as header
col_names <- unlist(tab_prodes[1, ])
names(dat) <- col_names

# Clean data
deforestation <- dat |>
  # Convert to numeric
  mutate(across(everything(), as.numeric)) |>
  rename(year = 1, total = last_col()) |>
  # Convert to long
  pivot_longer(cols = -"year", names_to = "abbrev_state", values_to = "area") |>
  # Compute grouped trend
  mutate(trend = rollmean(area, k = 5, fill = NA), .by = "abbrev_state")

# Separate a data set for the column plot
deforestation_total <- deforestation |>
  filter(abbrev_state == "total")

# Import shapefile of Brazilian states
states <- read_state()

# Separate a data set for the map (state data)
dat_state23 <- deforestation |>
  filter(year == 2023, abbrev_state != "total")

# Join state deforestation with shape
states_deforest <- left_join(states, dat_state23, by = "abbrev_state")
# Bin data into simple groups
states_deforest <- states_deforest |>
  mutate(group = factor(findInterval(area, c(100, 500, 1000))))

# Plot elements -----------------------------------------------------------

# Get palette from ggredist
# ggredist::ggredist$natgeo

pal <- c("#abafd0", "#e8b3a5", "#fded7e", "#b6c572", "#efc965", "#fcf3e2", "gray40", "#000000", "#fefefe")

basic_colors <- c("#FFD51C", "#000000")

text_axis <- tibble(
  x = 1986,
  y = seq(5, 30, 5) * 1000,
  label = ifelse(y == 30000, paste(format(y, big.mark = "."), "km²"), format(y, big.mark = "."))
  )

title <- '<span style = "color: #FFD51C;">|</span><span> DEFORESTATION IN THE AMAZON RAINFOREST</span>'

# Plots -------------------------------------------------------------------

plot_col <- ggplot(deforestation_total, aes(year, area)) +
  geom_col(fill = pal[5]) +
  geom_line(aes(year, trend), alpha = 0.8, linewidth = 1) +
  geom_text(
    data = text_axis,
    aes(x, y, label = label),
    nudge_x = -1.1,
    nudge_y = 900,
    size = 3.5,
    family = font,
    hjust = 0,
    color = colors[2]
  ) +
  geom_hline(yintercept = 0) +
  annotate(
    "label",
    x = 2014,
    y = 13000,
    label = "5-year moving average",
    family = font,
    size = 3.5
  ) +
  geom_curve(
    data = dplyr::filter(deforestation_total, year == 2020),
    aes(x = 2020, xend = 2018, y = area, yend = 13000),
    angle = 90,
    arrow = arrow(length = unit(2, "pt"))
  ) +
  scale_x_continuous(
    breaks = c(1988, seq(1990, 2020, 5), 2024),
    expand = expansion(mult = c(0.05, 0.01))) +
  scale_y_continuous(
    breaks = seq(5, 30, 5) * 1000,
    expand = expansion(mult = c(0, 0.05))) +
  labs(
    x = NULL,
    y = NULL,
    title = title,
    subtitle = "Annual forest loss shows a slight downward trend after rising in the mid-2010s.\nThe data was last updated in November 2024.",
    caption = "Source: INPE / PRODES"
    ) +
  theme_minimal(base_family = "Jost", base_size = 12) +
  theme(
    text = element_text(color = basic_colors[2]),
    panel.background = element_rect(fill = pal[9], color = pal[9]),
    plot.background = element_rect(fill = pal[9], color = pal[9]),
    panel.grid.major.y = element_line(color = "gray80"),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.ticks.x = element_line(),
    axis.text.y = element_blank(),
    #plot.title = element_text(size = 22)
    plot.title = element_textbox(family = "Jost")
  )


plot_map <- ggplot(states_deforest) +
  geom_sf(aes(fill = group), color = "gray90", lwd = 0.15) +
  geom_sf_label(aes(label = abbrev_state, color = group), family = "Jost", size = 5) +
  coord_sf(xlim = c(NA, -43), ylim = c(-20, NA)) +
  scale_fill_manual(
    name = "Deforested Area (km²)",
    values = pal[c(1, 3, 5, 2)],
    labels = c("0—100", "101—500", "501—1000", ">1000")) +
  scale_color_manual(
    name = "Deforested Area (km²)",
    values = pal[c(1, 7, 5, 2)],
    labels = c("0—100", "101—500", "501—1000", ">1000")) +
  guides(color = "none") +
  labs(
    title = "DEFORESTATION BY STATE (2023)"
  ) +
  ggthemes::theme_map(base_family = "Jost") +
  theme(
    panel.background = element_rect(fill = pal[6], color = pal[6]),
    plot.background = element_rect(fill = pal[6], color = pal[6]),
    legend.background = element_rect(fill = pal[6], color = pal[6]),
    legend.title = element_text(size = 14),
    legend.text = element_text(size = 10),
    plot.title = element_text(size = 22, hjust = 0.5)
  )

showtext_opts(dpi = 300)
showtext_auto()
ggsave("plots/30_national_geographic.png", plot_col, width = 7.76, height = 4.36, dpi = 300)
ggsave("plots/30_national_geographic_map.png", plot_map, width = 7, height = 7, dpi = 300)
