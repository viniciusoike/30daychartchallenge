library(ggplot2)
library(data.table)
library(patchwork)
library(tidyr)
import::from(here, here)

# Import data ---------------------------------------------------------------
dirpath <- here("data/day_6/bilheteria-diaria-obras-por-distribuidoras-csv")

# List all csvs and stack
files_path <- fs::dir_ls(dirpath)
files <- parallel::mclapply(files_path, fread)
# Stack all datasets
dat <- rbindlist(files, fill = TRUE)
dat <- janitor::clean_names(dat)
# Format dates and find totals by year-month
dat[, data_exibicao := as.Date(data_exibicao, format = "%d/%m/%Y")]
dat[, year := year(data_exibicao)]
dat[, month := month(data_exibicao)]

public_month <- dat[,
  .(publico_total = sum(publico, na.rm = TRUE)),
  by = c("year", "month")
]

# Get month abbreviations to use as labels
public_month[, date := lubridate::make_date(year, month, 1)]
public_month[, month_label := lubridate::month(date, label = TRUE, abbr = TRUE)]

# Plots elements -----------------------------------------------------------

theme_plot <- # Create a period-appropriate theme
  theme(
    text = element_text(family = "Copperplate", color = "#2B2522"),
    # Set background to mimic aged paper
    panel.background = element_rect(fill = "#F8F3E6"),
    plot.background = element_rect(fill = "#F8F3E6", color = "#F8F3E6"),

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
    plot.margin = margin(t = 5, b = 5)
  )

theme_annotation <- theme(
  # Set background to mimic aged paper
  panel.background = element_rect(fill = "#F8F3E6"),
  plot.background = element_rect(fill = "#F8F3E6", color = "#F8F3E6"),
  plot.title = element_text(
    family = "Baskerville",
    face = "bold",
    size = 16,
    hjust = 0.5,
    margin = margin(t = 10, b = 5)
  ),
  plot.subtitle = element_text(
    family = "Copperplate",
    size = 10,
    hjust = 0.5,
    color = "#4a4e69",
    margin = margin(b = 5)
  ),
  plot.caption = element_text(
    family = "Baskerville",
    size = 8,
    hjust = 0,
    color = "#4a4e69",
    margin = margin(t = 5)
  ),
  plot.margin = margin(t = 5, r = 10, b = 5, l = 10)
)

# Plot function -------------------------------------------------------------

plot_year <- function(y) {
  subdata <- public_month[year == y]

  # Create Nightingale rose diagram with authentic Victorian styling
  ggplot(subdata, aes(x = month, y = publico_total / 1e6)) +
    # Use thick black borders as in original diagrams
    geom_col(
      width = 1,
      position = "stack",
      fill = "#4E79A7",
      color = "black",
      linewidth = 0.5,
      alpha = 0.85
    ) +
    geom_label(
      aes(label = round(publico_total / 1e6, 1)),
      family = "Copperplate",
      nudge_y = 1.5,
      size = 3
    ) +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    scale_y_continuous(limits = c(0, 28)) +
    # facet_wrap(vars(year)) +
    # Use a coordinate system similar to Nightingale's original
    coord_polar(clip = "off") +
    # Create Victorian-style labeling
    labs(subtitle = y, y = NULL) +
    theme_plot
}

# Plots --------------------------------------------------------------------

plots <- lapply(2019:2024, plot_year)
names(plots) <- paste0("p", seq_along(plots))

# Best panel: shows before and after
panel <- plots$p1 | plots$p6

final_plot <- panel &
  plot_annotation(
    title = toupper("Movie theathers in Brazil are still down in public"),
    subtitle = "Total number of monthly movie theater ticket sales by volume\n(millions of tickets sold) in Brazil, comparing 2019 baseline with 2024.\nScales of both axes are fixed to facilitate comparisons between years.",
    caption = "SOURCE: ANCINE (2025).",
    theme = theme_annotation
  )

# Export
ggsave(
  here("plots/6_florence_nightingale.png"),
  final_plot,
  width = 8,
  height = 8 / 1.618
)
