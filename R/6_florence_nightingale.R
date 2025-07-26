library(ggplot2)
library(data.table)
library(tidyr)
import::from(here, here)

dirpath = here("data/day_7/bilheteria-diaria-obras-por-distribuidoras-csv")

files_path <- fs::dir_ls(dirpath)

files <- parallel::mclapply(files_path, fread)

dat <- rbindlist(files, fill = TRUE)
dat <- janitor::clean_names(dat)

dat[, data_exibicao := as.Date(data_exibicao, format = "%d/%m/%Y")]
dat[, year := year(data_exibicao)]
dat[, month := month(data_exibicao)]
public_month <- dat[, .(publico_total = sum(publico, na.rm = TRUE)), by = c("year", "month")]


public_month[, date := lubridate::make_date(year, month, 1)]
public_month[, month_label := lubridate::month(date, label = TRUE, abbr = TRUE)]

theme_plot <- # Create a period-appropriate theme
  theme(
    # Set background to mimic aged paper
    panel.background = element_rect(fill = "#F8F3E6"),
    plot.background = element_rect(fill = "#F8F3E6", color = "#6D5C41", linewidth = 1.5),

    # Use period-appropriate typography
    text = element_text(family = "Didot", color = "#2B2522"),
    plot.title = element_text(family = "Baskerville", face = "bold", size = 16,
                              hjust = 0.5, margin = margin(t = 20, b = 10)),
    plot.subtitle = element_text(family = "Baskerville", size = 14, hjust = 0.5,
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
      alpha = 0.85) +
    geom_text(aes(label = month_label)) +
    scale_x_continuous(breaks = 1:12, labels = month.abb) +
    scale_y_continuous(limits = c(0, 27)) +
    # facet_wrap(vars(year)) +
    # Use a coordinate system similar to Nightingale's original
    coord_polar(clip = "off") +
    # Create Victorian-style labeling
    labs(subtitle = y) +
    theme_plot

}

plot_year(2019)

library(patchwork)

plots <- lapply(2019:2024, plot_year)
names(plots) <- paste0("p", seq_along(plots))

panel <- plots$p1 | plots$p2

panel &
  plot_annotation(
    title = "",
    subtitle = "",
    caption = ""
  ) &
  theme_plot

(plots$p1 | plots$p2 | plots$p3) / (plots$p4 | plots$p5 | plots$p6)

public_month[year >= 2019, .(max(publico_total))]

dat[, .(total_film = sum(publico, na.rm = TRUE)), by = "titulo_original"][order(-total_film)] |> View()


