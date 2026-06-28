# Prompt: Uncertainties — Risk
# Brazil's EMBI+ country-risk over recent economic history, annotated.
# Source: J.P. Morgan via Ipeadata.

library(dplyr)
library(ggplot2)

import::from(ipeadatar, ipeadata)
import::from(tibble, tribble, tibble)
import::from(stringr, str_wrap, str_count)
import::from(scales, label_number)
import::from(MetBrewer, met.brewer)
import::from(lubridate, months)
import::from(ragg, agg_png)
import::from(here, here)

# Data --------------------------------------------------------------------

embi <- ipeadata("JPM366_EMBI366")

codace <- tribble(
  ~rec_start,               ~rec_end, ~label,
  #-------------------#----------------------#----------#
  as.Date("1997-10-01"), as.Date("1999-01-01"), "FHC-1",
  as.Date("2001-01-01"), as.Date("2001-10-01"), "FHC-2",
  as.Date("2002-10-01"), as.Date("2003-04-01"), "LULA",
  as.Date("2008-07-01"), as.Date("2009-01-01"), "GFR",
  as.Date("2014-01-01"), as.Date("2016-10-01"), "DILMA",
  as.Date("2019-10-01"), as.Date("2020-04-01"), "COVID"
)

# Adjusted Itamar Franco and Lula (III) to fit better with EMBI
presidents <- tribble(
  ~start,         ~end, ~name,
  "1979-12-01", "1985-02-01", "Figueiredo",
  "1985-03-01", "1989-12-01", "Sarney",
  "1990-01-01", "1992-12-01", "Collor",
  "1994-04-20", "1994-12-01", "Itamar Franco",
  "1995-01-01", "2001-12-01", "FHC",
  "2002-01-01", "2008-12-01", "Lula",
  "2009-01-01", "2016-06-01", "Dilma",
  "2016-07-01", "2017-12-01", "Temer",
  "2018-01-01", "2022-12-01", "Bolsonaro",
  "2023-01-01", "2025-12-31", "Lula"
)

presidents <- presidents |>
  mutate(
    start = as.Date(start),
    end = as.Date(end),
    x_label = if_else(name == "Collor", start + months(6), start + months(3))
  ) |>
  filter(end >= min(embi$date))

# Superseded first events table (joined undefined `ipca`; overwritten below, not run)
# df <- tribble(
#   ~date, ~event, ~date_label,
#   "1980-01-01", "Delfim + Oil Crisis", "DEC 1979",
#   "1983-02-01", "Delfim (2nd)", "FEB 1983",
#   "1985-05-01", "Military Regime Ends", "MAR 1985",
#   "1986-04-01", "Cruzado Plan", "FEB 1986",
#   "1987-08-01", "Bresser Plan", "JUN 1987",
#   "1989-03-01", "Verão Plan", "JAN 1989",
#   "1990-05-01", "Collor Plan", "MAR 1990",
#   "1991-04-01", "Collor Plan II", "JAN 1991",
#   "1993-01-01", "Collor Impeachment", "DEC 1992",
#   "1994-08-01", "Real Plan", "JUL 1994",
#   "1999-01-01", "Flotaing Exchange Rate", "JAN '99",
#   "1999-08-01", "Inflation Targeting", "JUN '99",
#   "2000-07-01", "New Fiscal Regime", "MAY '00",
#   "2002-11-01", "Argentina + Lula Confidence Crisis", "2002",
#   "2008-04-01", "Brazil gains Investment Grade", NA,
#   "2008-07-01", "Great Financial Recession", "2008",
#   "2015-09-01", "Brazil loses Investment Grade", "2015",
#   "2016-08-01", "Dilma Impeachment", "AUG 2016",
#   "2020-03-01", "Covid-19", "MAR 2020",
#   "2021-03-01", "Central Bank Independence", "FEB 2021"
# )

# df <- df |>
#   mutate(
#     date = as.Date(date),
#     event_label = str_wrap(event, width = 9)
#   )

# events <- left_join(df, ipca, by = "date")

# events <- events |>
#   mutate(
#     yseg = c(-8, -9.5, 25, -9.5, 45, -9.5, -9.5, 55, -9.5, 65, 10, -9.5, 15, -9.5, -8, 10, 10, -9.5, -10, 10),
#     x = str_count(event_label, "\\\n"),
#     vjust = 1.5 + 1.5 * x,
#     ytext = ifelse(yseg < 0, yseg - vjust, yseg + vjust)
#   )

df <- tribble(
  ~date,                          ~event,        ~date_label,    ~yseg,
  "1994-08-01", "Real Plan ends Hyperinflation",      "JUL 1994",   -250,
  "1997-06-02", "Asian Financial Crisis",             "1997",       -250,
  "1998-06-01", "Russian Crisis",                     "1998",       2000,
  "1999-01-01", "Flotaing Exchange Rate",             "JAN '99",   -250,
  "2000-07-01", "New Fiscal Regime",                  "MAY '00",   -250,
  "2002-10-01", "Argentina + Lula Confidence Crisis", "2002",       -250,
  "2008-04-01", "Brazil gains Investment Grade",      "SEP 2008",  -250,
  "2008-07-01", "Great Financial Recession",          "2008",       1200,
  "2013-06-01", "Riots and Protests of 2013",         "JUN 2013",  -250,
  "2015-09-01", "Brazil loses Investment Grade",      "SEP 2015", 1300,
  "2016-08-01", "Dilma Impeachment",                  "AUG 2016",  -250,
  "2016-12-01", "Expenditure Ceiling",                "DEC 2016", 900,
  "2019-10-01", "Pensions Reform",                    "OCT 2019",  -250,
  "2020-03-01", "COVID-19 outbreaks in Brazil",       "MAR 2020", 1250,
  "2021-03-01", "Central Bank Independence",          "FEB 2021",  750,
  "2023-01-08", "Failed Coup Attempt",                "JAN 2023", -250,
  "2024-07-29", "EMBI+ is Discontinued",              "JUL 2024", 500
)

# Adjusted function to find nearest date
find_nearest_date <- function(ref_date, date_vector) {
  dist <- abs(as.numeric(difftime(date_vector, ref_date, units = "days")))
  ind <- which.min(dist)
  return(date_vector[ind])
}

df <- df |>
  mutate(
    date = as.Date(date),
    event_label = str_wrap(event, width = 9)
  ) |>
  rowwise() |>
  mutate(
    date_adjusted = find_nearest_date(date, embi$date)
  )

events <- left_join(df, embi, by = c("date_adjusted" = "date"))

left_event <- c(
  "Russian Crisis",
  "Brazil gains Investment Grade",
  "Brazil loses Investment Grade",
  "Riots and Protests of 2013"
)

events <- events |>
  mutate(
    #yseg = c(-4, 7, -4, 14, -4, -3, -3, 12, 13, -4, 8, -4, 8, -4, -4, -4),
    x_event_label = if_else(event %in% left_event, date - months(12), date),
    y_date_label = if_else(yseg > 0, yseg - 150, -150),
    x = str_count(event_label, "\\\n"),
    vjust = 45 + 45 * x,
    ytext = ifelse(yseg < 0, yseg - vjust, yseg + vjust),
    segment_end = if_else(event %in% left_event, date - months(12), date + months(12))
  )

# Manual y-axis labels (built but unused, not run)
# df_axis_text <- tibble(
#   x = as.Date("1992-01-01"),
#   y = seq(0, 2500, 500),
#   label = format(y, big.mark = ",")
# )
# df_axis_text <- df_axis_text |>
#   mutate(label = ifelse(y == 2500, paste(label, "Basis Points"), label))

# Plot --------------------------------------------------------------------

font <- "Avenir"

base_plot <- ggplot() +
  geom_line(
    data = embi, aes(date, value)
  ) +
  geom_hline(yintercept = 0) +
  scale_x_date(
    breaks = c(seq(as.Date("1995-01-01"), as.Date("2025-01-01"), by = "5 year")),
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.03))
  ) +
  scale_y_continuous(
    breaks = seq(0, 2500, 500),
    labels = label_number(big.mark = ",")) +
  scale_fill_manual(values = met.brewer("VanGogh1", n = 12)[c(7, 4, 2, 12, 10, 8)]) +
  guides(fill = "none")

rect_plot <- base_plot +
  geom_rect(
    data = codace,
    aes(xmin = rec_start, xmax = rec_end, ymin = 0, ymax = 2700, group = label),
    alpha = 0.4
  ) +
  geom_rect(
    data = presidents,
    aes(xmin = start, xmax = end, ymin = 0, ymax = 2700, fill = name),
    alpha = 0.5
  ) +
  # Label President
  geom_text(
    data = presidents,
    aes(x = x_label, y = 2650, label = name),
    angle = 90,
    hjust = 1,
    family = font,
    fontface = "bold",
    color = "gray20"
  )

annotated_plot <- rect_plot +
  # Points
  geom_point(
    data = events,
    aes(x = date, y = value),
    shape = 21,
    fill = "#de2d26",
    color = "gray90"
  ) +
  # Vertical red lines
  geom_segment(
    data = events,
    aes(x = date, xend = date, y = yseg, yend = value),
    color = "#de2d26"
  ) +
  # Horizontal red line
  geom_segment(
    data = events,
    aes(x = date, xend = segment_end, y = yseg, yend = yseg),
    color = "#de2d26"
  ) +
  # Events dates labels
  geom_label(
    data = events,
    aes(x = date, y = y_date_label, label = date_label),
    label.r = unit(0, "pt"),
    label.padding = unit(0.1, "line"),
    size = 3,
    fill = "black",
    color = "white",
    family = font
  ) +
  # Events labels
  geom_text(
    data = events,
    aes(x = x_event_label, y = ytext, label = str_wrap(event, 11)),
    size = 3,
    hjust = 0,
    lineheight = .8,
    family = font
  )

offwhite <- "#fefefe"

final_plot <- annotated_plot +
  labs(
    title = "Brazil's recent economic history from the EMBI+ viewpoint",
    subtitle = "A higher index value signals higher 'risk' (and, typically, a higher risk premium) for foreign investors. The index is measured in\nbasis points (100 basis points = 1%), so an EMBI+ reading of 1,000 indicates that Brazilian bonds, on average, yield 10 percentage\npoints more than U.S. bonds. Gray-shaded areas represent recessions, while other colors indicate the ruling president at the time.",
    x = NULL,
    y = "Basis Points",
    caption = "Source: J.P. Morgan (Ipeadata)."
  ) +
  theme_minimal(base_family = font) +
  theme(
    panel.background = element_rect(fill = offwhite, color = offwhite),
    plot.background = element_rect(fill = offwhite, color = offwhite),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(size = 12, color = "gray15"),
    axis.title.y = element_text(size = 12),
    plot.margin = margin(20, 5, 10, 5),
    plot.title = element_text(size = 20, face = "bold"),
    plot.subtitle = element_text(color = "gray30"),
    plot.caption = element_text(color = "gray30")
  )

# Save --------------------------------------------------------------------

ggsave(here("2025/plots/25_risk.png"), final_plot, width = 12, height = 6.5, device = agg_png)
