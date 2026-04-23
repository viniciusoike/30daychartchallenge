library(ggplot2)
library(dplyr)
library(rvest)
library(stringr)

url <- "https://en.wikipedia.org/wiki/List_of_solar_cycles"

dating <- read_html(url) |>
  html_table() |>
  purrr::pluck(1) |>
  janitor::clean_names()

cycle_dating <- dating |>
  filter(solar_cycle != "Average") |>
  mutate(
    start_year = as.numeric(str_extract(start_min_y_m, "^[0-9]{4}")),
    start_month = str_extract(start_min_y_m, "(?<=– )[A-z]{3}"),
    start_date = readr::parse_date(paste(start_year, start_month, "01", sep = "/"), format = "%Y/%b/%d")
  )

recession <- readxl::read_excel(
  "data/day_22/BCDC_spreadsheet_for_website.xlsx",
  sheet = "NBER Chronology",
  range = "C5:J38",
  col_names = c("peak", "trough", "peak_month_number", "trough_month_number",
                "contraction", "expansion", "cycle_trough", "cycle_peak")
)

recession <- recession |>
  mutate(
    date_ym_peak = str_trim(str_remove(peak, "\\(.+\\)")),
    date_ym_trough = str_trim(str_remove(trough, "\\(.+\\)")),
    rec_start = readr::parse_date(date_ym_peak, format = "%B %Y"),
    rec_end = readr::parse_date(date_ym_trough, format = "%B %Y")
  )


# Monthly numbers of sunspots, as from the World Data Center, aka SIDC

dat <- read.delim(
  "data/day_22/SN_m_tot_V2.0.csv",
  sep = ";",
  dec = ".",
  header = FALSE
  )

names(dat) <- c("year", "month", "date", "sunspot", "sunspot_sd", "ind1", "ind2")
dat <- as_tibble(dat)

dat <- dat |>
  mutate(
    date = lubridate::make_date(year, month, 1)
  )

model_stl <- stl(ts(dat$sunspot, frequency = 12), s.window = 27)
filter_ma <- stats::filter(dat$sunspot, sides = 1, filter = rep(1/23, 23))

dat$trend <- as.numeric(model_stl$time.series[, "trend"])

dat <- dat |>
  mutate(
    trend_ma = RcppRoll::roll_mean(sunspot, 29, fill = NA)
  )

sunspots <- dplyr::filter(dat, year >= 1855)

font = "Avenir"
offwhite <- "#fefefe"

text_label_1 <- str_wrap("Dashed lines indicate sunspot cycles", 15)
text_label_2 <- str_wrap("Shaded areas indicate periods of economic contraction of the US economy", 19)

p <- ggplot() +
  geom_rect(
    data = recession,
    aes(xmin = rec_start, xmax = rec_end, ymin = 0, ymax = Inf),
    alpha = 0.5,
    fill = "gray"
  ) +
  geom_line(
    data = sunspots,
    aes(date, sunspot),
    alpha = 0.6,
    linewidth = 0.4,
    color = "#9b2226"
  ) +
  geom_line(
    data = sunspots,
    aes(date, trend),
    linewidth = 1,
    color = "#9b2226"
  ) +
  geom_vline(
    data = dplyr::filter(cycle_dating, start_date >= as.Date("1855-01-01")),
    aes(xintercept = start_date),
    color = "gray50",
    linetype = 2
  ) +
  geom_hline(yintercept = 0, linewidth = 1, color = "gray20") +
  annotate(
    "label",
    x = as.Date("2030-07-01"), y = 335, label = text_label_1,
    family = font,
    size = 3,
    fill = offwhite
  ) +
  annotate(
    "label",
    x = as.Date("2030-07-01"), y = 260, label = text_label_2,
    family = font,
    size = 3,
    fill = offwhite
  ) +
  scale_x_date(
    breaks = seq(from = as.Date("1860-01-01"), to = as.Date("2020-01-01"), by = "20 year"),
    date_labels = "%Y",
    expand = expansion(mult = c(0.01, 0.065))
    ) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  labs(
    title = "Sunspot Cycles and Economic Booms and Busts",
    subtitle = str_wrap("Since British economist Stanley Jevons' (1835-1882) seminal work, several economists and econometricians have obsessed over sunspots and their influence on economic cycles. Some have contended that downturns in economic activity coincide with troughs in the sunspot cycle. Jevons believed that sunspot activity affected crop productivity, which in turn influenced crop prices — leading to a descrease in aggregate demand and lower profits. Despite the enduring appeal of this idea, the evidence suggests that there's no significant connection between them.", 171),
    x = NULL,
    y = "Monthly number of sunspots",
    caption = "Sources: Royal Observatory of Belgium (sunspots). National Bureau of Economic Research (economic cycles)."
  ) +
  theme_minimal(base_family = font) +
  theme(
    plot.margin = margin(15, 10, 5, 10),
    legend.position = "bottom",
    panel.background = element_rect(color = offwhite, fill = offwhite),
    plot.background = element_rect(color = offwhite, fill = offwhite),
    plot.title = element_text(size = 24),
    plot.subtitle = element_text(size = 12, color = "gray30"),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linetype = 1),
    panel.grid.major.x = element_blank(),
    axis.text = element_text(size = 12, color = "gray30"),
    axis.ticks.x = element_line(color = "gray30"),
    axis.ticks.length = unit(7, "pt")
  )

ggsave("plots/22_stars.png", p, width = 14, height = 7)
