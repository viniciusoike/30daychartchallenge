library(ggplot2)
library(ggtext)

import::from(dplyr, tibble, mutate, filter)
import::from(GetBCBData, gbcbd_get_series)
import::from(stringr, str_glue)

reserves <- gbcbd_get_series(3546, first.date = "1970-01-01")
# max(dat$ref.date)

reserves <- reserves |>
  filter(ref.date <= as.Date("2026-03-01")) |>
  mutate(bln = value / 1e3)

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

df_labels <- tibble(
  date = as.Date(c("1971-06-01", "1990-01-01", "1993-01-01", "2012-01-01")),
  ypos = c(60, 140, 250, 280),
  label_title = c(
    "The Lost Decades (1971-1993)",
    "A New Hope: Real (1994-2001)",
    "The Lula Boom (2002-2010)",
    "The New Normal (2011-)"
  ),
  label_text = c(
    "Brazil's foreign reserves rarely exceeded $10B during<br>
decades of hyperinflation, debt crises, and IMF bailouts.",
    "The Real tames inflation but<br>defending its peg depletes reserves<br>in 1999, forcing the Central Bank to<br>float the exchange rate.",
    "Rising commodity prices and record<br>trade surpluses allowed Brazil to <br>finally accumulate reserves. Brazil pays<br>off its IMF debt in 2005, ahead of schedule.",
    "Brazil has held ~$350B in reserves<br>since 2012. In 2009, for the first<br>time, it lent to the IMF<br>— not the other way around."
  )
)

df_labels <- df_labels |>
  mutate(
    label = str_glue("<b>{label_title}</b><br>{label_text}")
  )

plot_final <- ggplot(reserves, aes(ref.date, bln)) +
  geom_area(alpha = 0.2, fill = main_color) +
  geom_line(lwd = 0.7, color = main_color) +
  geom_richtext(
    data = df_labels,
    aes(x = date, y = ypos, label = label),
    size = 2.5,
    family = "Lato",
    hjust = 0
  ) +
  scale_x_date(
    breaks = c(
      as.Date("1971-01-01"),
      seq(as.Date("1980-01-01"), as.Date("2020-01-01"), by = "10 years"),
      as.Date("2026-01-01")
    ),
    date_labels = "%Y",
    expand = expansion(0.01),
  ) +
  scale_y_continuous(
    breaks = seq(0, 400, 50),
    labels = scales::label_dollar(),
    expand = expansion(0),
    limits = c(NA, 400)
  ) +
  labs(
    title = "From Bailouts to Buffers",
    subtitle = "Brazil's international reserves, 1971–2024 (USD billions).",
    caption = "Source: Brazilian Central Bank (BCB) • @viniciusoike",
    x = NULL,
    y = NULL
  ) +
  theme_plot

ggsave(
  here::here("2026", "plots", "21_historical.png"),
  plot_final,
  width = 8,
  height = 5,
  dpi = 400
)
