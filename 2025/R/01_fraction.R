# Prompt: Comparisons — Fraction
# Share of Brazilian cities where rents and sale prices beat inflation, as a
# hand-built waffle: one tile per city, one grid per year.
# Sources: FipeZap online listings (realestatebr) and IPCA inflation (rbcb).

library(dplyr)
library(ggplot2)

import::from(realestatebr, get_dataset)
import::from(rbcb, get_series)
import::from(tidyr, pivot_wider, expand_grid)
import::from(lubridate, year, month)
import::from(ggtext, element_textbox_simple, geom_textbox)
import::from(ragg, agg_png)
import::from(here, here)

# Data --------------------------------------------------------------------

fipe <- get_dataset("rppi", table = "fipezap", quiet = TRUE)
ipca <- get_series(433, start_date = as.Date("2019-01-01"), as = "tibble")

# Wrangle -----------------------------------------------------------------

ipca_year <- ipca |>
  rename(value = `433`) |>
  mutate(year = year(date)) |>
  summarise(ipca = prod(1 + value / 100) - 1, .by = year)

# December 12-month change vs. calendar-year IPCA. Only cities with both a
# rent and a sale series are covered in a given year.
cities <- fipe |>
  filter(
    market == "residential",
    variable == "acum12m",
    rooms == "total",
    month(date) == 12,
    year(date) %in% 2019:2024,
    name_muni != "Brazil",
    !is.na(value)
  ) |>
  mutate(year = year(date)) |>
  pivot_wider(
    id_cols = c(year, name_muni),
    names_from = rent_sale,
    values_from = value
  ) |>
  filter(!is.na(rent), !is.na(sale)) |>
  left_join(ipca_year, by = "year") |>
  mutate(
    category = case_when(
      sale > ipca & rent > ipca ~ "both",
      sale > ipca ~ "sale",
      rent > ipca ~ "rent",
      .default = "none"
    )
  )

## Waffle grid -------------------------------------------------------------

# Tiles fill a fixed square grid bottom-up, left to right, sorted by
# category. Slots beyond a year's coverage stay empty.
levels_cat <- c("both", "sale", "rent", "none", "not_covered")
n_side <- ceiling(sqrt(max(count(cities, year)$n)))

tiles <- cities |>
  mutate(category = factor(category, levels_cat)) |>
  arrange(year, category) |>
  mutate(slot = row_number(), .by = year) |>
  select(year, slot, name_muni, category)

tiles <- expand_grid(year = 2019:2024, slot = seq_len(n_side^2)) |>
  left_join(tiles, by = c("year", "slot")) |>
  mutate(
    category = replace(category, is.na(category), "not_covered"),
    x = (slot - 1) %% n_side + 1,
    y = (slot - 1) %/% n_side + 1
  )

## Highlights --------------------------------------------------------------

# Outline of a set of unit tiles: keep each tile edge whose neighbour across
# that edge is not in the set. Works for any shape, including staircases.
outline_tiles <- function(cells) {
  edges <- bind_rows(
    mutate(
      cells,
      x0 = x - .5,
      x1 = x + .5,
      y0 = y - .5,
      y1 = y - .5,
      nx = x,
      ny = y - 1
    ),
    mutate(
      cells,
      x0 = x - .5,
      x1 = x + .5,
      y0 = y + .5,
      y1 = y + .5,
      nx = x,
      ny = y + 1
    ),
    mutate(
      cells,
      x0 = x - .5,
      x1 = x - .5,
      y0 = y - .5,
      y1 = y + .5,
      nx = x - 1,
      ny = y
    ),
    mutate(
      cells,
      x0 = x + .5,
      x1 = x + .5,
      y0 = y - .5,
      y1 = y + .5,
      nx = x + 1,
      ny = y
    )
  )
  anti_join(
    edges,
    select(cells, year, x, y),
    by = c("year", "nx" = "x", "ny" = "y")
  )
}

# Sanity check: a 2x1 block has 6 outer edges
stopifnot(nrow(outline_tiles(tibble(year = 1, x = 1:2, y = 1))) == 6)

share_of <- function(yr, cat) {
  d <- filter(tiles, year == yr, category != "not_covered")
  n <- sum(d$category == cat)
  list(n = n, total = nrow(d), pct = round(100 * n / nrow(d)))
}

s20 <- share_of(2020, "both")
s21 <- share_of(2021, "none")
s23 <- share_of(2023, "none")
stopifnot(s23$n == 0) # the 2023 note assumes no city fell behind on both
s24 <- share_of(2024, "both")
key_city <- filter(tiles, year == 2019, slot == 1)$name_muni

ipca22 <- round(100 * ipca_year$ipca[ipca_year$year == 2022], 1)
rent22 <- fipe |>
  filter(
    name_muni == "Brazil",
    market == "residential",
    rent_sale == "rent",
    variable == "acum12m",
    rooms == "total",
    date == as.Date("2022-12-01")
  ) |>
  pull(value)
rent22 <- round(100 * rent22, 1)

highlights <- bind_rows(
  filter(tiles, year == 2019, slot == 1),
  filter(tiles, year == 2020, category == "both"),
  filter(tiles, year == 2021, category == "none"),
  filter(tiles, year == 2024, category == "both")
)

notes <- tibble(
  year = c(2019, 2020, 2021, 2022, 2023, 2024),
  label = c(
    glue::glue(
      "**How to read.** Each tile is one city. The outlined tile is ",
      "{key_city}."
    ),
    glue::glue(
      "**First signs.** Only {s20$n} cities ({s20$pct}%) saw both rents and ",
      "prices beat inflation."
    ),
    glue::glue(
      "**Pandemic low.** In {s21$pct}% of cities ({s21$n} of {s21$total}), ",
      "neither rents nor prices beat inflation."
    ),
    glue::glue(
      "**Rent boom.** Even with inflation at {ipca22}%, rents grew ",
      "{rent22}% nationwide."
    ),
    glue::glue(
      "**No city left behind.** Rents or prices beat inflation in all ",
      "{s23$total} cities."
    ),
    glue::glue(
      "**Today.** In {s24$pct}% of cities ({s24$n} of {s24$total}), ",
      "both rents and prices beat inflation."
    )
  )
)

# Plot --------------------------------------------------------------------

offwhite <- "#f5f5f5"
colors_cat <- c(
  both = "#9b2226",
  sale = "#bb3e03",
  rent = "#ee9b00",
  none = "#778da9",
  not_covered = offwhite
)
labels_cat <- c(
  both = "Both above inflation",
  sale = "Only sale prices above",
  rent = "Only rents above",
  none = "Both below inflation",
  not_covered = "Not covered"
)

p_waffle <- ggplot(tiles, aes(x, y)) +
  geom_tile(
    aes(fill = category, color = category == "not_covered"),
    width = 0.86,
    height = 0.86,
    linewidth = 0.3
  ) +
  geom_segment(
    data = outline_tiles(highlights),
    aes(x = x0, xend = x1, y = y0, yend = y1),
    linewidth = 0.8,
    lineend = "square",
    color = "gray10"
  ) +
  geom_textbox(
    data = notes,
    aes(x = 0.5, y = 0.2, label = label),
    hjust = 0,
    vjust = 1,
    halign = 0,
    width = unit(1, "npc"),
    box.colour = NA,
    fill = NA,
    box.padding = margin(0),
    family = "Lato",
    size = 3.2,
    lineheight = 1.2,
    color = "gray20"
  ) +
  facet_wrap(vars(year), nrow = 1) +
  scale_fill_manual(values = colors_cat, labels = labels_cat, name = NULL) +
  scale_color_manual(
    values = c(`FALSE` = NA, `TRUE` = "gray60"),
    guide = "none"
  ) +
  scale_y_continuous(limits = c(-2.6, n_side + 0.5), expand = c(0, 0)) +
  coord_equal(clip = "off") +
  guides(
    fill = guide_legend(
      nrow = 1,
      override.aes = list(color = c(NA, NA, NA, NA, "gray60"))
    )
  ) +
  labs(
    title = "Rents and home prices now beat inflation in most Brazilian cities",
    subtitle = glue::glue(
      "Cities by whether rents, sale prices, or both rose faster ",
      "than inflation (IPCA) over the year. FipeZap tracks both prices in ",
      "{min(count(cities, year)$n)} cities through 2021 and ",
      "{max(count(cities, year)$n)} from 2022."
    ),
    caption = "Source: FipeZap (online listings, 12-month change in December) and IBGE (IPCA) • @viniciusoike",
    x = NULL,
    y = NULL
  ) +
  theme_void(base_family = "Lato") +
  theme(
    plot.background = element_rect(fill = offwhite, color = offwhite),
    plot.margin = margin(15, 20, 10, 20),
    plot.title = element_text(size = 18, margin = margin(b = 6)),
    plot.subtitle = element_textbox_simple(
      size = 11,
      color = "gray25",
      margin = margin(b = 12)
    ),
    plot.caption = element_text(hjust = 0, color = "gray40", size = 8),
    legend.position = "top",
    legend.justification = "left",
    legend.text = element_text(size = 10),
    legend.margin = margin(b = 6),
    strip.text = element_text(size = 13, face = "bold", margin = margin(b = 4)),
    panel.spacing = unit(1.4, "lines")
  )

# Save --------------------------------------------------------------------

ggsave(
  here("2025/plots/01_fraction.png"),
  p_waffle,
  width = 12,
  height = 4.3,
  dpi = 300,
  device = agg_png
)
