# Prompt: Comparisons — Pictogram
# Neurath-style (ISOTYPE) pictogram of Brazil's housing stock: tenure (owned,
# rented, other) by dwelling type (house, apartment). Each symbol = 1 million
# households. IBGE Census 2022, SIDRA table 9933.

library(dplyr)
library(ggplot2)
library(stringr)
library(ggtext)
library(ragg)

import::from(sidrar, get_sidra)
import::from(janitor, clean_names)
import::from(tidyr, uncount)
import::from(systemfonts, register_font, registry_fonts)
import::from(here, here)

# Fonts ---------------------------------------------------------------------

# Font Awesome 5 (solid) ships with the waffle package; register it so ragg
# can render the glyphs without a system-wide install
if (!"Font Awesome 5 Free Solid" %in% registry_fonts()$family) {
  register_font(
    name = "Font Awesome 5 Free Solid",
    plain = system.file("fonts", "fa-solid-900.ttf", package = "waffle")
  )
}

font_fa <- "Font Awesome 5 Free Solid"

# Data ----------------------------------------------------------------------

dat <- get_sidra(
  api = "/t/9933/n1/all/v/allxp/p/all/c1975/73086/c63/allxt/c125/allxt"
)

dat <- dat |>
  as_tibble() |>
  clean_names()

# Wrangle ---------------------------------------------------------------------

tenures <- c(
  "Próprio de algum morador",
  "Alugado",
  "Cedido ou emprestado",
  "Outra condição"
)

households_classf <- dat |>
  filter(condicao_de_ocupacao_do_domicilio %in% tenures) |>
  mutate(
    tenure = replace_values(
      condicao_de_ocupacao_do_domicilio,
      "Próprio de algum morador" ~ "Owned",
      "Alugado" ~ "Rented",
      "Cedido ou emprestado" ~ "Other",
      "Outra condição" ~ "Other"
    ),
    type = case_when(
      str_detect(tipo_de_domicilio, "Casa") ~ "House",
      tipo_de_domicilio == "Apartamento" ~ "Apartment",
      .default = "Other"
    )
  )

# Sanity check
sum(households_classf$valor)

households <- households_classf |>
  summarise(
    total = sum(valor, na.rm = TRUE),
    .by = c("tenure", "type")
  )

## Icon grid ------------------------------------------------------------------

icon_unit <- 1e6 # one symbol = 1 million households
icons_per_line <- 15 # wrap long tenure rows
group_gap <- 0.4 # extra space after every 5 symbols (eases counting)
block_gap <- 1.8 # vertical space between tenure blocks
header_gap <- 0.95 # header sits this far above a block's first icon line

lvls_tenure <- c("Owned", "Rented", "Other")

# FA5 solid glyph codepoints: house U+F015, building U+F1AD
glyphs <- c(House = "", Apartment = "")
colors <- c(House = "#D69E2E", Apartment = "#1E3A5F")

icons <- households |>
  # remaining dwelling types (cortiços, indigenous, degraded) are < 1% of the
  # stock and round to zero symbols; noted in the caption instead
  filter_out(type == "Other") |>
  mutate(
    tenure = factor(tenure, levels = lvls_tenure),
    type = factor(type, levels = c("House", "Apartment")),
    # ISOTYPE-style: round to the nearest half symbol; an odd half renders as
    # a cut (left-half) glyph
    n_half = round(total / (icon_unit / 2)),
    n_icon = ceiling(n_half / 2)
  ) |>
  arrange(tenure, type) |>
  uncount(n_icon, .id = "pos", .remove = FALSE) |>
  mutate(half = pos == n_icon & n_half %% 2 == 1) |>
  mutate(idx = row_number() - 1, .by = "tenure") |>
  mutate(
    line = idx %/% icons_per_line,
    col = idx %% icons_per_line,
    x = col + group_gap * (col %/% 5)
  )

# Vertical offset of each tenure block (stacked top to bottom)
blocks <- icons |>
  summarise(n_line = max(line) + 1, .by = "tenure") |>
  arrange(tenure) |>
  mutate(
    offset = cumsum(lag(n_line, default = 0)) + (row_number() - 1) * block_gap
  )

icons <- icons |>
  left_join(blocks, by = "tenure") |>
  mutate(y = -(offset + line))

icons <- icons |>
  mutate(
    x = case_when(
      type == "Apartment" & tenure == "Rented" & pos < 4 ~ x - 11.8,
      type == "Apartment" & tenure == "Rented" & pos == 4 ~ x,
      .default = x
    ),
    y = case_when(
      type == "Apartment" & tenure == "Rented" & pos < 4 ~ y - 1,
      .default = y
    )
  )

# Block headers show the actual (unrounded) totals, including minor types
headers <- households |>
  summarise(total = sum(total), .by = "tenure") |>
  mutate(tenure = factor(tenure, levels = lvls_tenure)) |>
  left_join(blocks, by = "tenure") |>
  mutate(
    y = -(offset - header_gap),
    label = str_glue(
      "<span style='font-family:Georgia;font-size:13pt'>**{tenure}**</span> ",
      "<span style='color:#666666'>",
      "{format(round(total / 1e6, 1), decimal.mark = ',')} million households ",
      "({round(total / sum(total) * 100)}%)</span>"
    ),
    line_y = y - 0.3,
    x = -0.5,
    xend = icons_per_line + 2
  )

# Plot ------------------------------------------------------------------------

offwhite <- "#f8fbf8"

subtitle <- str_glue(
  "Brazil's 72,5 million occupied homes by tenure and dwelling type<br>",
  "Each symbol represents 1 million households: ",
  "<span style='font-family:\"{font_fa}\";color:{colors['House']}'>{glyphs['House']}</span> ",
  "**<span style='color:{colors['House']}'>houses</span>** and ",
  "<span style='font-family:\"{font_fa}\";color:{colors['Apartment']}'>{glyphs['Apartment']}</span> ",
  "**<span style='color:{colors['Apartment']}'>apartments</span>**.",
  "<br>"
)

caption_source <- "Source: IBGE (Census, 2022) • @viniciusoike"

caption_description <- str_wrap(
  "Symbols are rounded to the nearest half million. Other dwelling types (~1% of the stock) are ommited. The 'other' tenure includes ceded and borrowed properties, as well as informal housing arrangements.",
  121
)

caption <- str_c(caption_source, "\n", caption_description)

base_plot <- ggplot(icons, aes(x, y)) +
  geom_text(
    aes(label = glyphs[as.character(type)], color = type),
    family = font_fa,
    size = 5
  ) +
  # mask the right half of cut symbols with the background colour
  geom_rect(
    data = filter(icons, half),
    aes(xmin = x, xmax = x + 0.6, ymin = y - 0.6, ymax = y + 0.6),
    inherit.aes = FALSE,
    fill = offwhite
  ) +
  geom_richtext(
    data = headers,
    aes(x = -0.45, y = y, label = label),
    inherit.aes = FALSE,
    hjust = 0,
    size = 4,
    family = "Lato",
    fill = NA,
    label.color = NA,
    label.padding = unit(0, "pt")
  ) +
  geom_segment(
    data = headers,
    aes(x = x, xend = xend, y = line_y),
    inherit.aes = FALSE,
    color = "gray20",
    lwd = 0.5
  ) +
  scale_color_manual(values = colors, guide = "none") +
  scale_x_continuous(expand = expansion(c(0, 0.1)))

pictogram <- base_plot +
  labs(
    title = "Brazil: A Country of Owned Houses",
    subtitle = subtitle,
    caption = caption
  ) +
  theme_minimal(base_family = "Lato") +
  theme_sub_plot(
    title = element_text(size = 22, family = "Georgia"),
    subtitle = element_markdown(size = 12, color = "gray20", lineheight = 1.3),
    caption = element_text(size = 8, hjust = 0, color = "gray35"),
    background = element_rect(fill = offwhite, color = offwhite),
    title.position = "plot",
    caption.position = "plot",
    margin = margin(15, 10, 10, 10),
  ) +
  theme_sub_panel(
    background = element_rect(fill = offwhite, color = offwhite),
    grid.major = element_blank(),
    grid.minor = element_blank()
  ) +
  theme_sub_axis(
    title = element_blank(),
    text = element_blank(),
    ticks = element_blank()
  )

# Save ------------------------------------------------------------------------

ggsave(
  here("2026", "plots", "02_pictogram.png"),
  pictogram,
  width = 8,
  height = 5,
  dpi = 400,
  device = agg_png
)
