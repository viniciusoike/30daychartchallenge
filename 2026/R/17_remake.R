# Prompt: Relationships
# Remake -- William Playfair's "Exports & Imports" (1786), the first time-series
# area chart ever published, recreated with Brazil's merchandise trade. Two
# lines (exports, imports) with the gap between them shaded by sign: green when
# Brazil runs a surplus ("balance in its favour"), red when it runs a deficit.
#
# Source is the Banco Central do Brasil monthly trade series (SGS 22708/22709),
# rolled into a trailing 12-month sum and drawn on a logarithmic scale so the
# 1990s (~US$50B) and the 2020s (~US$350B) both read clearly on one plate.

# Setup -------------------------------------------------------------------

library(dplyr)
library(ggplot2)
library(ggtext)
library(scales)

import::from(here, here)
import::from(tibble, tibble)

# Fonts ("EB Garamond", "Playfair Display") are installed locally and resolved
# by name through the ragg device passed to ggsave() below — no showtext needed.

# Data --------------------------------------------------------------------

# BCB SGS: merchandise exports (22708) and imports (22709), monthly, current
# US$ millions. Trailing 12-month sum annualises the series and smooths the
# seasonal swings; log() compresses three decades of growth onto one scale.
data_bcb <- rbcb::get_series(
  list("export" = 22708, "import" = 22709)
)

series <- data_bcb |>
  purrr::reduce(left_join) |>
  rename(
    exports = export,
    imports = import
  ) |>
  mutate(
    exports = RcppRoll::roll_sumr(exports, n = 12, align = "right"),
    imports = RcppRoll::roll_sumr(imports, n = 12, align = "right"),
    exports = log(exports),
    imports = log(imports)
  ) |>
  filter(!is.na(exports), !is.na(imports)) |>
  arrange(date)

# Insert interpolated crossing vertices wherever exports and imports swap order,
# so the two-colour balance shading pinches to zero exactly at each crossing
# (no jagged seams between surplus and deficit eras). Interpolation is done on
# the Date axis, so a fractional crossing lands on the right day.
add_crossings <- function(df) {
  out <- vector("list", 0)
  for (i in seq_len(nrow(df) - 1)) {
    out[[length(out) + 1]] <- df[i, ]
    d1 <- df$exports[i] - df$imports[i]
    d2 <- df$exports[i + 1] - df$imports[i + 1]
    if (sign(d1) != sign(d2) && d1 != 0 && d2 != 0) {
      t <- d1 / (d1 - d2) # fraction to the zero crossing
      dt <- df$date[i] + t * (df$date[i + 1] - df$date[i])
      val <- df$exports[i] + t * (df$exports[i + 1] - df$exports[i])
      out[[length(out) + 1]] <- tibble(date = dt, exports = val, imports = val)
    }
  }
  out[[length(out) + 1]] <- df[nrow(df), ]
  bind_rows(out)
}

aug <- add_crossings(series)

# Plot --------------------------------------------------------------------

paper <- "#ece0c2" # aged parchment
ink <- "#3b2f1e" # sepia ink
col_exp <- "#2f2a20" # exports line
col_imp <- "#7c2d2d" # imports line (deep red-brown)
col_surplus <- "#4f6f3f" # balance in Brazil's favour
col_deficit <- "#9b3a3a" # balance against

end_date <- max(series$date)
exp_end <- series$exports[series$date == end_date]
imp_end <- series$imports[series$date == end_date]

# Period notes, à la Playfair's hand-lettered annotations. y is in log(US$ mn)
# units to match the transformed series; values chosen to clear the bands.
notes <- tibble(
  date = as.Date(c("1998-06-01", "2007-01-01", "2015-03-01", "2021-06-01")),
  y = c(11.35, 12.55, 11.55, 12.95),
  label = c(
    "Real\novervaluation\ndeficits",
    "Commodity-boom\nsurpluses",
    "Recession\ndeficit",
    "Record\nsurpluses"
  )
)

theme_plate <- theme_minimal(base_family = "EB Garamond", base_size = 13) +
  theme_sub_plot(
    title = element_text(
      family = "Playfair Display",
      size = 22,
      color = ink,
      hjust = 0.5
    ),
    subtitle = element_textbox_simple(
      family = "EB Garamond",
      size = 11,
      color = ink,
      hjust = 0.5,
      halign = 0.5,
      lineheight = 1.2,
      margin = margin(t = 4, b = 14)
    ),
    caption = element_text(
      family = "EB Garamond",
      size = 8.5,
      color = "#6b5d45",
      hjust = 0.5
    ),
    margin = margin(18, 20, 12, 20),
    background = element_rect(fill = paper, color = paper)
  ) +
  theme_sub_panel(
    grid.minor = element_blank(),
    grid.major = element_line(color = "#d6c8a6", linewidth = 0.3),
    background = element_rect(fill = paper, color = paper)
  ) +
  theme_sub_axis(
    text = element_text(color = ink, family = "EB Garamond"),
    ticks = element_line(color = ink, linewidth = 0.3)
  ) +
  theme(
    panel.border = element_rect(color = ink, fill = NA, linewidth = 0.7),
    plot.background = element_rect(fill = paper, color = ink, linewidth = 1.1)
  )

p <- ggplot(aug, aes(x = date)) +
  # Two-colour balance shading (pmax/pmin pinch each band to zero off-season).
  geom_ribbon(
    aes(ymin = imports, ymax = pmax(exports, imports)),
    fill = col_surplus,
    alpha = 0.5
  ) +
  geom_ribbon(
    aes(ymin = pmin(exports, imports), ymax = imports),
    fill = col_deficit,
    alpha = 0.5
  ) +
  # The two trade lines.
  geom_line(aes(y = imports), color = col_imp, linewidth = 0.7) +
  geom_line(aes(y = exports), color = col_exp, linewidth = 0.8) +
  # Inline line labels at the right edge (offsets in log units).
  annotate(
    "text",
    x = end_date + 120,
    y = exp_end + 0.06,
    label = "EXPORTS",
    family = "Playfair Display",
    color = col_exp,
    size = 3.6,
    hjust = 1,
    fontface = "bold"
  ) +
  annotate(
    "text",
    x = end_date + 120,
    y = imp_end - 0.06,
    label = "IMPORTS",
    family = "Playfair Display",
    color = col_imp,
    size = 3.6,
    hjust = 1,
    fontface = "bold"
  ) +
  # Period notes.
  geom_text(
    data = notes,
    aes(y = y, label = label),
    family = "EB Garamond",
    fontface = "italic",
    color = ink,
    size = 3.1,
    lineheight = 0.9
  ) +
  scale_x_date(
    breaks = as.Date(paste0(seq(1995, 2025, 10), "-01-01")),
    date_labels = "%Y",
    expand = expansion(mult = c(0.02, 0.08))
  ) +
  scale_y_continuous(
    position = "right",
    breaks = log(c(50e3, 100e3, 200e3, 400e3)),
    labels = c("$50 B", "$100 B", "$200 B", "$400 B"),
    expand = expansion(mult = c(0.05, 0.07))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = "EXPORTS & IMPORTS of BRAZIL",
    subtitle = paste0(
      "Chart Shewing the Value of Merchandize Traded to and from all Nations, ",
      "1995&ndash;2026, drawn upon a Logarithmick Scale; the Balance in ",
      "Brazil's Favour shaded ",
      "<b style='color:",
      col_surplus,
      "'>green</b>, and Against, ",
      "<b style='color:",
      col_deficit,
      "'>red</b>."
    ),
    caption = paste0(
      "Source: Banco Central do Brasil (SGS 22708/22709) — merchandise exports/imports, trailing 12-month sum (current US$, not inflation-adjusted), 1995–2026. ",
      "After William Playfair's 'Exports & Imports' (1786). • @viniciusoike"
    ),
    x = NULL,
    y = NULL
  ) +
  theme_plate

ggsave(
  here("2026/plots/17_remake.png"),
  p,
  width = 10,
  height = 6,
  dpi = 400,
  device = ragg::agg_png
)
