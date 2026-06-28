# =============================================================================
# pictogram_households.R
#
# Linear script — run top to bottom, tweak anything along the way.
# Builds a household type × ownership pictogram from clean_dat.
#
# Icons: download flat/mono SVGs from svgrepo and save to ./icons/
#   house.svg     → svgrepo.com/svg/494102/house
#   apartment.svg → svgrepo.com/svg/380557/building-line-architecture
#   other.svg     → svgrepo.com/svg/530598/building
#
# Dependencies: dplyr, glue, stringr, xml2, rsvg, magick
# =============================================================================

library(dplyr)
library(glue)
library(stringr)
library(xml2)
library(rsvg)
library(magick)

# =============================================================================
# 1. SETTINGS — everything you'd want to change lives here
# =============================================================================

icons_dir <- "2026/data/icons"
out_svg <- "pictogram_households.svg"
out_png <- "pictogram_households.png"
out_width_in <- 10 # output PNG width in inches
dpi <- 300 # print resolution

# Canvas (SVG coordinate space)
canvas_w <- 900
margin_l <- 40
margin_r <- 40
margin_t <- 50

# Brazil grid: larger cells
br_cell <- 16 # cell pitch (centre-to-centre, px)
br_icon_size <- 12 # rendered icon size (a bit smaller than cell)

# State grids: smaller cells
st_cell <- 12
st_icon_size <- 9

# Geographies for the states row
# names = code_state values in clean_dat; values = display labels
state_names <- c(
  "35" = "São Paulo",
  "33" = "Rio de Janeiro",
  "31" = "Minas Gerais",
  "43" = "Rio Grande do Sul"
)
state_abbrs <- c(
  "35" = "SP",
  "33" = "RJ",
  "31" = "MG",
  "43" = "RS"
)

# Colours
col_owned <- "#378ADD"
col_rented <- "#639922"
col_other_own <- "#BA7517"
col_ghost_stroke <- "#BBBBBB"
col_text_primary <- "#1A1A1A"
col_text_secondary <- "#5A5A5A"
col_text_tertiary <- "#9A9A9A"
col_border <- "#CCCCCC"
col_label_owned <- "#0C447C"
col_label_rented <- "#3B6D11"
col_label_other <- "#5F3A11"

# Ordering (determines clustering order within each grid)
ownership_levels <- c("Owned", "Rented", "Other")
type_levels <- c("House", "Apartment", "Other")

# =============================================================================
# 2. LOAD ICONS
# Each icon is parsed from a local SVG file. We strip the <svg> wrapper and
# keep only drawable elements so we can re-embed them inline at any size/colour.
# =============================================================================

load_icon <- function(path) {
  doc <- read_xml(path)

  vb_str <- xml_attr(doc, "viewBox")
  vb <- if (is.na(vb_str)) {
    c(
      0,
      0,
      as.numeric(xml_attr(doc, "width")),
      as.numeric(xml_attr(doc, "height"))
    )
  } else {
    as.numeric(str_split(str_trim(vb_str), "[\\s,]+")[[1]])
  }

  drawable <- c(
    "path",
    "rect",
    "polygon",
    "polyline",
    "circle",
    "ellipse",
    "line",
    "g"
  )
  children <- xml_children(doc)
  inner <- paste(
    as.character(children[xml_name(children) %in% drawable]),
    collapse = "\n"
  )
  list(vb = vb, inner = inner)
}

icon_house <- load_icon(file.path(icons_dir, "house.svg"))
icon_apartment <- load_icon(file.path(icons_dir, "apartment.svg"))
icon_other <- load_icon(file.path(icons_dir, "other.svg"))

icons <- list(
  House = icon_house,
  Apartment = icon_apartment,
  Other = icon_other
)

# =============================================================================
# 3. ICON PLACEMENT HELPER
# Produces one <g> SVG fragment that centres an icon at (cx, cy).
# =============================================================================

place_icon <- function(
  icon,
  cx,
  cy,
  size,
  fill = NULL,
  ghost = FALSE,
  opacity = 1
) {
  vb <- icon$vb
  scale <- size / max(vb[3], vb[4])
  tx <- cx - vb[3] * scale / 2 - vb[1] * scale
  ty <- cy - vb[4] * scale / 2 - vb[2] * scale

  style <- if (ghost) {
    glue('fill="none" stroke="{col_ghost_stroke}" stroke-width="{0.8 / scale}"')
  } else if (!is.null(fill)) {
    glue('fill="{fill}" stroke="none"')
  } else {
    ""
  }

  glue(
    '<g transform="translate({tx},{ty}) scale({scale})" opacity="{opacity}">',
    '  <g {style}>{icon$inner}</g>',
    '</g>'
  )
}

# =============================================================================
# 4. LARGEST-REMAINDER ROUNDING
# Distributes n_total counts across categories so integers always sum exactly.
# =============================================================================

largest_remainder <- function(shares, n_total) {
  scaled <- shares / sum(shares) * n_total
  floors <- floor(scaled)
  deficit <- n_total - sum(floors)
  top_idx <- order(scaled - floors, decreasing = TRUE)[seq_len(deficit)]
  floors[top_idx] <- floors[top_idx] + 1
  floors
}

# =============================================================================
# 5. PREPARE DATA
# =============================================================================

dat_br <- clean_dat |>
  filter(code_state == 1) |>
  mutate(
    grouped_property = factor(grouped_property, levels = ownership_levels),
    grouped_type = factor(grouped_type, levels = type_levels)
  )

n_br_fmt <- format(round(sum(dat_br$total) / 1e6, 1), nsmall = 1)

dat_states <- clean_dat |>
  filter(code_state %in% as.integer(names(state_names))) |>
  mutate(
    grouped_property = factor(grouped_property, levels = ownership_levels),
    grouped_type = factor(grouped_type, levels = type_levels)
  )

# =============================================================================
# 6. LAYOUT — all Y positions derived from settings, nothing hardcoded
# =============================================================================

n_states <- length(state_names)
st_panel_w <- floor((canvas_w - margin_l - margin_r) / n_states)
strip_gap <- 16 # vertical gap between ownership strips

br_panel_h <- 30 + 3 * (12 + 10 * br_cell + strip_gap)
st_panel_h <- 26 + 3 * (10 + 10 * st_cell + strip_gap)

y_title <- margin_t
y_legend <- y_title + 22
y_source <- y_legend + 14
y_br <- y_source + 20
y_divider <- y_br + br_panel_h + 14
y_note <- y_divider + 12
y_states <- y_note + 18
canvas_h <- y_states + st_panel_h + 50

# =============================================================================
# 7. BUILD SVG
# We accumulate fragments into `svg`, then paste() at the end.
# Each section is clearly delimited so you can comment out or edit any part.
# =============================================================================

svg <- character(0)

# ── Header + background ───────────────────────────────────────────────────────

svg <- c(
  svg,
  glue(
    '<svg xmlns="http://www.w3.org/2000/svg"',
    ' width="{canvas_w}" height="{canvas_h}"',
    ' viewBox="0 0 {canvas_w} {canvas_h}">',
    '<rect width="{canvas_w}" height="{canvas_h}" fill="white"/>'
  )
)

# ── Title ─────────────────────────────────────────────────────────────────────

svg <- c(
  svg,
  glue(
    '<text x="{canvas_w / 2}" y="{y_title}"',
    ' text-anchor="middle"',
    ' font-family="system-ui,sans-serif" font-size="14" font-weight="600"',
    ' fill="{col_text_primary}">',
    'Households by dwelling type and ownership \u2014 Brazil &amp; selected states',
    '</text>'
  )
)

# ── Legend ─────────────────────────────────────────────────────────────────────
# Type icons on the left, ownership colour swatches on the right

lx <- as.numeric(margin_l)
ly <- as.numeric(y_legend)

for (typ in type_levels) {
  svg <- c(
    svg,
    place_icon(
      icons[[typ]],
      cx = lx + 5,
      cy = ly,
      size = 10,
      fill = col_text_secondary,
      opacity = 0.7
    ),
    glue(
      '<text x="{lx + 13}" y="{ly + 4}"',
      ' font-family="system-ui,sans-serif" font-size="9"',
      ' fill="{col_text_secondary}">{typ}</text>'
    )
  )
  lx <- lx + 13 + nchar(typ) * 5.5 + 8
}

lx <- lx + 14

own_fill_cols <- c(
  Owned = col_owned,
  Rented = col_rented,
  Other = col_other_own
)
own_label_cols <- c(
  Owned = col_label_owned,
  Rented = col_label_rented,
  Other = col_label_other
)
own_display <- c(Owned = "Owned", Rented = "Rented", Other = "Other ownership")

for (own in ownership_levels) {
  svg <- c(
    svg,
    glue(
      '<rect x="{lx}" y="{ly - 5}" width="8" height="8" rx="1"',
      ' fill="{own_fill_cols[own]}" opacity="0.85"/>'
    ),
    glue(
      '<text x="{lx + 11}" y="{ly + 4}"',
      ' font-family="system-ui,sans-serif" font-size="9"',
      ' fill="{col_text_secondary}">{own_display[own]}</text>'
    )
  )
  lx <- lx + 11 + nchar(own_display[own]) * 5.5 + 10
}

# ── Source note ───────────────────────────────────────────────────────────────

svg <- c(
  svg,
  glue(
    '<text x="{canvas_w / 2}" y="{y_source}"',
    ' text-anchor="middle"',
    ' font-family="system-ui,sans-serif" font-size="9"',
    ' fill="{col_text_tertiary}">',
    'IBGE, Censo Demogr\u00e1fico 2022 (SIDRA t/9933) \u00b7 ',
    'Each icon = 1% of total households in that geography',
    '</text>'
  )
)

# =============================================================================
# 8. GRID DRAWING
# Explicit, step-by-step: compute counts → build type sequence → place icons.
# draw_grid() is small and single-purpose; the loop logic is visible here.
# =============================================================================

draw_grid <- function(dat_own, x0, y0, cell, icon_size, fill_col) {
  # Share by type within this ownership group (as % of total geography)
  sh <- setNames(
    sapply(type_levels, function(t) {
      v <- dat_own |> filter(grouped_type == t) |> pull(share)
      if (length(v) == 0) 0 else sum(v)
    }),
    type_levels
  )

  n_filled <- min(100, max(0, round(sum(sh))))

  # Integer counts per type (largest-remainder)
  counts <- largest_remainder(shares = sh, n_total = n_filled)

  # Type sequence: filled cells (House → Apartment → Other), then ghost filler
  type_seq <- c(
    rep("House", counts["House"]),
    rep("Apartment", counts["Apartment"]),
    rep("Other", counts["Other"]),
    rep("House", 100 - n_filled) # ghosts — type is arbitrary
  )

  # Place all 100 icons
  frags <- character(100)
  for (i in seq_len(100)) {
    cx <- x0 + ((i - 1) %% 10) * cell + cell / 2
    cy <- y0 + ((i - 1) %/% 10) * cell + cell / 2
    is_ghost <- i > n_filled
    frags[i] <- place_icon(
      icon = icons[[type_seq[i]]],
      cx = cx,
      cy = cy,
      size = icon_size,
      fill = if (is_ghost) NULL else fill_col,
      ghost = is_ghost,
      opacity = if (is_ghost) 0.3 else 0.88
    )
  }

  paste(frags, collapse = "\n")
}

# ── Brazil panel ──────────────────────────────────────────────────────────────

svg <- c(
  svg,
  glue(
    '<text x="{margin_l}" y="{y_br}"',
    ' font-family="system-ui,sans-serif" font-size="14" font-weight="600"',
    ' fill="{col_text_primary}">Brazil</text>',
    '<text x="{margin_l}" y="{y_br + 16}"',
    ' font-family="system-ui,sans-serif" font-size="9"',
    ' fill="{col_text_secondary}">N = {n_br_fmt}M households</text>'
  )
)

strip_y <- y_br + 32 # top of first strip label

for (own in ownership_levels) {
  dat_own <- dat_br |> filter(grouped_property == own)
  pct <- round(sum(dat_own$share))

  svg <- c(
    svg,
    glue(
      '<text x="{margin_l}" y="{strip_y}"',
      ' font-family="system-ui,sans-serif" font-size="9" font-weight="500"',
      ' fill="{own_label_cols[own]}">{own}  {pct}%</text>'
    ),
    draw_grid(
      dat_own,
      x0 = margin_l,
      y0 = strip_y + 4,
      cell = br_cell,
      icon_size = br_icon_size,
      fill_col = own_fill_cols[own]
    )
  )
  strip_y <- strip_y + 4 + 10 * br_cell + strip_gap
}

# ── Section divider ───────────────────────────────────────────────────────────

svg <- c(
  svg,
  glue(
    '<line x1="{margin_l}" y1="{y_divider}"',
    ' x2="{canvas_w - margin_r}" y2="{y_divider}"',
    ' stroke="{col_border}" stroke-width="0.6"/>'
  ),
  glue(
    '<text x="{canvas_w / 2}" y="{y_note + 4}"',
    ' text-anchor="middle"',
    ' font-family="system-ui,sans-serif" font-size="9"',
    ' fill="{col_text_tertiary}">',
    'Selected states \u2014 each icon = 1% of that state\'s households',
    '</text>'
  )
)

# ── State panels ──────────────────────────────────────────────────────────────

for (i in seq_along(state_names)) {
  code <- names(state_names)[i]
  x0_st <- margin_l + (i - 1) * st_panel_w

  # Dashed vertical divider between panels
  if (i > 1) {
    svg <- c(
      svg,
      glue(
        '<line x1="{x0_st - 4}" y1="{y_states - 8}"',
        ' x2="{x0_st - 4}" y2="{y_states + st_panel_h}"',
        ' stroke="{col_border}" stroke-width="0.5" stroke-dasharray="3 3"/>'
      )
    )
  }

  svg <- c(
    svg,
    glue(
      '<text x="{x0_st}" y="{y_states}"',
      ' font-family="system-ui,sans-serif" font-size="11" font-weight="600"',
      ' fill="{col_text_primary}">{state_names[code]}</text>',
      '<text x="{x0_st}" y="{y_states + 13}"',
      ' font-family="system-ui,sans-serif" font-size="9"',
      ' fill="{col_text_secondary}">{state_abbrs[code]}</text>'
    )
  )

  dat_st <- dat_states |> filter(code_state == as.integer(code))
  strip_y <- y_states + 26

  for (own in ownership_levels) {
    dat_own <- dat_st |> filter(grouped_property == own)
    pct <- round(sum(dat_own$share))

    svg <- c(
      svg,
      glue(
        '<text x="{x0_st}" y="{strip_y}"',
        ' font-family="system-ui,sans-serif" font-size="8" font-weight="500"',
        ' fill="{own_label_cols[own]}">{own}  {pct}%</text>'
      ),
      draw_grid(
        dat_own,
        x0 = x0_st,
        y0 = strip_y + 3,
        cell = st_cell,
        icon_size = st_icon_size,
        fill_col = own_fill_cols[own]
      )
    )
    strip_y <- strip_y + 3 + 10 * st_cell + strip_gap
  }
}

# ── Close SVG ─────────────────────────────────────────────────────────────────

svg <- c(svg, "</svg>")
svg_str <- paste(svg, collapse = "\n")

# 9. WRITE SVG + RENDER PNG
writeLines(svg_str, out_svg)
message("SVG written: ", out_svg)

rsvg::rsvg_png(out_svg, file = out_png, width = dpi * out_width_in)

img <- magick::image_read(out_png)
img <- magick::image_set_dpi(img, density = dpi)
magick::image_write(img, path = out_png, format = "png")

img <- magick::image_read(out_png)
img <- magick::image_set_dpi(img, density = dpi)
magick::image_write(img, path = out_png, format = "png")

message(glue("PNG written: {out_png}  ({dpi * out_width_in}px @ {dpi}dpi)"))
