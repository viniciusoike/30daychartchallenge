# ══════════════════════════════════════════════════════════════════════════════
#  Rio Grande do Sul — ISOTYPE Chart  (Neurath / Arntz style)
#
#  DESIGN PRINCIPLES (Otto Neurath / Gerd Arntz)
#  ─────────────────────────────────────────────
#  • Solid silhouette icons, single colour per category, no gradients
#  • Horizontal rows: icons read left-to-right
#  • Partial icon for remainder (classic Neurath technique)
#  • Muted earthy palette on cream paper
#  • Geometric sans-serif, all-caps labels, generous white space
#  • Hand-laid layout — NOT facet_grid
#
#  ARCHITECTURE
#  ─────────────
#  1. Download 7 silhouette SVGs from SVGrepo (free, CC0/public domain)
#  2. Recolour each SVG (string replace fill) → save tinted PNG via rsvg
#  3. Build icon grid data frame (full icons + 1 partial per row)
#  4. Partial icon: clip a rasterGrob to fraction × width, inject as annotation
#  5. Draw with ggplot2 + ggimage + patchwork
#
#  INSTALL (once):
#  install.packages(c("tidyverse", "ggimage", "rsvg", "magick",
#                     "showtext", "sysfonts", "glue", "patchwork", "grid"))
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)
library(ggimage)
library(rsvg) # SVG → raster
library(magick) # image recolouring & compositing
library(showtext)
library(sysfonts)
library(glue)
library(patchwork)
library(grid)

# ── Fonts ─────────────────────────────────────────────────────────────────────
font_add_google("Barlow Condensed", "barlow")
font_add_google("Barlow", "barlow_body")
showtext_auto()
showtext_opts(dpi = 300)

# ══════════════════════════════════════════════════════════════════════════════
#  SECTION 1 — SVG DOWNLOAD GUIDE
#  ──────────────────────────────
#  SVGrepo uses CC0 / public domain. Direct download URLs below.
#  Run the downloader block once; after that, files are cached locally.
#
#  Icons chosen for side-profile silhouette style (all from same visual family):
#
#  CROPS:
#    soja   → https://www.svgrepo.com/svg/229491/soybean
#    milho  → https://www.svgrepo.com/svg/229475/corn
#    arroz  → https://www.svgrepo.com/svg/30065/wheat   (wheat sheaf silhouette)
#    trigo  → https://www.svgrepo.com/svg/108557/wheat-grain
#
#  LIVESTOCK:
#    frango → https://www.svgrepo.com/svg/105197/chicken-silhouette
#    suino  → https://www.svgrepo.com/svg/81014/pig-silhouette
#    bovino → https://www.svgrepo.com/svg/112191/cow-silhouette
#
#  NOTE: SVGrepo CDN occasionally changes URLs. If a download fails, visit the
#  URL in a browser, right-click the SVG, copy the direct .svg link, and
#  update svg_urls below. Alternatively, search svgrepo.com for the term and
#  pick any clean silhouette-style result.
# ══════════════════════════════════════════════════════════════════════════════

svg_dir <- "icons_svg"
png_dir <- "icons_png"
dir.create(svg_dir, showWarnings = FALSE)
dir.create(png_dir, showWarnings = FALSE)

svg_urls <- list(
  soja = "https://www.svgrepo.com/download/503572/soybean.svg",
  milho = "https://www.svgrepo.com/download/229475/corn.svg",
  arroz = "https://www.svgrepo.com/download/30065/wheat.svg",
  trigo = "https://www.svgrepo.com/download/108557/wheat-grain.svg",
  frango = "https://www.svgrepo.com/download/105197/chicken-silhouette.svg",
  suino = "https://www.svgrepo.com/download/81014/pig-silhouette.svg",
  bovino = "https://www.svgrepo.com/download/112191/cow-silhouette.svg"
)

# Download SVGs (skip if already present)
# SVGrepo returns 429 to R's default user-agent — spoof a browser header.
# Requires httr: install.packages("httr")
library(httr)

download_svg <- function(name, url) {
  dest <- file.path(svg_dir, paste0(name, ".svg"))
  if (file.exists(dest)) {
    message(glue("  ↩ {name} already cached, skipping"))
    return(invisible(dest))
  }
  message(glue("  ↓ {name}..."))
  resp <- tryCatch(
    httr::GET(
      url,
      httr::user_agent(paste0(
        "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) ",
        "AppleWebKit/537.36 (KHTML, like Gecko) ",
        "Chrome/124.0.0.0 Safari/537.36"
      )),
      httr::add_headers(
        "Accept" = "image/svg+xml,*/*",
        "Accept-Language" = "en-US,en;q=0.9",
        "Referer" = "https://www.svgrepo.com/"
      ),
      httr::timeout(30)
    ),
    error = function(e) {
      warning(glue("  ✗ {name}: {e$message}"))
      NULL
    }
  )
  if (is.null(resp)) {
    return(invisible(NULL))
  }
  if (httr::status_code(resp) != 200) {
    warning(glue("  ✗ {name}: HTTP {httr::status_code(resp)}"))
    return(invisible(NULL))
  }
  writeBin(httr::content(resp, "raw"), dest)
  Sys.sleep(0.8) # be polite — avoid triggering rate limit on next request
  invisible(dest)
}

walk2(names(svg_urls), svg_urls, download_svg)

# ══════════════════════════════════════════════════════════════════════════════
#  SECTION 2 — PALETTE  (Neurath/Arntz earthy tones)
# ══════════════════════════════════════════════════════════════════════════════

# Arntz used a restricted palette: brick red, dark ochre, slate, deep brown, black
# on warm cream. We assign one hue per category.
icon_colours <- c(
  soja = "#8B1A1A", # brick red     — main commodity
  milho = "#B8860B", # dark goldenrod
  arroz = "#2F6B8C", # slate blue
  trigo = "#8B6914", # dark ochre
  frango = "#5C4A1E", # dark brown
  suino = "#7B3F00", # chocolate
  bovino = "#2C4A2E" # dark forest green
)

bg_col <- "#F5F0E8" # warm cream (Neurath poster paper)
rule_col <- "#C8BCA8" # faint ruling line
label_col <- "#1A1208" # near-black
dim_col <- "#9A8E7A" # muted for secondary text

# ══════════════════════════════════════════════════════════════════════════════
#  SECTION 3 — SVG RECOLOURING & PNG EXPORT
#  ──────────────────────────────────────────
#  Strategy: read SVG as text, replace fill/stroke with target colour,
#  render to PNG at 2× resolution via rsvg, then use magick to tint any
#  remaining black pixels to the target colour.
# ══════════════════════════════════════════════════════════════════════════════

recolour_svg_to_png <- function(name, colour, size_px = 256) {
  svg_path <- file.path(svg_dir, paste0(name, ".svg"))
  png_path <- file.path(png_dir, paste0(name, ".png"))

  if (!file.exists(svg_path)) {
    warning(glue("SVG not found for {name}, skipping recolour"))
    return(invisible(NULL))
  }

  # Read SVG text
  svg_txt <- readLines(svg_path, warn = FALSE) |> paste(collapse = "\n")

  # Inject fill colour. We handle three common SVG patterns:
  #   1. fill="..." attribute
  #   2. fill:... in style=""
  #   3. No fill (defaults to black) — inject fill on <svg> root
  svg_txt <- str_replace_all(svg_txt, 'fill="[^"]*"', glue('fill="{colour}"'))
  svg_txt <- str_replace_all(
    svg_txt,
    'fill:#[0-9a-fA-F]{3,6}',
    glue('fill:{colour}')
  )
  svg_txt <- str_replace_all(svg_txt, 'fill:black', glue('fill:{colour}'))
  svg_txt <- str_replace_all(svg_txt, 'fill:none', 'fill:PRESERVE_NONE') # preserve explicit none
  # If no fill at all present on root <svg>, add it
  if (!str_detect(svg_txt, glue('fill="{colour}"'))) {
    svg_txt <- str_replace(svg_txt, "<svg", glue('<svg fill="{colour}"'))
  }
  svg_txt <- str_replace_all(svg_txt, 'fill:PRESERVE_NONE', 'fill:none')
  # Remove strokes (clean silhouette)
  svg_txt <- str_replace_all(svg_txt, 'stroke="[^"]*"', 'stroke="none"')

  # Render to PNG via rsvg
  png_raw <- rsvg::rsvg_png(
    chartr("\n", "\n", svg_txt) |> chartr("", "", x = _),
    width = size_px,
    height = size_px
  )
  # rsvg_png can also take raw bytes — write temp file first
  tmp_svg <- tempfile(fileext = ".svg")
  writeLines(svg_txt, tmp_svg)
  rsvg::rsvg_png(tmp_svg, file = png_path, width = size_px, height = size_px)

  # Use magick to force any remaining near-black pixels → exact colour
  img <- magick::image_read(png_path)
  img <- magick::image_fill(img, colour, point = "+1+1", fuzz = 20) # flood-fill bg transparent
  # Convert black/dark pixels to our colour using colorize
  img <- magick::image_colorize(img, opacity = 100, color = colour)
  magick::image_write(img, path = png_path, format = "png")

  unlink(tmp_svg)
  message(glue("  ✓ {name} → {png_path}"))
  invisible(png_path)
}

message("Recolouring SVGs...")
iwalk(icon_colours, recolour_svg_to_png)

# ══════════════════════════════════════════════════════════════════════════════
#  SECTION 4 — DATA
# ══════════════════════════════════════════════════════════════════════════════

# For a hand-laid Neurath layout we use a single long data frame.
# Each row = one data point (category × period).
# Layout: rows go top-to-bottom, separated by a category divider line.
# Within each category, three sub-rows for the three periods.

period_labels <- c(
  "Pré-enchente (22/23)",
  "Enchentes (23/24)",
  "Pós-enchente (24/25)"
)

# CROPS icon unit = 1 Mt
CROP_UNIT <- 1 # Mt per icon

# LIVESTOCK icon unit = 100 kt
LIVE_UNIT <- 100 # kt per icon

raw_data <- tribble(
  ~group     , ~category  , ~item    , ~period          , ~value , ~unit , ~icon_unit ,
  "Lavouras" , "Lavouras" , "Soja"   , period_labels[1] ,  19.7  , "Mt"  , CROP_UNIT  ,
  "Lavouras" , "Lavouras" , "Soja"   , period_labels[2] ,  19.7  , "Mt"  , CROP_UNIT  ,
  "Lavouras" , "Lavouras" , "Soja"   , period_labels[3] ,  14.6  , "Mt"  , CROP_UNIT  ,
  "Lavouras" , "Lavouras" , "Milho"  , period_labels[1] ,   6.2  , "Mt"  , CROP_UNIT  ,
  "Lavouras" , "Lavouras" , "Milho"  , period_labels[2] ,   4.85 , "Mt"  , CROP_UNIT  ,
  "Lavouras" , "Lavouras" , "Milho"  , period_labels[3] ,   5.51 , "Mt"  , CROP_UNIT  ,
  "Lavouras" , "Lavouras" , "Arroz"  , period_labels[1] ,   7.2  , "Mt"  , CROP_UNIT  ,
  "Lavouras" , "Lavouras" , "Arroz"  , period_labels[2] ,   7.2  , "Mt"  , CROP_UNIT  ,
  "Lavouras" , "Lavouras" , "Arroz"  , period_labels[3] ,   8.3  , "Mt"  , CROP_UNIT  ,
  "Lavouras" , "Lavouras" , "Trigo"  , period_labels[1] ,   5.0  , "Mt"  , CROP_UNIT  ,
  "Lavouras" , "Lavouras" , "Trigo"  , period_labels[2] ,   3.92 , "Mt"  , CROP_UNIT  ,
  "Lavouras" , "Lavouras" , "Trigo"  , period_labels[3] ,   4.09 , "Mt"  , CROP_UNIT  ,
  "Pecuária" , "Pecuária" , "Frango" , period_labels[1] , 738    , "kt"  , LIVE_UNIT  ,
  "Pecuária" , "Pecuária" , "Frango" , period_labels[2] , 785    , "kt"  , LIVE_UNIT  ,
  "Pecuária" , "Pecuária" , "Frango" , period_labels[3] , 820    , "kt"  , LIVE_UNIT  ,
  "Pecuária" , "Pecuária" , "Suíno"  , period_labels[1] , 865    , "kt"  , LIVE_UNIT  ,
  "Pecuária" , "Pecuária" , "Suíno"  , period_labels[2] , 895    , "kt"  , LIVE_UNIT  ,
  "Pecuária" , "Pecuária" , "Suíno"  , period_labels[3] , 920    , "kt"  , LIVE_UNIT  ,
  "Pecuária" , "Pecuária" , "Bovino" , period_labels[1] , 320    , "kt"  , LIVE_UNIT  ,
  "Pecuária" , "Pecuária" , "Bovino" , period_labels[2] , 340    , "kt"  , LIVE_UNIT  ,
  "Pecuária" , "Pecuária" , "Bovino" , period_labels[3] , 355    , "kt"  , LIVE_UNIT
) |>
  mutate(
    item = factor(
      item,
      levels = c("Soja", "Milho", "Arroz", "Trigo", "Frango", "Suíno", "Bovino")
    ),
    period = factor(period, levels = period_labels),
    icon_key = tolower(item), # matches png filename
    icon_path = file.path(png_dir, paste0(tolower(item), ".png")),
    colour = icon_colours[tolower(item)],
    # icon units (continuous)
    n_total = value / icon_unit, # total icons (continuous)
    n_full = floor(n_total), # whole icons
    n_partial = n_total - n_full # fractional remainder (0–1)
  )

# ══════════════════════════════════════════════════════════════════════════════
#  SECTION 5 — EXPAND TO ICON-LEVEL DATA FRAME
#  ────────────────────────────────────────────
#  For ggimage we need one row per icon position.
#  Full icons: opacity = 1
#  Partial icon: we place it at position n_full + 1 with a clip annotation
#  (handled separately via grid::clipGrob in Section 7)
# ══════════════════════════════════════════════════════════════════════════════

ICON_SIZE <- 0.6 # ggimage size units (relative to data coords)
ICON_STEP <- 1.1 # horizontal spacing between icon centres
ROW_HEIGHT <- 1.5 # vertical distance between period sub-rows
GROUP_GAP <- 3.0 # extra vertical gap between crop groups

# Assign y-positions to each (item × period) row
# Layout order: items top-to-bottom, periods within each item
item_levels <- levels(raw_data$item)
period_levels_fac <- levels(raw_data$period)

layout <- raw_data |>
  arrange(item, period) |>
  mutate(
    item_idx = as.integer(item),
    period_idx = as.integer(period),
    # y increases upward in ggplot; we want item 1 at top, so negate
    # Insert a gap between Lavouras and Pecuária groups
    group_offset = if_else(group == "Pecuária", GROUP_GAP, 0),
    y_row = -(item_idx *
      length(period_levels_fac) *
      ROW_HEIGHT +
      period_idx * ROW_HEIGHT +
      group_offset)
  )

# Expand full icons
full_icons <- layout |>
  filter(n_full > 0) |>
  rowwise() |>
  mutate(
    positions = list(tibble(pos = seq_len(n_full)))
  ) |>
  ungroup() |>
  unnest(positions) |>
  mutate(
    x = (pos - 1) * ICON_STEP + ICON_STEP,
    y = y_row,
    alpha_val = 1
  )

# Partial icon positions (one per row where remainder > 0.05)
partial_icons <- layout |>
  filter(n_partial > 0.05) |>
  mutate(
    x = n_full * ICON_STEP + ICON_STEP, # just after last full icon
    y = y_row,
    clip_frac = n_partial # fraction of icon width to show
  )

# ══════════════════════════════════════════════════════════════════════════════
#  SECTION 6 — LABEL DATA FRAMES
# ══════════════════════════════════════════════════════════════════════════════

# Left-side labels: item name (once per item, vertically centred over its 3 rows)
item_labels <- layout |>
  group_by(item, group, colour) |>
  summarise(y_mid = mean(y_row), .groups = "drop") |>
  mutate(x = -0.5)

# Period labels (left strip, one per period per item — show once per item)
period_strip <- layout |>
  select(item, period, y_row, colour) |>
  mutate(x = 0.2)

# Value labels (right of icons)
value_labels <- layout |>
  mutate(
    x_val = (n_full + if_else(n_partial > 0.05, 1.2, 0.2)) *
      ICON_STEP +
      ICON_STEP,
    label = if_else(unit == "Mt", paste0(value, " Mt"), paste0(value, " kt"))
  )

# Group section headers
group_headers <- layout |>
  group_by(group) |>
  summarise(y_top = max(y_row) + ROW_HEIGHT * 0.8, .groups = "drop") |>
  mutate(x = -0.5)

# ══════════════════════════════════════════════════════════════════════════════
#  SECTION 7 — BUILD THE GGPLOT
#  ──────────────────────────────
#  We use ggimage::geom_image() for full icons.
#  Partial icons are implemented with a custom annotation using grid::rasterGrob
#  + grid::clipGrob injected via annotation_custom().
# ══════════════════════════════════════════════════════════════════════════════

# Helper: build a clipped rasterGrob for one partial icon
make_partial_grob <- function(
  png_path,
  frac,
  colour_hex,
  x_centre,
  y_centre,
  x_range,
  y_range,
  icon_w_data,
  icon_h_data
) {
  if (!file.exists(png_path)) {
    return(NULL)
  }

  img <- png::readPNG(png_path) # requires png package

  # Crop image to left `frac` fraction
  w_px <- ncol(img)
  keep <- max(1L, as.integer(w_px * frac))
  img_crop <- img[, seq_len(keep), , drop = FALSE]

  # Convert data coords to npc for annotation_custom
  x_npc <- (x_centre - x_range[1]) / diff(x_range)
  y_npc <- (y_centre - y_range[1]) / diff(y_range)

  w_npc <- icon_w_data / diff(x_range) * frac
  h_npc <- icon_h_data / diff(y_range)

  rg <- grid::rasterGrob(
    img_crop,
    x = unit(x_npc - w_npc / 2, "npc"),
    y = unit(y_npc, "npc"),
    width = unit(w_npc, "npc"),
    height = unit(h_npc, "npc"),
    just = c("left", "centre")
  )
  rg
}

# Determine axis ranges for npc conversion
x_min_plot <- min(full_icons$x, partial_icons$x, na.rm = TRUE) - ICON_STEP
x_max_plot <- max(value_labels$x_val, na.rm = TRUE) + 1
y_min_plot <- min(layout$y_row, na.rm = TRUE) - ROW_HEIGHT
y_max_plot <- max(layout$y_row, na.rm = TRUE) + ROW_HEIGHT * 2

x_range <- c(x_min_plot, x_max_plot)
y_range <- c(y_min_plot, y_max_plot)

# Build base plot
p <- ggplot() +

  # ── Faint horizontal rules behind each row ────────────────────────────────
  geom_hline(
    data = layout |> select(y_row) |> distinct(),
    aes(yintercept = y_row),
    colour = rule_col,
    linewidth = 0.3,
    linetype = "solid"
  ) +

  # ── Full icons ────────────────────────────────────────────────────────────
  geom_image(
    data = full_icons,
    aes(x = x, y = y, image = icon_path),
    size = ICON_SIZE / diff(x_range), # normalise to plot width
    asp = diff(x_range) / diff(y_range)
  ) +

  # ── Period labels (small, muted, left-aligned) ────────────────────────────
  geom_text(
    data = period_strip,
    aes(x = x, y = y_row, label = as.character(period), colour = colour),
    hjust = 1,
    vjust = 0.5,
    family = "barlow_body",
    size = 2.8,
    fontface = "italic"
  ) +
  scale_colour_identity() +

  # ── Item name labels (bold, left margin) ──────────────────────────────────
  geom_text(
    data = item_labels,
    aes(x = x, y = y_mid, label = toupper(as.character(item)), colour = colour),
    hjust = 1,
    vjust = 0.5,
    family = "barlow",
    size = 4.5,
    fontface = "bold"
  ) +

  # ── Value labels (right of icons) ─────────────────────────────────────────
  geom_text(
    data = value_labels,
    aes(x = x_val, y = y_row, label = label, colour = colour),
    hjust = 0,
    vjust = 0.5,
    family = "barlow",
    size = 3.2,
    fontface = "bold"
  ) +

  # ── Group headers ─────────────────────────────────────────────────────────
  geom_text(
    data = group_headers,
    aes(x = x, y = y_top, label = toupper(group)),
    colour = label_col,
    hjust = 1,
    vjust = 0,
    family = "barlow",
    size = 5.5,
    fontface = "bold"
  ) +

  # ── Unit legend (bottom left) ─────────────────────────────────────────────
  annotate(
    "text",
    x = x_min_plot,
    y = y_min_plot + 0.3,
    label = "Lavouras: cada ícone = 1 Mt  |  Pecuária: cada ícone = 100 kt",
    hjust = 0,
    vjust = 0,
    colour = dim_col,
    family = "barlow_body",
    size = 2.8,
    fontface = "italic"
  ) +

  coord_cartesian(
    xlim = x_range,
    ylim = y_range,
    clip = "off"
  ) +
  labs(
    title = "RIO GRANDE DO SUL COMO PRODUTOR AGROPECUÁRIO",
    subtitle = paste0(
      "Três períodos: pré-enchentes (22/23)  ·  ",
      "enchentes de maio/2024 (23/24)  ·  ",
      "pós-enchentes / estiagem (24/25)"
    ),
    caption = paste0(
      "Lavouras: Conab, Séries Históricas + 7º Levantamento 2024/25 (abr/2025).  ",
      "Pecuária: IBGE, Pesquisa Trimestral do Abate (2022–2024) — estimativas RS.\n",
      "Ícones: SVGrepo (CC0). Elaboração própria."
    )
  ) +
  theme_void(base_family = "barlow_body") +
  theme(
    plot.background = element_rect(fill = bg_col, colour = NA),
    plot.title = element_text(
      family = "barlow",
      face = "bold",
      size = 18,
      colour = label_col,
      margin = margin(b = 4, t = 14),
      hjust = 0
    ),
    plot.subtitle = element_text(
      family = "barlow_body",
      size = 10.5,
      colour = dim_col,
      margin = margin(b = 16),
      hjust = 0
    ),
    plot.caption = element_text(
      family = "barlow_body",
      size = 7.5,
      colour = dim_col,
      hjust = 0,
      lineheight = 1.4,
      margin = margin(t = 14, b = 8)
    ),
    plot.margin = margin(16, 32, 12, 32)
  )

# ── Inject partial icons via annotation_custom + grid clipping ───────────────
# Requires png package: install.packages("png")
if (requireNamespace("png", quietly = TRUE) && nrow(partial_icons) > 0) {
  for (i in seq_len(nrow(partial_icons))) {
    row <- partial_icons[i, ]
    grob <- make_partial_grob(
      png_path = row$icon_path,
      frac = row$clip_frac,
      colour_hex = row$colour,
      x_centre = row$x,
      y_centre = row$y,
      x_range = x_range,
      y_range = y_range,
      icon_w_data = ICON_STEP * 0.9,
      icon_h_data = ROW_HEIGHT * 0.85
    )
    if (!is.null(grob)) {
      p <- p +
        annotation_custom(
          grob,
          xmin = -Inf,
          xmax = Inf,
          ymin = -Inf,
          ymax = Inf
        )
    }
  }
}

# ══════════════════════════════════════════════════════════════════════════════
#  SECTION 8 — SAVE
# ══════════════════════════════════════════════════════════════════════════════

ggsave(
  filename = "rs_isotype_neurath.png",
  plot = p,
  width = 20,
  height = 16,
  dpi = 300,
  bg = bg_col
)

message("✓ Saved → rs_isotype_neurath.png")

# ══════════════════════════════════════════════════════════════════════════════
#  TROUBLESHOOTING NOTES
#  ──────────────────────
#  1. SVG download fails   → visit URL in browser, download manually to icons_svg/
#  2. Icons appear black   → the SVG may use <path> with no fill attr; try:
#                            svg_txt <- gsub("<path", '<path fill="COLOUR"', svg_txt)
#  3. Partial icons wrong  → check that png package is installed; if not, partial
#                            icons are simply omitted (rounded values shown)
#  4. Icon size off        → tune ICON_SIZE and ICON_STEP constants above
#  5. Layout too tall/wide → adjust ROW_HEIGHT, GROUP_GAP, ggsave width/height
# ══════════════════════════════════════════════════════════════════════════════
