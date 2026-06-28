# =============================================================================
# pictogram_households_ggsvg_v2.R
#
# Single 10×10 grid per geography. Icons are ordered in ownership blocks
# (Owned → Rented → Other), type-sorted within each block (House → Apt → Other).
# Colour = ownership group. Shape = dwelling type. Ghost = remainder.
#
# Install ggsvg: remotes::install_github("coolbutuseless/ggsvg")
#
# Icons in ./icons/:
#   house.svg     → svgrepo.com/svg/494102/house
#   apartment.svg → svgrepo.com/svg/380557/building-line-architecture
#   other.svg     → svgrepo.com/svg/530598/building
# =============================================================================

library(dplyr)
library(purrr)
library(stringr)
library(ggplot2)
library(ggsvg)

# =============================================================================
# 1. SETTINGS
# =============================================================================

icons_dir <- "2026/data/icons"
out_path <- "pictogram_households_v2.png"
out_width <- 12 # inches
out_height <- 5 # inches — much more compact now
out_dpi <- 300

geo_labels <- c(
  "0" = "Brazil",
  "35" = "São Paulo",
  "33" = "Rio de Janeiro",
  "31" = "Minas Gerais",
  "43" = "Rio Grande do Sul"
)

ownership_levels <- c("Owned", "Rented", "Other")
type_levels <- c("House", "Apartment", "Other")

own_colours <- c(
  Owned = "#378ADD",
  Rented = "#639922",
  Other = "#BA7517"
)
own_label_colours <- c(
  Owned = "#0C447C",
  Rented = "#3B6D11",
  Other = "#5F3A11"
)

ghost_colour <- "#DDDDDD"

icon_size_brazil <- 4.5 # slightly larger for Brazil
icon_size_state <- 4.0

# =============================================================================
# 2. LOAD ICONS
# Wrap inner SVG content in <g id="icon"> for reliable CSS targeting.
# =============================================================================

read_icon_svg <- function(path) {
  raw <- paste(readLines(path, warn = FALSE), collapse = "\n")
  vb <- str_extract(raw, 'viewBox=["\'][^"\']+["\']')
  if (is.na(vb)) {
    vb <- 'viewBox="0 0 24 24"'
  }

  inner <- raw |>
    str_remove_all("(?s)<\\?xml[^>]*>") |>
    str_remove_all("(?s)<!DOCTYPE[^>]*>") |>
    str_remove_all("(?s)<svg[^>]*>") |>
    str_remove_all("(?s)</svg>") |>
    str_remove_all("(?s)<!--.*?-->") |>
    str_trim()

  glue::glue(
    '<svg xmlns="http://www.w3.org/2000/svg" {vb}>',
    '  <g id="icon">{inner}</g>',
    '</svg>'
  )
}

svg_house <- read_icon_svg(file.path(icons_dir, "house.svg"))
svg_apartment <- read_icon_svg(file.path(icons_dir, "apartment.svg"))
svg_other <- read_icon_svg(file.path(icons_dir, "other.svg"))

svg_icons <- list(
  House = svg_house,
  Apartment = svg_apartment,
  Other = svg_other
)

# =============================================================================
# 3. LARGEST-REMAINDER ROUNDING
# =============================================================================

largest_remainder <- function(shares, n_total) {
  n_total <- as.integer(round(n_total))
  if (n_total == 0L || sum(shares) == 0) {
    return(setNames(rep(0L, length(shares)), names(shares)))
  }
  scaled <- shares / sum(shares) * n_total
  floors <- floor(scaled)
  deficit <- n_total - sum(floors)
  if (deficit > 0L) {
    top_idx <- order(scaled - floors, decreasing = TRUE)[seq_len(deficit)]
    floors[top_idx] <- floors[top_idx] + 1L
  }
  as.integer(floors)
}

# =============================================================================
# 4. BUILD LONG TIBBLE
#
# For each geography, we build a flat sequence of 100 icon slots:
#   [ Owned block: House … Apt … Other ] [ Rented block ] [ Other own block ] [ ghosts ]
#
# Each row has: geo, icon_idx (1–100), type, ownership, ghost, fill_col, size
# Grid position is derived from icon_idx: col = (i-1)%%10+1, row = (i-1)%/%10+1
# =============================================================================

build_icon_data <- function(clean_dat) {
  map_dfr(names(geo_labels), function(geo_code) {
    dat_geo <- clean_dat |> filter(code_state == as.integer(geo_code))
    if (nrow(dat_geo) == 0) {
      return(NULL)
    }

    # For each ownership group, get type shares and compute icon counts
    blocks <- map(ownership_levels, function(own) {
      dat_own <- dat_geo |> filter(grouped_property == own)

      sh <- setNames(
        map_dbl(type_levels, function(t) {
          v <- dat_own |> filter(grouped_type == t) |> pull(share)
          if (length(v) == 0) 0 else sum(v)
        }),
        type_levels
      )

      n_own <- round(sum(sh)) # total icons for this ownership group
      counts <- largest_remainder(sh, n_own)

      # Return a sequence of (type, ownership) pairs for this block
      tibble(
        type = c(
          rep("House", counts["House"]),
          rep("Apartment", counts["Apartment"]),
          rep("Other", counts["Other"])
        ),
        ownership = own,
        ghost = FALSE
      )
    })

    filled <- bind_rows(blocks) # up to 100 rows

    # Pad to exactly 100 with ghost icons
    n_ghost <- 100L - nrow(filled)
    if (n_ghost > 0) {
      ghosts <- tibble(
        type = rep("House", n_ghost),
        ownership = NA_character_,
        ghost = TRUE
      )
      filled <- bind_rows(filled, ghosts)
    }

    filled |>
      mutate(
        geo = geo_labels[geo_code],
        geo_code = geo_code,
        icon_idx = row_number(),
        col = (icon_idx - 1L) %% 10L + 1L,
        row = (icon_idx - 1L) %/% 10L + 1L,
        fill_col = if_else(ghost, ghost_colour, own_colours[ownership]),
        size = if_else(geo_code == "0", icon_size_brazil, icon_size_state)
      )
  }) |>
    mutate(
      geo = factor(geo, levels = geo_labels),
      ownership = factor(ownership, levels = ownership_levels),
      type = factor(type, levels = type_levels)
    )
}

icon_data <- build_icon_data(clean_dat)

# =============================================================================
# 5. OWNERSHIP SHARE ANNOTATIONS
# Shown below each grid as a small coloured breakdown: "Owned 74%  Rented 18%  Other 8%"
# We'll place these as individual geom_text() calls, one per ownership group.
# =============================================================================

share_annot <- icon_data |>
  filter(!ghost) |>
  count(geo, geo_code, ownership) |>
  mutate(
    label = paste0(ownership, " ", n, "%"),
    label_col = own_label_colours[as.character(ownership)],
    # x position: stagger the three labels across the grid width
    x_pos = case_when(
      ownership == "Owned" ~ 2,
      ownership == "Rented" ~ 5.5,
      ownership == "Other" ~ 8.5
    )
  )

# =============================================================================
# 6. PLOT
#
# One facet per geography (single row of panels).
# Three geom_point_svg() layers — one per dwelling type.
# fill_col in the data is already a hex colour → scale_svg_fill_identity().
# =============================================================================

make_svg_layer <- function(type_name) {
  list(
    geom_point_svg(
      data = filter(icon_data, type == type_name),
      mapping = aes(x = col, y = -row, css("g#icon", fill = fill_col)),
      svg = svg_icons[[type_name]],
      size = filter(icon_data, type == type_name)$size[1]
    ),
    scale_svg_fill_identity(
      aesthetics = css("g#icon", fill = fill_col)
    )
  )
}

p <- ggplot() +

  # Icon layers
  make_svg_layer("House") +
  make_svg_layer("Apartment") +
  make_svg_layer("Other") +

  # Ownership breakdown labels below each grid
  geom_text(
    data = share_annot,
    mapping = aes(x = x_pos, y = -11.2, label = label, colour = ownership),
    size = 2.2,
    hjust = 0.5,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  scale_colour_manual(values = own_label_colours, guide = "none") +

  # One column per geography
  facet_wrap(~geo, nrow = 1) +

  coord_equal(xlim = c(0.5, 10.5), ylim = c(-11.8, -0.5), clip = "off") +

  theme_void(base_family = "sans") +
  theme(
    strip.text = element_text(size = 9, face = "bold", margin = margin(b = 5)),
    panel.spacing = unit(0.5, "cm"),
    plot.margin = margin(10, 12, 10, 12),
    plot.title = element_text(size = 12, face = "bold", margin = margin(b = 3)),
    plot.subtitle = element_text(
      size = 8,
      colour = "grey50",
      margin = margin(b = 8)
    ),
    plot.caption = element_text(
      size = 7,
      colour = "grey60",
      margin = margin(t = 6)
    )
  ) +

  labs(
    title = "Households by dwelling type and ownership",
    subtitle = "Brazil and selected states · each icon = 1% of total households · colour = ownership · shape = dwelling type",
    caption = "Source: IBGE, Censo Demográfico 2022 (SIDRA t/9933)"
  )

# =============================================================================
# 7. LEGEND STRIP (cowplot annotation row)
# A compact single-row legend: shape icons + ownership colour swatches.
# =============================================================================

if (requireNamespace("cowplot", quietly = TRUE)) {
  library(cowplot)

  leg_type <- tibble(
    x = c(1, 2.2, 3.4),
    y = 1,
    type = c("House", "Apartment", "Other"),
    col = "grey35"
  )
  leg_own <- tibble(
    x = c(5, 6.4, 7.8),
    y = 1,
    lab = c("Owned", "Rented", "Other ownership"),
    col = unname(own_colours)
  )

  legend_row <- ggplot() +
    # Type icons
    geom_point_svg(
      data = filter(leg_type, type == "House"),
      aes(x, y, css("g#icon", fill = col)),
      svg = svg_house,
      size = 3
    ) +
    scale_svg_fill_identity(aesthetics = css("g#icon", fill = col)) +
    geom_point_svg(
      data = filter(leg_type, type == "Apartment"),
      aes(x, y, css("g#icon", fill = col)),
      svg = svg_apartment,
      size = 3
    ) +
    scale_svg_fill_identity(aesthetics = css("g#icon", fill = col)) +
    geom_point_svg(
      data = filter(leg_type, type == "Other"),
      aes(x, y, css("g#icon", fill = col)),
      svg = svg_other,
      size = 3
    ) +
    scale_svg_fill_identity(aesthetics = css("g#icon", fill = col)) +
    geom_text(
      data = leg_type,
      aes(x + 0.12, y, label = type),
      hjust = 0,
      size = 2.5,
      colour = "grey35"
    ) +
    # Ownership swatches
    geom_point(
      data = leg_own,
      aes(x, y, fill = col),
      shape = 22,
      size = 6,
      colour = NA
    ) +
    scale_fill_identity() +
    geom_text(
      data = leg_own,
      aes(x + 0.15, y, label = lab),
      hjust = 0,
      size = 2.5,
      colour = "grey35"
    ) +
    xlim(0.5, 10) +
    ylim(0.5, 1.5) +
    theme_void()

  final <- plot_grid(
    p,
    legend_row,
    ncol = 1,
    rel_heights = c(10, 0.8)
  )
} else {
  final <- p
}

# =============================================================================
# 8. SAVE
# =============================================================================

ggsave(
  out_path,
  final,
  width = out_width,
  height = out_height,
  dpi = out_dpi,
  bg = "white"
)

message("Saved: ", out_path)
