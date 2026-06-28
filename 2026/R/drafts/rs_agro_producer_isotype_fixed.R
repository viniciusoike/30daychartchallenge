# ══════════════════════════════════════════════════════════════════════════════
#  RS Agro Producer — Isotype Chart (fixed, no geom_pictogram)
#
#  WHY NO geom_pictogram?
#  StatWaffle (used internally by geom_pictogram) calls rep(x, times = values),
#  which requires integer values AND breaks with facet_grid(rows ~ cols).
#  Both conditions apply here → we pre-build the icon grid ourselves and use
#  plain geom_text(), which is more robust and gives identical output.
#
#  ICON STRATEGY (choose one, see section "Icon setup" below):
#    A) emojifont  → Font Awesome glyphs via fontawesome("fa-*") lookup
#    B) Unicode emoji → zero extra deps, works on macOS/Linux natively
#
#  INSTALL:
#    install.packages(c("tidyverse", "ggtext", "showtext",
#                       "sysfonts", "glue", "patchwork"))
#    install.packages("emojifont")   # for option A
# ══════════════════════════════════════════════════════════════════════════════

library(tidyverse)
library(showtext)
library(sysfonts)
library(ggtext)
library(glue)
library(patchwork)

# ── Icon strategy: set to "fa" or "emoji" ─────────────────────────────────────
ICON_MODE <- "emoji"   # ← change to "fa" if emojifont is installed

if (ICON_MODE == "fa") {
  library(emojifont)
  load.fontawesome()   # loads FA font into showtext
}

# ── Fonts ─────────────────────────────────────────────────────────────────────
font_add_google("Barlow Condensed", "barlow")
font_add_google("Barlow",           "barlow_body")
showtext_auto()
showtext_opts(dpi = 300)

# ── Palette ───────────────────────────────────────────────────────────────────
pal_crops <- c(
  "Soja"   = "#2d7a1f",
  "Milho"  = "#c8940a",
  "Arroz"  = "#1a6fa3",
  "Trigo"  = "#a0541a"
)
pal_livestock <- c(
  "Frango" = "#7d3c98",
  "Suíno"  = "#c0392b",
  "Bovino" = "#6e4a1e"
)

bg_col      <- "#f8f6ef"
panel_col   <- "#eeeade"
text_col    <- "#1c1c2e"
subtext_col <- "#6b6b80"
caption_col <- "#9999aa"

# ── Period factor levels ───────────────────────────────────────────────────────
period_levels <- c(
  "Pré-enchente\n(22/23)",
  "Enchentes\n(23/24)",
  "Pós-enchente\n(24/25)"
)

# ── Data ──────────────────────────────────────────────────────────────────────
# Grains: Conab (safra year, RS only)
# Livestock: IBGE Pesquisa Trimestral do Abate (calendar year, RS only)
#   RS frango ≈ 12% of national; suíno ≈ 17%; bovino from IBGE UF breakdown
#
# GRAINS icon unit  = 1 Mt    (values already in Mt)
# LIVESTOCK icon unit = 100kt  (values stored in kt, divided below)

crops_raw <- tribble(
  ~crop,   ~period,                    ~value,
  "Soja",  "Pré-enchente\n(22/23)",    19.7,
  "Soja",  "Enchentes\n(23/24)",       19.7,   # flood hit AFTER soja harvest
  "Soja",  "Pós-enchente\n(24/25)",    14.6,   # drought/heat collapse
  "Milho", "Pré-enchente\n(22/23)",     6.2,
  "Milho", "Enchentes\n(23/24)",        4.85,
  "Milho", "Pós-enchente\n(24/25)",     5.51,
  "Arroz", "Pré-enchente\n(22/23)",     7.2,
  "Arroz", "Enchentes\n(23/24)",        7.2,
  "Arroz", "Pós-enchente\n(24/25)",     8.3,
  "Trigo", "Pré-enchente\n(22/23)",     5.0,
  "Trigo", "Enchentes\n(23/24)",        3.92,  # flood delayed winter planting
  "Trigo", "Pós-enchente\n(24/25)",     4.09
)

LSTOCK_UNIT_KT <- 100   # kt per icon in livestock panel

livestock_raw <- tribble(
  ~crop,    ~period,                    ~value_kt,
  "Frango", "Pré-enchente\n(22/23)",    738,
  "Frango", "Enchentes\n(23/24)",       785,
  "Frango", "Pós-enchente\n(24/25)",    820,
  "Suíno",  "Pré-enchente\n(22/23)",    865,
  "Suíno",  "Enchentes\n(23/24)",       895,
  "Suíno",  "Pós-enchente\n(24/25)",    920,
  "Bovino", "Pré-enchente\n(22/23)",    320,
  "Bovino", "Enchentes\n(23/24)",       340,
  "Bovino", "Pós-enchente\n(24/25)",    355
) |>
  mutate(value = value_kt / LSTOCK_UNIT_KT)   # icon units

# Factor ordering
crops_raw <- crops_raw |>
  mutate(
    crop   = factor(crop,   levels = c("Soja", "Milho", "Arroz", "Trigo")),
    period = factor(period, levels = period_levels)
  )

livestock_raw <- livestock_raw |>
  mutate(
    crop   = factor(crop,   levels = c("Frango", "Suíno", "Bovino")),
    period = factor(period, levels = period_levels)
  )

# ── Icon lookup ───────────────────────────────────────────────────────────────
# FA mode: emojifont::fontawesome("fa-seedling") returns the Unicode glyph
# Emoji mode: direct Unicode characters

if (ICON_MODE == "fa") {
  crop_icons <- c(
    "Soja"   = fontawesome("fa-seedling"),
    "Milho"  = fontawesome("fa-tree"),        # no corn in FA free
    "Arroz"  = fontawesome("fa-leaf"),
    "Trigo"  = fontawesome("fa-spa")          # no wheat in FA free v5
  )
  livestock_icons <- c(
    "Frango" = fontawesome("fa-feather"),
    "Suíno"  = fontawesome("fa-circle"),      # no pig in FA free
    "Bovino" = fontawesome("fa-paw")
  )
  icon_family <- "fontawesome-webfont"
} else {
  crop_icons <- c(
    "Soja"   = "\U0001F331",   # 🌱
    "Milho"  = "\U0001F33D",   # 🌽
    "Arroz"  = "\U0001F33E",   # 🌾
    "Trigo"  = "\U0001F33F"    # 🌿
  )
  livestock_icons <- c(
    "Frango" = "\U0001F413",   # 🐓
    "Suíno"  = "\U0001F437",   # 🐷
    "Bovino" = "\U0001F404"    # 🐄
  )
  icon_family <- ""  # system emoji font
}

# ── Grid builder ──────────────────────────────────────────────────────────────
# Expands each data row into N icon positions (col 1–5, rows stacking upward).
# Key fix vs. geom_pictogram: we round() to integer BEFORE expansion,
# guard against n = 0, and never pass floats to rep().

ICONS_PER_ROW <- 5

build_grid <- function(df, icon_map) {
  icon_tbl <- enframe(icon_map, name = "crop", value = "glyph")

  df |>
    mutate(n = pmax(1L, as.integer(round(value)))) |>   # integer, min 1
    left_join(icon_tbl, by = "crop") |>
    rowwise() |>
    mutate(
      positions = list(
        tibble(
          col_pos = ((seq_len(n) - 1L) %% ICONS_PER_ROW) + 1L,
          row_pos = ((seq_len(n) - 1L) %/% ICONS_PER_ROW) + 1L
        )
      )
    ) |>
    ungroup() |>
    unnest(positions)
}

crops_grid     <- build_grid(crops_raw,     crop_icons)
livestock_grid <- build_grid(livestock_raw, livestock_icons)

# ── Value label data (one row per crop × period) ──────────────────────────────
make_label_df <- function(df, fmt_fn) {
  df |>
    mutate(
      n       = pmax(1L, as.integer(round(value))),
      row_max = ceiling(n / ICONS_PER_ROW),
      label_y = row_max + 1.1,
      label   = fmt_fn(df)
    )
}

crop_label_df <- make_label_df(
  crops_raw,
  function(d) paste0(d$value, " Mt")
)

lstock_label_df <- make_label_df(
  livestock_raw,
  function(d) paste0(d$value_kt, " kt")
)

# ── Isotype panel factory ─────────────────────────────────────────────────────
make_panel <- function(grid_df, label_df, pal, title, subtitle) {

  ggplot() +
    # icons
    geom_text(
      data    = grid_df,
      mapping = aes(x = col_pos, y = row_pos, label = glyph, color = crop),
      family  = icon_family,
      size    = 7,
      vjust   = 0.5,
      hjust   = 0.5
    ) +
    # value labels
    geom_text(
      data    = label_df,
      mapping = aes(x = 3, y = label_y, label = label, color = crop),
      family  = "barlow",
      fontface = "bold",
      size    = 4.8
    ) +
    scale_color_manual(values = pal, guide = "none") +
    scale_x_continuous(limits = c(0.2, 5.8), expand = expansion(0)) +
    scale_y_continuous(expand = expansion(mult = c(0.08, 0.38))) +
    facet_grid(
      rows   = vars(period),
      cols   = vars(crop),
      switch = "y"
    ) +
    labs(title = title, subtitle = subtitle) +
    theme_void(base_family = "barlow_body") +
    theme(
      plot.background  = element_rect(fill = bg_col,    color = NA),
      panel.background = element_rect(fill = panel_col, color = NA),
      strip.text.x = element_text(
        family = "barlow", face = "bold", size = 14,
        color  = text_col, margin = margin(b = 6, t = 5)
      ),
      strip.text.y.left = element_text(
        family = "barlow_body", size = 9, color = subtext_col,
        angle  = 0, hjust = 1, margin = margin(r = 10)
      ),
      plot.title = element_text(
        family = "barlow", face = "bold", size = 14,
        color  = text_col, margin = margin(b = 2, t = 4)
      ),
      plot.subtitle = element_text(
        family = "barlow_body", size = 9.5, color = subtext_col,
        margin = margin(b = 6)
      ),
      panel.spacing.x = unit(0.7, "lines"),
      panel.spacing.y = unit(0.4, "lines"),
      plot.margin     = margin(8, 16, 4, 16)
    )
}

p_crops <- make_panel(
  grid_df  = crops_grid,
  label_df = crop_label_df,
  pal      = pal_crops,
  title    = "Lavouras — produção safra RS",
  subtitle = "Cada ícone = 1 Mt  |  Fonte: Conab, Séries Históricas + 7º Lev. 2024/25 (abr/2025)"
)

p_livestock <- make_panel(
  grid_df  = livestock_grid,
  label_df = lstock_label_df,
  pal      = pal_livestock,
  title    = "Pecuária — abate RS (peso carcaça)",
  subtitle = glue(
    "Cada ícone = {LSTOCK_UNIT_KT} kt  |  ",
    "Fonte: IBGE, Pesquisa Trimestral do Abate (ano civil 2022–2024)"
  )
)

# ── Compose ───────────────────────────────────────────────────────────────────
p_final <- (p_crops / p_livestock) +
  plot_annotation(
    title    = "Rio Grande do Sul como produtor agropecuário",
    subtitle = paste0(
      "Três períodos: pré-enchentes (safra 22/23)  •  ",
      "enchentes de maio/2024 (safra 23/24)  •  ",
      "pós-enchentes / estiagem (safra 24/25)\n",
      "Escalas de ícones distintas entre os dois painéis."
    ),
    caption  = paste0(
      "Lavouras: Conab, Séries Históricas e 7º Levantamento Safra 2024/25 (abr/2025) — RS.\n",
      "Pecuária: IBGE, Pesquisa Trimestral do Abate de Animais (2022–2024) — RS. ",
      "Frango estimado com share ~12% do nacional; suíno ~17% (fonte: IBGE UF breakdown).\n",
      "Elaboração própria."
    ),
    theme = theme(
      plot.background = element_rect(fill = bg_col, color = NA),
      plot.title = element_text(
        family = "barlow", face = "bold", size = 21,
        color  = text_col, margin = margin(b = 5, t = 10)
      ),
      plot.subtitle = element_text(
        family = "barlow_body", size = 11, color = subtext_col,
        lineheight = 1.35, margin = margin(b = 2)
      ),
      plot.caption = element_text(
        family = "barlow_body", size = 8, color = caption_col,
        hjust = 0, lineheight = 1.4, margin = margin(t = 10, b = 8)
      ),
      plot.margin = margin(10, 20, 8, 20)
    )
  )

# ── Save ──────────────────────────────────────────────────────────────────────
out_file <- "rs_agro_producer_isotype.png"

ggsave(
  filename = out_file,
  plot     = p_final,
  width    = 18,
  height   = 14,
  dpi      = 300,
  bg       = bg_col
)

message(glue("✓ Saved → {out_file}"))
