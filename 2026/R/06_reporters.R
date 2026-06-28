# Prompt: Comparisons — Reporters Without Borders
# Press-freedom dumbbell for Latin America, 2022 vs 2026. Source: RSF index.

library(dplyr)
library(ggplot2)

import::from(tidyr, pivot_wider)
import::from(stringr, str_glue)
import::from(ggtext, element_markdown, element_textbox_simple)
import::from(countrycode, countrycode)
import::from(fs, dir_create, path)
import::from(scales, number)
import::from(forcats, fct_reorder)
import::from(readr, read_delim, locale, write_csv)
import::from(janitor, make_clean_names)
import::from(here, here)

# Data --------------------------------------------------------------------

# RSF World Press Freedom Index, global score (0-100, higher = freer).
# Comparison limited to 2022-2026: RSF revised its methodology in 2022, so
# pre-2022 "abuse scores" are not comparable to the current 0-100 scale.
# Source: Reporters Without Borders (rsf.org/en/index).

# Column names: 2022-2024 use "Score" -> "score"; 2025-2026 use "Score YYYY"
# -> "score_yyyy". Encoding: 2022-2024 UTF-8, 2025+ Windows-1252.
import_rsf <- function(year) {
  url <- str_glue(
    "https://rsf.org/sites/default/files/import_classement/{year}.csv"
  )

  locale <- if (year >= 2025) {
    locale(decimal_mark = ",", encoding = "Windows-1252")
  } else {
    locale(decimal_mark = ",", grouping_mark = ".")
  }

  dat <- read_delim(
    url,
    delim = ";",
    locale = locale,
    name_repair = make_clean_names,
    show_col_types = FALSE
  )

  score_col <- grep("^score", names(dat), value = TRUE)[1]
  dat |>
    select(iso, score = all_of(score_col)) |>
    mutate(year = as.character(year))
}

dat <- bind_rows(lapply(2022:2026, import_rsf)) |>
  mutate(
    country = countrycode(iso, "iso3c", "country.name"),
    region = countrycode(iso, "iso3c", "region")
  )

# Keep a copy alongside the other (gitignored) inputs.
data_dir <- here("2026", "data", "press_freedom")
dir_create(data_dir)
write_csv(dat, path(data_dir, "rsf_scores.csv"))

# Reshape -----------------------------------------------------------------

plot_data <- dat |>
  filter(region == "Latin America & Caribbean") |>
  select(iso, country, year, score) |>
  pivot_wider(
    names_from = "year",
    values_from = "score",
    names_prefix = "score_"
  ) |>
  mutate(
    delta_score = score_2026 - score_2022,
    direction = if_else(delta_score >= 0, "More free", "Less free"),
    is_brazil = iso == "BRA",
    country = fct_reorder(country, score_2026)
  )

gr <- "#1B5E20"

# Bold Brazil's axis label via markdown.
plot_data <- plot_data |>
  mutate(
    label_y = if_else(
      is_brazil,
      str_glue("<b style='color:{gr}'>{country}</b>"),
      as.character(country)
    ),
    label_y = fct_reorder(label_y, score_2026)
  )

# Value label at the 2026 endpoint.
df_ends <- plot_data |>
  mutate(
    label_score = number(score_2026, accuracy = 0.1),
    label_off = if_else(score_2026 >= score_2022, 1.6, -1.6),
    label_h = if_else(score_2026 >= score_2022, 0, 1)
  )

# Plot --------------------------------------------------------------------

offwhite <- "#f8fbf8"
col_up <- "#1E3A5F"
col_dn <- "#C53030"
col_22 <- "gray55"
font_text <- "Roboto Slab"

theme_plot <- theme_minimal(base_family = font_text) +
  theme_sub_plot(
    title = element_text(size = 16, family = "Georgia"),
    title.position = "plot",
    subtitle = element_textbox_simple(
      size = 10,
      color = "gray40",
      margin = margin(b = 12)
    ),
    caption = element_text(size = 8, color = "gray60"),
    caption.position = "plot",
    margin = margin(15, 14, 10, 10),
    background = element_rect(fill = offwhite, color = offwhite)
  ) +
  theme_sub_panel(
    grid.minor = element_blank(),
    grid.major.y = element_blank(),
    background = element_rect(fill = offwhite, color = offwhite)
  ) +
  theme_sub_axis_x(
    ticks = element_line(color = "gray20"),
    line = element_line(color = "gray20", linewidth = 0.5),
    text = element_text(color = "gray20")
  )

dumbbell <- ggplot(plot_data, aes(y = label_y)) +
  geom_segment(
    aes(
      x = score_2022,
      xend = score_2026,
      yend = label_y,
      color = direction
    ),
    linewidth = 1
  ) +
  geom_point(aes(x = score_2022), color = col_22, size = 2.2) +
  geom_text(
    data = df_ends,
    aes(
      x = score_2026 + label_off,
      label = label_score,
      hjust = label_h,
      color = direction
    ),
    family = font_text,
    size = 2.6,
    show.legend = FALSE
  ) +
  scale_x_continuous(
    limits = c(20, 92),
    breaks = seq(20, 90, 10),
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  scale_color_manual(values = c("More free" = col_up, "Less free" = col_dn)) +
  labs(
    title = "Press freedom is sliding across Latin America",
    subtitle = str_glue(
      "Change in Reporters Without Borders' World Press Freedom Index score, ",
      "<b style='color:{col_22}'>2022</b> &rarr; <b>2026</b> ",
      "(0&ndash;100, higher = freer). ",
      "Most of the region <b style='color:{col_dn}'>lost ground</b>; ",
      "<b style='color:{gr}'>Brazil</b> is a rare ",
      "<b style='color:{col_up}'>improver</b>, climbing from 110th to 52nd."
    ),
    caption = paste(
      "Source: Reporters Without Borders — World Press Freedom Index",
      "(2022, 2026). Comparison limited to RSF's post-2022 methodology.",
      "• @viniciusoike"
    ),
    x = NULL,
    y = NULL,
    color = NULL
  ) +
  theme_plot +
  theme(
    legend.position = "none",
    axis.text.y = element_markdown(color = "gray20"),
    axis.text.y.left = element_markdown(color = "gray20")
  )

# Save --------------------------------------------------------------------

ggsave(
  here("2026/plots/06_reporters.png"),
  dumbbell,
  width = 8,
  height = 8,
  dpi = 400
)

# Exploratory (not run) ---------------------------------------------------

# d <- plot_data |>
#   select(country, label_y, score_2022, score_2026) |>
#   pivot_longer(
#     cols = c(score_2022, score_2026),
#     names_sep = "_",
#     names_to = c("variable", "year")
#   ) |>
#   mutate(year = factor(year))

# ggplot(d, aes(value, label_y)) +
#   geom_line(color = "gray55", lwd = 1.2) +
#   geom_point(aes(fill = year), shape = 21, size = 2) +
#   scale_x_continuous(
#     limits = c(20, 92),
#     breaks = seq(20, 90, 10),
#     expand = expansion(mult = c(0.02, 0.04))
#   ) +
#   scale_fill_manual(name = NULL, values = c("#ffffff", col_up)) +
#   labs(
#     title = "Press freedom is sliding across Latin America",
#     caption = paste(
#       "Source: Reporters Without Borders — World Press Freedom Index",
#       "(2022, 2026). Comparison limited to RSF's post-2022 methodology.",
#       "• @viniciusoike"
#     ),
#     x = NULL,
#     y = NULL,
#     color = NULL
#   ) +
#   theme_plot +
#   theme(
#     panel.grid.major.y = element_line(linewidth = 0.2, linetype = 2),
#     legend.position = "top",
#     legend.justification = "left",
#     legend.margin = margin(0, 0, 0, 0),
#     axis.text.y = element_markdown(color = "gray20"),
#     axis.text.y.left = element_markdown(color = "gray20")
#   )

# long_data <- plot_data |>
#   select(iso, is_brazil, score_2022:score_2026) |>
#   pivot_longer(
#     cols = score_2022:score_2026,
#     names_to = c("variable", "year"),
#     names_sep = "_",
#     values_to = "score"
#   ) |>
#   mutate(year = as.numeric(year))

# changes <- long_data |>
#   arrange(year, iso) |>
#   mutate(
#     chg = score - lag(score),
#     .by = "iso"
#   ) |>
#   mutate(
#     chg_norm = chg / max(chg, na.rm = TRUE),
#     .by = "year"
#   )

# ggplot(long_data, aes(year, chg_norm)) +
#   geom_col()
