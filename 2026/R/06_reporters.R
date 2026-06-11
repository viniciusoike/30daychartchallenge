library(dplyr)
library(tidyr)
library(ggplot2)
library(ragg)
library(stringr)
library(ggtext)
library(scales)

import::from(here, here)
import::from(forcats, fct_reorder)

# Data --------------------------------------------------------------------

# RSF World Press Freedom Index, global score (0-100, higher = freer).
# Comparison limited to 2022-2026: RSF revised its methodology in 2022, so
# pre-2022 "abuse scores" are not comparable to the current 0-100 scale.
# Source: Reporters Without Borders (rsf.org/en/index). Verify against RSF
# before publishing.
rsf <- tibble::tribble(
  ~country, ~iso3, ~score_2022, ~rank_2022, ~score_2026, ~rank_2026,
  "Costa Rica",         "CRI", 85.92,   8, 72.35,  38,
  "Uruguay",            "URY", 72.03,  44, 68.72,  48,
  "Dominican Republic", "DOM", 76.90,  30, 69.73,  44,
  "Brazil",             "BRA", 55.36, 110, 66.37,  52,
  "Panama",             "PAN", 62.78,  74, 62.14,  65,
  "Chile",              "CHL", 60.61,  82, 60.84,  70,
  "Paraguay",           "PRY", 58.36,  96, 54.67,  88,
  "Bolivia",            "BOL", 47.58, 126, 54.25,  91,
  "Argentina",          "ARG", 77.28,  29, 52.44,  98,
  "Colombia",           "COL", 42.43, 145, 51.66, 102,
  "Mexico",             "MEX", 47.57, 127, 45.23, 122,
  "Ecuador",            "ECU", 64.61,  68, 44.37, 125,
  "Guatemala",          "GTM", 47.94, 124, 43.21, 128,
  "Honduras",           "HND", 34.61, 165, 41.02, 132,
  "El Salvador",        "SLV", 54.09, 112, 38.88, 143,
  "Peru",               "PER", 61.75,  77, 37.86, 144,
  "Venezuela",          "VEN", 37.78, 159, 30.48, 159,
  "Cuba",               "CUB", 27.32, 173, 29.22, 160,
  "Nicaragua",          "NIC", 37.09, 160, 24.98, 168
)

# Keep a copy of the raw data alongside the other (gitignored) inputs.
data_dir <- here("2026", "data", "press_freedom")
if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)
readr::write_csv(rsf, file.path(data_dir, "rsf_scores.csv"))

# Reshape -----------------------------------------------------------------

plot_data <- rsf |>
  mutate(
    delta = score_2026 - score_2022,
    direction = if_else(delta >= 0, "More free", "Less free"),
    is_brazil = country == "Brazil",
    country = fct_reorder(country, score_2026)
  )

# Bold Brazil's axis label via markdown.
plot_data <- plot_data |>
  mutate(
    label_y = if_else(
      is_brazil,
      str_glue("<b style='color:#1B5E20'>{country}</b>"),
      as.character(country)
    ),
    label_y = fct_reorder(label_y, score_2026)
  )

# Value label at the 2026 endpoint (with rank movement for Brazil).
df_ends <- plot_data |>
  mutate(
    label_score = number(score_2026, accuracy = 0.1),
    label_off = if_else(score_2026 >= score_2022, 1.6, -1.6),
    label_h = if_else(score_2026 >= score_2022, 0, 1)
  )

# Plot --------------------------------------------------------------------

offwhite <- "#fefefe"
col_up <- "#2C7A7B"   # gained press freedom
col_dn <- "#C2410C"   # lost press freedom
col_22 <- "gray55"    # 2022 endpoint

font_text <- "Roboto Slab"

theme_plot <- theme_minimal(base_family = font_text) +
  theme_sub_plot(
    title = element_text(size = 16, family = "Georgia"),
    subtitle = element_textbox_simple(size = 10, color = "gray40", margin = margin(b = 12)),
    caption = element_text(size = 8, color = "gray60"),
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
    aes(x = score_2022, xend = score_2026, yend = label_y, color = direction),
    linewidth = 1,
    arrow = arrow(length = unit(0.07, "inches"), type = "closed")
  ) +
  geom_point(aes(x = score_2022), color = col_22, size = 2.2) +
  geom_text(
    data = df_ends,
    aes(x = score_2026 + label_off, label = label_score, hjust = label_h, color = direction),
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
      "<b style='color:{col_22}'>2022</b> &rarr; <b>2026</b> (0&ndash;100, higher = freer). ",
      "Most of the region <b style='color:{col_dn}'>lost ground</b>; ",
      "<b style='color:#1B5E20'>Brazil</b> is a rare <b style='color:{col_up}'>improver</b>, ",
      "climbing from 110th to 52nd."
    ),
    caption = "Source: Reporters Without Borders — World Press Freedom Index (2022, 2026). Comparison limited to RSF's post-2022 methodology. • @viniciusoike",
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

ggsave(
  here("2026/plots/06_reporters.png"),
  dumbbell,
  width = 8,
  height = 8,
  dpi = 400
)
