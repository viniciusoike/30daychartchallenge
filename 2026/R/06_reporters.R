library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(ggtext)
library(fs)
library(countrycode)
library(scales)

import::from(here, here)
import::from(forcats, fct_reorder)

# Data --------------------------------------------------------------------

years <- 2022:2026

urls <- str_glue(
  "https://rsf.org/sites/default/files/import_classement/{years}.csv"
)

import_csv <- function(x, ...) {
  readr::read_delim(
    file = x,
    delim = ";",
    locale = readr::locale(decimal_mark = ","),
    name_repair = janitor::make_clean_names,
    ...
  )
}

safe_import_csv <- purrr::safely(import_csv)

files <- purrr::map(urls, safe_import_csv)

# RSF World Press Freedom Index, global score (0-100, higher = freer).
# Comparison limited to 2022-2026: RSF revised its methodology in 2022, so
# pre-2022 "abuse scores" are not comparable to the current 0-100 scale.
# Source: Reporters Without Borders (rsf.org/en/index).

# very strange csv
url22 <- "https://rsf.org/sites/default/files/import_classement/2022.csv"
url26 <- "https://rsf.org/sites/default/files/import_classement/2026.csv"

data_dir <- here("2026", "data", "press_freedom")

if (!dir_exists(data_dir)) {
  dir_create(data_dir)
}
# csv corrupts when downloading
# download.file(url22, destfile = path(data_dir, "2022.csv"))
download.file(url26, destfile = path(data_dir, "2026.csv"), mode = "wb")

dat22 <- readr::read_delim(
  url22,
  delim = ";",
  locale = readr::locale(decimal_mark = ",", grouping_mark = "."),
  name_repair = janitor::make_clean_names
)

dat26 <- readr::read_delim(
  path(data_dir, "2026.csv"),
  delim = ";",
  locale = readr::locale(decimal_mark = ",", encoding = "Windows-1252"),
  name_repair = janitor::make_clean_names
)

dat <- bind_rows(
  list(
    "2026" = select(dat26, iso, score = score_2026, rank),
    "2022" = select(dat22, iso, score, rank)
  ),
  .id = "year"
)


dat <- dat |>
  mutate(
    country = countrycode(iso, "iso3c", "country.name"),
    region = countrycode(iso, "iso3c", "region")
  )

# Keep a copy of the raw data alongside the other (gitignored) inputs.
readr::write_csv(dat, path(data_dir, "rsf_scores.csv"))

# Reshape -----------------------------------------------------------------

rsf <- readr::read_csv(path(data_dir, "rsf_scores.csv"))

rsf <- rsf |>
  filter(region == "Latin America & Caribbean") |>
  pivot_wider(
    id_cols = c("iso", "region", "country"),
    names_from = "year",
    values_from = c("score", "rank")
  ) |>
  mutate(
    delta_score = score_2026 - score_2022,
    delta_rank = rank_2026 - rank_2022,
    direction_score = if_else(delta_score >= 0, "More free", "Less free"),
    direction_rank = if_else(delta_rank >= 0, "More free", "Less free"),
    is_brazil = factor(if_else(iso == "BRA", 1, 0)),
    country = fct_reorder(country, score_2026)
  )

gr <- ekioplot::ekio_pal("green")[8]

# Bold Brazil's axis label via markdown.
plot_data <- rsf |>
  mutate(
    label_y = if_else(
      is_brazil == 1,
      str_glue("<b style='color:{gr}'>{country}</b>"),
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

offwhite <- "#f8fbf8"

ekioplot::ekio_pal("blue_red")

col_up <- "#1E3A5F" # gained press freedom
col_dn <- "#C53030" # lost press freedom
col_22 <- "gray55" # 2022 endpoint

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

dat <- plot_data |>
  select(country, label_y, score_2022, score_2026) |>
  pivot_longer(
    cols = c(score_2022, score_2026),
    names_sep = "_",
    names_to = c("variable", "year")
  ) |>
  mutate(year = factor(year))

ggplot(dat, aes(value, label_y)) +
  geom_line(color = "gray55", lwd = 1.2) +
  geom_point(aes(fill = year), shape = 21, size = 2) +
  scale_x_continuous(
    limits = c(20, 92),
    breaks = seq(20, 90, 10),
    expand = expansion(mult = c(0.02, 0.04))
  ) +
  scale_fill_manual(name = NULL, values = c("#ffffff", col_up)) +
  labs(
    title = "Press freedom is sliding across Latin America",
    caption = "Source: Reporters Without Borders — World Press Freedom Index (2022, 2026). Comparison limited to RSF's post-2022 methodology. • @viniciusoike",
    x = NULL,
    y = NULL,
    color = NULL
  ) +
  theme_plot +
  theme(
    panel.grid.major.y = element_line(linewidth = 0.2, linetype = 2),
    legend.position = "top",
    legend.justification = "left",
    legend.margin = margin(0, 0, 0, 0),
    axis.text.y = element_markdown(color = "gray20"),
    axis.text.y.left = element_markdown(color = "gray20")
  )


ggplot(plot_data) +
  geom_segment(
    aes(
      x = score_2022,
      xend = score_2026,
      y = label_y,
      yend = label_y,
      color = direction_score
    )
  ) +
  geom_point(aes(x = score_2026, y = label_y)) +
  scale_color_manual(
    values = c("More free" = col_up, "Less free" = col_dn)
  )


ggplot(plot_data, aes(y = label_y)) +
  geom_segment(
    aes(
      x = score_2022,
      xend = score_2026,
      yend = label_y,
      color = direction_score
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
      color = direction_score
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
