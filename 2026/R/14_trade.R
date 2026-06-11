library(dplyr)
library(ggplot2)
library(ragg)
library(ggtext)
library(scales)
library(stringr)

import::from(here, here)
import::from(data.table, fread)
import::from(countrycode, countrycode)
import::from(ggrepel, geom_text_repel)

# Data --------------------------------------------------------------------

# Comex Stat / MDIC full-year bulk files: one row per NCM x partner x state x
# month. We only need the FOB value (US$) and the partner country code, summed
# to a single export and import total per partner. Files are large (~100-170MB)
# and gitignored, so cache them locally and only download once.
ref_year <- 2024

data_dir <- here("2026", "data", "comex")
if (!dir.exists(data_dir)) dir.create(data_dir, recursive = TRUE)

comex_base <- "https://balanca.economia.gov.br/balanca/bd"
files <- c(
  exports = sprintf("%s/comexstat-bd/ncm/EXP_%d.csv", comex_base, ref_year),
  imports = sprintf("%s/comexstat-bd/ncm/IMP_%d.csv", comex_base, ref_year),
  pais    = sprintf("%s/tabelas/PAIS.csv", comex_base)
)

local_path <- function(url) file.path(data_dir, basename(url))

options(timeout = max(1200, getOption("timeout")))
for (url in files) {
  dest <- local_path(url)
  if (!file.exists(dest)) {
    download.file(url, dest, mode = "wb", quiet = FALSE)
  }
}

# Sum FOB value by partner for one flow (exports or imports). Read only the two
# columns we need so the large files stay light in memory.
read_flow <- function(flow) {
  fread(
    local_path(files[[flow]]),
    sep = ";",
    select = c("CO_PAIS", "VL_FOB"),
    colClasses = c(CO_PAIS = "integer", VL_FOB = "numeric"),
    encoding = "Latin-1"
  ) |>
    as_tibble() |>
    summarise(value = sum(VL_FOB, na.rm = TRUE), .by = "CO_PAIS") |>
    rename(!!flow := value)
}

exports <- read_flow("exports")
imports <- read_flow("imports")

# Partner lookup: MDIC code -> ISO-3 + English name.
pais <- fread(
  local_path(files[["pais"]]),
  sep = ";",
  select = c("CO_PAIS", "CO_PAIS_ISOA3", "NO_PAIS_ING"),
  colClasses = c(CO_PAIS = "integer"),
  encoding = "Latin-1"
) |>
  as_tibble() |>
  rename(iso3 = CO_PAIS_ISOA3, country = NO_PAIS_ING)

# Reshape -----------------------------------------------------------------

trade <- exports |>
  full_join(imports, by = "CO_PAIS") |>
  left_join(pais, by = "CO_PAIS") |>
  mutate(across(c(exports, imports), \(x) coalesce(x, 0))) |>
  # Drop non-country aggregates (bunkers, "not defined", special zones): they
  # have no real ISO-3 and would distort the partner ranking.
  filter(!is.na(iso3), iso3 != "ZZZ") |>
  mutate(continent = countrycode(iso3, "iso3c", "continent")) |>
  filter(!is.na(continent)) |>
  mutate(
    # Split the Americas so Brazil's regional partners read clearly.
    region = case_when(
      continent != "Americas" ~ continent,
      iso3 %in% c("USA", "CAN") ~ "North America",
      TRUE ~ "Latin America"
    )
  ) |>
  mutate(
    exports = exports / 1e9,   # US$ billion FOB
    imports = imports / 1e9,
    total = exports + imports,
    balance = exports - imports
  )

# Keep the partners that actually drive Brazil's trade.
plot_data <- trade |>
  slice_max(total, n = 28) |>
  mutate(
    is_key = iso3 %in% c("CHN", "USA"),
    face = if_else(is_key, "bold", "plain")
  )

# Plot --------------------------------------------------------------------

offwhite <- "#fefefe"
col_surplus <- "#2C7A7B"   # Brazil sells more than it buys
col_deficit <- "#C2410C"   # Brazil buys more than it sells

font_text <- "Roboto Slab"

region_pal <- c(
  "Asia" = "#B45309",
  "Latin America" = "#2C7A7B",
  "North America" = "#9D174D",
  "Europe" = "#1D4ED8",
  "Africa" = "#6D28D9",
  "Oceania" = "#475569"
)

# Shared limits so the 45-degree balanced-trade line reads diagonally.
lim <- range(c(plot_data$exports, plot_data$imports))
lim <- c(lim[1] * 0.6, lim[2] * 1.4)

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
    background = element_rect(fill = offwhite, color = offwhite)
  ) +
  theme_sub_legend(
    position = "bottom",
    title = element_text(size = 9, family = "Lora")
  )

bubbles <- ggplot(plot_data, aes(imports, exports)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray55") +
  annotate(
    "richtext", x = lim[1] * 1.15, y = lim[2] * 0.85, hjust = 0,
    label = sprintf("<b style='color:%s'>Brazil sells more<br>than it buys</b>", col_surplus),
    family = font_text, size = 3, fill = NA, label.color = NA
  ) +
  annotate(
    "richtext", x = lim[2] * 0.85, y = lim[1] * 1.15, hjust = 1,
    label = sprintf("<b style='color:%s'>Brazil buys more<br>than it sells</b>", col_deficit),
    family = font_text, size = 3, fill = NA, label.color = NA
  ) +
  geom_point(aes(size = total, color = region), alpha = 0.8) +
  geom_text_repel(
    aes(label = country, fontface = face),
    family = font_text,
    size = 2.7,
    color = "gray20",
    seed = 14,
    max.overlaps = Inf,
    min.segment.length = 0,
    segment.color = "gray70",
    box.padding = 0.4
  ) +
  scale_x_log10(
    limits = lim,
    labels = label_dollar(suffix = "B", accuracy = 1)
  ) +
  scale_y_log10(
    limits = lim,
    labels = label_dollar(suffix = "B", accuracy = 1)
  ) +
  scale_size_area(max_size = 16, guide = "none") +
  scale_color_manual(values = region_pal) +
  coord_fixed() +
  labs(
    title = "Who Brazil trades with — and who it sells more to than it buys",
    subtitle = str_glue(
      "Brazil's top 28 trading partners in {ref_year}, by exports to vs. imports from each (US$ billion, FOB, log scale). ",
      "Bubble size is total bilateral trade. Points above the line are <b style='color:{col_surplus}'>surpluses</b>; ",
      "below, <b style='color:{col_deficit}'>deficits</b>. <b>China</b> alone buys far more from Brazil than it sells back."
    ),
    caption = str_glue("Source: Comex Stat / MDIC ({ref_year}) • @viniciusoike"),
    x = "Imports from partner",
    y = "Exports to partner",
    color = NULL
  ) +
  theme_plot +
  guides(color = guide_legend(override.aes = list(size = 4)))

ggsave(
  here("2026/plots/14_trade.png"),
  bubbles,
  width = 8,
  height = 8,
  dpi = 400
)
