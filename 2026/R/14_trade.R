library(dplyr)
library(ggplot2)
library(ragg)
library(ggtext)
library(scales)
library(stringr)
library(ggflags)

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
if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE)
}

comex_base <- "https://balanca.economia.gov.br/balanca/bd"
files <- c(
  exports = sprintf("%s/comexstat-bd/ncm/EXP_%d.csv", comex_base, ref_year),
  imports = sprintf("%s/comexstat-bd/ncm/IMP_%d.csv", comex_base, ref_year),
  pais = sprintf("%s/tabelas/PAIS.csv", comex_base)
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

country <- fread(
  local_path(files[["pais"]]),
  sep = ";",
  select = c("CO_PAIS", "CO_PAIS_ISOA3", "NO_PAIS_ING"),
  colClasses = c(CO_PAIS = "integer"),
  encoding = "Latin-1"
)

country <- country |>
  as_tibble() |>
  rename(iso3 = CO_PAIS_ISOA3, country = NO_PAIS_ING)

# Reshape -----------------------------------------------------------------

trade <- full_join(exports, imports, by = "CO_PAIS")
trade <- left_join(trade, country, by = "CO_PAIS")

trade <- trade |>
  mutate(across(c(exports, imports), \(x) coalesce(x, 0))) |>
  filter_out(is.na(iso3)) |>
  filter_out(iso3 == "ZZZ")

trade <- trade |>
  mutate(continent = countrycode(iso3, "iso3c", "continent")) |>
  filter_out(is.na(continent)) |>
  mutate(
    region = case_when(
      continent != "Americas" ~ continent,
      iso3 %in% c("USA", "CAN") ~ "North America",
      TRUE ~ "Latin America"
    ),
    total = exports + imports,
    balance = exports - imports
  ) |>
  mutate(across(c(exports, imports, total), log))


# Keep the partners that actually drive Brazil's trade.
plot_data <- trade |>
  slice_max(total, n = 30) |>
  mutate(
    is_key = iso3 %in% c("CHN", "USA"),
    face = if_else(is_key, "bold", "plain")
  )

#  [1] "China"                "United States"
#  [3] "Argentina"            "Germany"
#  [5] "Netherlands"          "Spain"
#  [7] "Mexico"               "Russia"
#  [9] "India"                "Chile"
# [11] "Japan"                "Italy"
# [13] "South Korea"          "France"
# [15] "Canada"               "Singapore"
# [17] "Vietnam"              "Paraguay"
# [19] "Indonesia"            "United Kingdom"
# [21] "Saudi Arabia"         "Thailand"
# [23] "Malaysia"             "Belgium"
# [25] "United Arab Emirates" "Colombia"
# [27] "Turkey"               "Uruguay"
# [29] "Egypt"                "Portugal"

code <- c(
  "China" = "cn",
  "United States" = "us",
  "Brazil" = "br",
  "Japan" = "jp",
  "Mexico" = "mx",
  "Germany" = "de",
  "Netherlands" = "nl",
  "Spain" = "es",
  "Russia" = "ru",
  "India" = "in",
  "Chile" = "cl",
  "Argentina" = "ar",
  "France" = "fr",
  "Canada" = "ca",
  "Singapore" = "sg",
  "Vietnam" = "vn",
  "Paraguay" = "py",
  "Indonesia" = "id",
  "United Kingdom" = "gb",
  "Saudi Arabia" = "sa",
  "Thailand" = "th",
  "Malaysia" = "my",
  "Belgium" = "be",
  "United Arab Emirates" = "ae",
  "Colombia" = "co",
  "Turkey" = "tr",
  "Uruguay" = "uy",
  "Egypt" = "eg",
  "Portugal" = "pt"
)

# Plot --------------------------------------------------------------------

offwhite <- "#fefefe"
col_surplus <- "#2C7A7B" # Brazil sells more than it buys
col_deficit <- "#C53030" # Brazil buys more than it sells

font_text <- "Roboto Slab"

region_pal <- c(
  "Asia" = "#D69E2E",
  "Latin America" = "#2C7A7B",
  "North America" = "#C53030",
  "Europe" = "#1E3A5F",
  "Africa" = "#805AD5",
  "Oceania" = "#475569"
)

# Shared limits so the 45-degree balanced-trade line reads diagonally.
lim <- range(c(plot_data$exports, plot_data$imports))
# lim <- c(lim[1] * 0.6, lim[2] * 1.4)

theme_plot <- theme_minimal(base_family = font_text) +
  theme_sub_plot(
    title = element_text(size = 16, family = "Georgia"),
    subtitle = element_textbox_simple(
      size = 10,
      color = "gray40",
      margin = margin(b = 12)
    ),
    caption = element_text(size = 8, color = "gray60"),
    margin = margin(15, 10, 15, 10),
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

ggplot(plot_data, aes(imports, exports)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray55") +
  ggrepel::geom_label_repel(
    aes(label = iso3, fontface = face),
    family = font_text,
    size = 2,
    max.overlaps = Inf,
    segment.color = "gray20",
    box.padding = 0.4
  ) +
  geom_point(aes(size = total, fill = region), shape = 21, color = "black") +
  scale_x_continuous(
    expand = expansion(mult = c(0.1, 0.1)),
    limits = lim,
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0.1, 0.1)),
    limits = lim
  ) +
  scale_size(range = c(2, 10), guide = "none") +
  scale_fill_manual(values = region_pal) +
  theme_plot +
  guides(color = guide_legend(override.aes = list(size = 4)))


# annotate(
#   "richtext", x = lim[1] * 1.15, y = lim[2] * 0.85, hjust = 0,
#   label = sprintf("<b style='color:%s'>Brazil sells more<br>than it buys</b>", col_surplus),
#   family = font_text, size = 3, fill = NA, label.color = NA
# ) +
# annotate(
#   "richtext", x = lim[2] * 0.85, y = lim[1] * 1.15, hjust = 1,
#   label = sprintf("<b style='color:%s'>Brazil buys more<br>than it sells</b>", col_deficit),
#   family = font_text, size = 3, fill = NA, label.color = NA
# ) +

# labs(
#   title = "Who Brazil trades with — and who it sells more to than it buys",
#   subtitle = str_glue(
#     "Brazil's top 28 trading partners in {ref_year}, by exports to vs. imports from each (US$ billion, FOB, log scale). ",
#     "Bubble size is total bilateral trade. Points above the line are <b style='color:{col_surplus}'>surpluses</b>; ",
#     "below, <b style='color:{col_deficit}'>deficits</b>. <b>China</b> alone buys far more from Brazil than it sells back."
#   ),
#   caption = str_glue(
#     "Source: Comex Stat / MDIC ({ref_year}) • @viniciusoike"
#   ),
#   x = "Imports from partner",
#   y = "Exports to partner",
#   color = NULL
# )

ggsave(
  here("2026/plots/14_trade.png"),
  bubbles,
  width = 8,
  height = 8,
  dpi = 400
)
