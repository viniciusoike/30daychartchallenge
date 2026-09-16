# Day 15 — Prompt: Correlation ---------------------------------------------
# Do richer neighbourhoods cost more? Scatter of residential sale price per m²
# against average household-head income, one point per São Paulo district.
# Income comes from the IBGE Censo 2022 head-of-household income (V06004), the
# same source as Day 08 (2026/R/08_circular.R), aggregated from census tracts up
# to the district. Price/m² is read from a local file (gitignored).

library(tidyverse)
library(ggtext)
import::from(here, here)

# Config ------------------------------------------------------------------
# City fixed to São Paulo; the district code (IBGE geocódigo, 9 digits =
# UF[2] + município[5] + distrito[2]) is the join key between price and income.
code_muni_sp <- 3550308

# Data contract for the price file. Provide 2026/data/sp_prices/district_prices.csv
# with at least:
#   code_district : 9-digit IBGE district geocode (character), OR
#   district      : district name (used only if code_district is absent), and
#   price_m2      : average residential sale price in R$/m².
# Adjust the path / column names here if your file differs.
price_path <- here("2026", "data", "sp_prices", "district_prices.csv")

# Income (IBGE Censo 2022) ------------------------------------------------
# Reuse the download from Day 08 if present; otherwise fetch the same zip.
data_dir <- here("2026", "data", "censo_renda")
zip_path <- file.path(data_dir, "renda_setores.zip")
url <- paste0(
  "https://ftp.ibge.gov.br/Censos/Censo_Demografico_2022/",
  "Agregados_por_Setores_Censitarios_Rendimento_do_Responsavel/",
  "Agregados_por_setores_renda_responsavel_BR_20260508_csv.zip"
)

if (!dir.exists(data_dir)) {
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
}
csv_path <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)
if (length(csv_path) == 0) {
  options(timeout = 600)
  download.file(url, zip_path, mode = "wb", quiet = TRUE)
  unzip(zip_path, exdir = data_dir)
  csv_path <- list.files(data_dir, pattern = "\\.csv$", full.names = TRUE)
}
csv_path <- csv_path[[1]]

# V06004 = average monthly nominal income of the head of household; V06001 =
# number of responsible persons (used to weight the tract means up to district).
# ';'-delimited, dot decimals, "X" = suppressed -> NA; read codes as character
# so the 15-digit CD_SETOR is not coerced to scientific notation.
income_raw <- read_delim(
  csv_path,
  delim = ";",
  na = "X",
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)

income_district <- income_raw |>
  transmute(
    code_muni = as.numeric(str_sub(CD_SETOR, 1, 7)),
    code_district = str_sub(CD_SETOR, 1, 9),
    income = as.numeric(V06004),
    weight = as.numeric(V06001)
  ) |>
  filter(code_muni == code_muni_sp, !is.na(income)) |>
  group_by(code_district) |>
  # Population-weighted district mean (falls back to a simple mean if the weight
  # column is missing/zero for every tract in the district).
  summarise(
    income = if (sum(weight, na.rm = TRUE) > 0) {
      weighted.mean(income, weight, na.rm = TRUE)
    } else {
      mean(income, na.rm = TRUE)
    },
    .groups = "drop"
  )

# Price -------------------------------------------------------------------
if (!file.exists(price_path)) {
  stop(
    "Price file not found: ", price_path,
    "\nDrop a CSV there with columns code_district (or district) and price_m2.",
    call. = FALSE
  )
}

price_raw <- read_csv(price_path, show_col_types = FALSE)

# Prefer the geocode join; fall back to a name join if code_district is absent.
join_by_code <- "code_district" %in% names(price_raw)
price <- price_raw |>
  mutate(
    price_m2 = as.numeric(price_m2),
    code_district = if (join_by_code) as.character(code_district) else NA_character_
  )

dat <- if (join_by_code) {
  inner_join(price, income_district, by = "code_district")
} else {
  # Name join: requires an IBGE district-name lookup keyed by code_district.
  stop(
    "price file has no code_district column; add one (9-digit IBGE geocode) ",
    "or supply a name->code lookup before joining.",
    call. = FALSE
  )
} |>
  filter(is.finite(price_m2), is.finite(income), price_m2 > 0, income > 0)

# Correlation stats (log-log: prices and incomes are right-skewed) --------
fit <- lm(log(price_m2) ~ log(income), data = dat)
r_pearson <- cor(log(dat$price_m2), log(dat$income))
r2 <- summary(fit)$r.squared

stat_label <- sprintf(
  "Pearson r = %.2f &middot; R<sup>2</sup> = %.2f",
  r_pearson, r2
)

# Districts to label: the four highest and four lowest by price/m².
to_label <- dat |>
  arrange(desc(price_m2)) |>
  slice(c(1:4, (n() - 3):n()))

# Theme -------------------------------------------------------------------
offwhite <- "#f8fbf8"
ink <- "#22303a"
point_col <- "#2a6f8e"

theme_plot <- theme_minimal(base_family = "Lato", base_size = 12) +
  theme_sub_plot(
    title = element_markdown(family = "Lora", size = 18, color = ink),
    subtitle = element_markdown(family = "Lato", size = 10.5, color = "gray35"),
    caption = element_text(family = "Lato", size = 8, color = "gray50"),
    background = element_rect(fill = offwhite, color = offwhite)
  ) +
  theme_sub_panel(grid.minor = element_blank()) +
  theme(
    plot.title.position = "plot",
    plot.margin = margin(14, 18, 10, 14),
    panel.grid.major = element_line(color = "gray88", linewidth = 0.3)
  )

# Plot --------------------------------------------------------------------
p <- ggplot(dat, aes(income, price_m2)) +
  geom_smooth(
    method = "lm",
    se = TRUE,
    color = ink,
    fill = "gray80",
    linewidth = 0.6,
    alpha = 0.4
  ) +
  geom_point(color = point_col, alpha = 0.7, size = 2.4) +
  ggrepel::geom_text_repel(
    data = to_label,
    aes(label = if (join_by_code) code_district else district),
    family = "Lato",
    size = 3,
    color = ink,
    min.segment.length = 0,
    seg.color = "gray60",
    max.overlaps = Inf
  ) +
  annotate(
    "richtext",
    x = min(dat$income),
    y = max(dat$price_m2),
    label = stat_label,
    hjust = 0,
    vjust = 1,
    family = "Lato",
    size = 3.4,
    label.color = NA,
    fill = NA,
    color = ink
  ) +
  scale_x_log10(labels = scales::label_number(prefix = "R$ ", big.mark = ".")) +
  scale_y_log10(labels = scales::label_number(prefix = "R$ ", big.mark = ".")) +
  labs(
    title = "Wealthier districts, pricier square metres",
    subtitle = paste0(
      "Average residential sale price per m² vs. average head-of-household ",
      "income, by São Paulo district (log–log scales)."
    ),
    x = "Average head-of-household income (monthly)",
    y = "Sale price per m²",
    caption = "Sources: IBGE — Censo 2022 (income); local listings (price) • @viniciusoike"
  ) +
  theme_plot

ggsave(
  here("2026", "plots", "15_correlation.png"),
  p,
  width = 8,
  height = 6.5,
  dpi = 300
)
