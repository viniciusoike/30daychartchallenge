library(ggplot2)
library(dplyr)
library(trendseries)
library(rbcb)


# SCMP-inspired color palette
scmp_red <- "#D7282F"
scmp_blue <- "#1D6FA4"
scmp_dark <- "#1A1A1A"
scmp_gray <- "#9E9E9E"
scmp_bg <- "#F7F4EF" # warm off-white, common in SCMP web charts

base_size = 11

theme_scmp <- theme_minimal(base_size = base_size, base_family = "Roboto") +
  theme_sub_plot(
    title = element_text(
      family = "Merriweather",
      face = "bold",
      size = base_size * 1.6,
      color = scmp_dark,
      margin = margin(b = 4)
    ),
    subtitle = element_text(
      family = "Roboto",
      size = base_size * 1.0,
      color = "#444444",
      margin = margin(b = 12)
    ),
    caption = element_text(
      family = "Roboto",
      size = base_size * 0.75,
      color = scmp_gray,
      hjust = 0
    ),
    background = element_rect(fill = scmp_bg, color = NA),
    margin = margin(12, 10, 8, 10)
  ) +
  theme_sub_panel(
    grid.minor = element_blank(),
    grid.major.x = element_blank(),
    grid.major.y = element_line(color = "#DEDAD5", linewidth = 0.4),
    background = element_rect(fill = scmp_bg, color = NA)
  ) +
  theme_sub_axis(
    text = element_text(
      color = "#555555",
      size = base_size * 0.9
    ),
    title = element_blank(),
    ticks = element_blank(),
    line = element_blank()
  ) +
  theme_sub_legend(position = "none")


params <- tribble(
  ~code , ~name                    ,
   4189 , "selic"                  ,

  # ── Taxas de financiamento imobiliário ──────────────────────────────────────
  20772 , "taxa_fimob_pf_mercado"  ,
  20773 , "taxa_fimob_pf_regulado" ,
  20774 , "taxa_fimob_pf_total"    ,

  # ── Volume de crédito imobiliário — concessões PF (R$ mil) ──────────────────
  20704 , "fimob_pf_total"         ,

  # ── Comprometimento de renda (% renda mensal) ───────────────────────────────
  29033 , "comprometimento_juros"  ,
  29034 , "comprometimento_serv"   ,
  29035 , "comprometimento_exchab" ,
  29036 , "comprometimento_amort"  ,

  # ── Endividamento das famílias (% renda acumulada 12 meses) ─────────────────
  29037 , "end_total"              ,
  29038 , "end_exchab"
)

# series <- list()

# for (i in seq_len(nrow(params))) {
#   code <- params$code[i]
#   name <- params$name[i]

#   tryCatch(
#     expr = {
#       series[[name]] <- get_series(code)
#     },
#     error = function(e) {
#       message(
#         "  ✗ Failed to download series ",
#         code,
#         " (",
#         name,
#         "): ",
#         conditionMessage(e)
#       )
#       series[[name]] <<- tibble(date = as.Date(NA_character_))
#     }
#   )
# }

# readr::write_rds(
#   series,
#   here::here("2026", "data", "macroeconomics", "bcb_series.rds")
# )

series <- readr::read_rds(
  here::here("2026", "data", "macroeconomics", "bcb_series.rds")
)

# ── Pivot to long format and join metadata ────────────────────────────────────
# Each get_series() returns a tibble with columns: date, <code_as_string>.
# After full_join, the wide table has date + one column per BCB code.
# pivot_longer converts those numeric-named columns back to a tidy code | value
# structure, then left_join restores the human-readable name.

tbl_series <- series |>
  purrr::reduce(full_join, by = "date") |>
  tidyr::pivot_longer(
    cols = -date,
    names_to = "code",
    names_transform = list(code = as.integer),
    values_to = "value"
  ) |>
  left_join(params, by = "code") |>
  filter(!is.na(name)) |> # drop any orphan columns from failed downloads
  arrange(name, date)

# Plot data -------------------------------------------------------------------

library(patchwork)
library(scales)
library(ggtext)
import::from(stringr, str_glue)

col_interest <- scmp_red
col_principal <- "#9FB4C0"

x_breaks <- as.Date(paste0(seq(2005, 2025, 5), "-01-01"))

# Debt service split into interest (juros) and principal (amortizacao).
# By BCB construction, juros + amort == total service ('serv'). The series
# begin in 2005-03; values are % of average monthly household income.
burden_levels <- c("Principal", "Interest")

burden <- tbl_series |>
  filter(
    name %in% c("comprometimento_juros", "comprometimento_amort"),
    !is.na(value)
  ) |>
  transmute(
    date,
    component = factor(
      if_else(name == "comprometimento_juros", "Interest", "Principal"),
      levels = burden_levels
    ),
    value
  )

start_date <- min(burden$date)
end_date <- max(burden$date)

# Selic policy rate (% per year), aligned to the same window.
selic <- tbl_series |>
  filter(name == "selic", !is.na(value), date >= start_date, date <= end_date)

# Latest-value labels at the right edge, centred within each stacked band.
burden_end <- burden |>
  filter(date == end_date) |>
  arrange(desc(component)) |>
  mutate(
    ypos = cumsum(value) - value / 2,
    label = str_glue("<b>{component}</b><br>{number(value, accuracy = 0.1)}%")
  )

total_end <- burden |>
  filter(date == end_date) |>
  summarise(value = sum(value)) |>
  mutate(date = end_date)

selic_end <- selic |> filter(date == max(date))

debt_total <- tbl_series |>
  filter(name == "end_total", date >= start_date, date <= end_date)

debt_end <- debt_total |>
  filter(date == max(date))

# Top panel: debt-service burden ----------------------------------------------

p_top <- ggplot(burden, aes(date, value, fill = component)) +
  geom_area(color = scmp_bg, linewidth = 0.2) +
  geom_hline(yintercept = 0, color = "gray10", linewidth = 0.3) +
  geom_richtext(
    data = burden_end,
    aes(x = end_date, y = ypos, label = label, color = component),
    inherit.aes = FALSE,
    hjust = 0,
    nudge_x = 120,
    family = "Roboto",
    size = 3,
    label.color = NA,
    fill = NA
  ) +
  geom_text(
    data = total_end,
    aes(
      x = date,
      y = value,
      label = number(value, accuracy = 0.1, suffix = "%")
    ),
    inherit.aes = FALSE,
    family = "Merriweather",
    fontface = "bold",
    color = scmp_dark,
    size = 3.4,
    hjust = 0,
    nudge_x = 120,
    vjust = -0.4
  ) +
  scale_fill_manual(
    values = c(Interest = col_interest, Principal = col_principal)
  ) +
  scale_color_manual(
    values = c(Interest = col_interest, Principal = "#5C7080")
  ) +
  scale_x_date(
    breaks = x_breaks,
    date_labels = "%Y",
    expand = expansion(mult = c(0.025, 0.16))
  ) +
  scale_y_continuous(
    labels = label_number(suffix = "%"),
    breaks = seq(0, 30, 10),
    limits = c(0, 33),
    expand = expansion(mult = c(0, 0.02))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    title = "The Value of Tomorrow: buried in debt",
    subtitle = "Brazilian households now commit nearly 30% of their monthly income to debt service<br>with <b style='color:#D7282F'>interest</b> alone taking more than a third of it"
  ) +
  theme_scmp +
  theme(
    plot.subtitle = element_markdown(
      family = "Roboto",
      size = 10,
      color = "#444444",
      lineheight = 1.2,
      margin = margin(b = 12)
    )
  )

# Bottom panel: Selic policy rate ---------------------------------------------

p_selic <- ggplot(selic, aes(date, value)) +
  geom_area(fill = scmp_dark, alpha = 0.07) +
  geom_hline(yintercept = 0, color = "gray10", linewidth = 0.3) +
  geom_step(color = scmp_dark, linewidth = 0.5) +
  geom_richtext(
    data = selic_end,
    aes(
      x = date,
      y = value,
      label = str_glue("<b>Selic</b><br>{number(value, accuracy = 0.1)}%")
    ),
    inherit.aes = FALSE,
    hjust = 0,
    nudge_x = 120,
    family = "Roboto",
    size = 3,
    color = scmp_dark,
    label.color = NA,
    fill = NA
  ) +
  scale_x_date(
    breaks = x_breaks,
    date_labels = "%Y",
    expand = expansion(mult = c(0.025, 0.16))
  ) +
  scale_y_continuous(
    labels = label_number(suffix = "%"),
    breaks = c(0, 7, 14),
    expand = expansion(mult = c(0.04, 0.12))
  ) +
  coord_cartesian(clip = "off") +
  labs(
    subtitle = "Selic policy rate, % per year (annualized, 252 days)",
  ) +
  theme_scmp +
  theme(
    plot.subtitle = element_text(
      family = "Roboto",
      size = 9,
      color = "#666666",
      margin = margin(t = 4, b = 6)
    )
  )

p_debt <- ggplot(debt_total, aes(date, value)) +
  geom_area(fill = scmp_dark, alpha = 0.07) +
  geom_hline(yintercept = 0, color = "gray10", linewidth = 0.3) +
  geom_line(color = scmp_dark, linewidth = 0.5) +
  geom_richtext(
    data = debt_end,
    aes(
      x = date,
      y = value,
      label = str_glue(
        "<b>Debt</b><br>{percent(value, scale = 1, accuracy = 0.1)}"
      )
    ),
    hjust = 0,
    nudge_x = 120,
    family = "Roboto",
    size = 3,
    color = scmp_dark,
    label.color = NA,
    fill = NA
  ) +
  scale_x_date(
    breaks = x_breaks,
    date_labels = "%Y",
    expand = expansion(mult = c(0.025, 0.16))
  ) +
  scale_y_continuous(
    labels = label_number(suffix = "%"),
    breaks = seq(0, 50, 25),
    expand = expansion(mult = c(0, 0.12))
  ) +
  labs(
    subtitle = "Household debt to income, % Households gross disposable income",
    caption = "Source: Brazilian Central Bank (BCB) — household debt, household debt-service ratio and Selic rate. • @viniciusoike\nDebt service (seasonally adjusted) is the share of monthly household income absorbed by interest and principal."
  ) +
  theme_scmp +
  theme(
    plot.subtitle = element_text(
      family = "Roboto",
      size = 9,
      color = "#666666",
      margin = margin(t = 4, b = 6)
    )
  )


# Compose ---------------------------------------------------------------------

p_scmp <- p_top /
  p_selic +
  p_debt +
  plot_layout(heights = c(2.5, 1, 1)) &
  theme(plot.background = element_rect(fill = scmp_bg, color = NA))

ggsave(
  here::here("2026/plots/24_scmp.png"),
  plot = p_scmp,
  width = 8,
  height = 10,
  dpi = 400
)
