library(ggplot2)
library(dplyr)
library(showtext)
library(trendseries)

# Fonts matching SCMP's web style
font_add_google("Merriweather", "merriweather")
font_add_google("Roboto", "roboto")
showtext_auto()

# SCMP-inspired color palette
scmp_red <- "#D7282F"
scmp_blue <- "#1D6FA4"
scmp_dark <- "#1A1A1A"
scmp_gray <- "#9E9E9E"
scmp_bg <- "#F7F4EF" # warm off-white, common in SCMP web charts

theme_scmp <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      text = element_text(family = "roboto", color = scmp_dark),
      plot.title = element_text(
        family = "merriweather",
        face = "bold",
        size = base_size * 1.6,
        color = scmp_dark,
        margin = margin(b = 4)
      ),
      plot.subtitle = element_text(
        family = "roboto",
        size = base_size * 1.0,
        color = "#444444",
        margin = margin(b = 12)
      ),
      plot.caption = element_text(
        family = "roboto",
        size = base_size * 0.75,
        color = scmp_gray,
        hjust = 0
      ),
      plot.background = element_rect(fill = scmp_bg, color = NA),
      panel.background = element_rect(fill = scmp_bg, color = NA),
      panel.grid.major = element_line(color = "#DEDAD5", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      axis.text = element_text(
        family = "roboto",
        color = "#555555",
        size = base_size * 0.85
      ),
      axis.title = element_blank(), # SCMP rarely shows axis titles
      axis.ticks = element_blank(),
      legend.position = "none" # prefer direct labels
    )
}

library(rbcb)

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

series <- list()

for (i in seq_len(nrow(params))) {
  code <- params$code[i]
  name <- params$name[i]

  tryCatch(
    expr = {
      series[[name]] <- get_series(code)
    },
    error = function(e) {
      message(
        "  ✗ Failed to download series ",
        code,
        " (",
        name,
        "): ",
        conditionMessage(e)
      )
      series[[name]] <<- tibble(date = as.Date(NA_character_))
    }
  )
}

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

tbl_series
