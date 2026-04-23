library(dplyr)
library(ggplot2)
library(sidrar)
import::from(stringr, str_glue)

api_query <- "/t/2612/n2/all/v/allxp/p/all/c235/5325,5326,5327,5328,5329,5330,5331,5332,5333,5334,5335,5336/c2/0/c237/0/c238/0/c240/0"

dat <- get_sidra(api = api_query)

dat <- dat |>
  as_tibble() |>
  janitor::clean_names() |>
  select(
    code_region = grande_regiao_codigo,
    year = ano,
    month = mes_do_nascimento,
    births = valor
  )

tab_births <- dat |>
  mutate(
    date = readr::parse_date(
      paste(year, month, "01", sep = "-"),
      format = "%Y-%B-%d",
      locale = readr::locale("pt")
    ),
    month_num = lubridate::month(date),
    year = as.numeric(year),
    code_region = as.numeric(code_region),
    name_region = case_when(
      code_region == 1 ~ "North",
      code_region == 2 ~ "Northeast",
      code_region == 3 ~ "Southeast",
      code_region == 4 ~ "South",
      code_region == 5 ~ "Mid-West"
    ),
    name_region = factor(name_region),
    name_region = forcats::fct_reorder(name_region, code_region)
  ) |>
  arrange(code_region, date)

tab_births <- tab_births |>
  mutate(
    share = births / sum(births) * 100,
    .by = c("code_region", "year"),
    month_label = lubridate::month(date, label = TRUE),
    month_label = forcats::fct_rev(month_label)
  ) |>
  select(code_region, name_region, date, year, month_label, births, share)

tab_births_brazil <- tab_births |>
  summarise(total = sum(births), .by = c("year", "month_label")) |>
  mutate(share = total / sum(total) * 100, .by = "year") |>
  mutate(
    name_region = "Brazil",
    code_region = 0
  )

offwhite <- "#f8fbf8"

plot_heatmap <- function(code_region) {
  if (code_region == 0) {
    dat <- tab_births_brazil
    name_region <- "Brazil"
  } else {
    dat <- dplyr::filter(tab_births, code_region == !!code_region)
    name_region <- unique(dat$name_region)
  }

  ggplot(
    dat,
    aes(x = year, y = month_label, fill = share)
  ) +
    geom_tile(color = "gray10", lwd = 0.1) +
    scale_fill_viridis_c(
      option = "viridis",
      breaks = seq(6.5, 9.5, 0.5),
      limits = c(6, 10)
    ) +
    scale_x_continuous(
      breaks = 2003:2024,
      position = "top",
      expand = expansion(0)
    ) +
    labs(
      title = stringr::str_glue("Monthly births: {name_region}"),
      subtitle = NULL,
      x = NULL,
      y = NULL,
      fill = "Share of annual births (%)"
    ) +
    theme_minimal(base_family = "Roboto Slab") +
    theme_sub_panel(
      grid.minor = element_blank(),
      grid.major = element_blank(),
      background = element_rect(fill = offwhite, color = offwhite)
    ) +
    theme_sub_plot(
      title = element_text(size = 12, family = "Georgia"),
      subtitle = element_text(size = 10, color = "gray40"),
      caption = element_text(size = 8, color = "gray60"),
      margin = margin(10, 10, 10, 10),
      background = element_rect(fill = offwhite, color = offwhite)
    ) +
    theme_sub_legend(
      background = element_rect(fill = offwhite, color = offwhite),
      title = element_text(size = 10, color = "gray40"),
      title.position = "top",
      justification = "left",
      position = "top",
      key.width = unit(2, "cm")
    ) +
    theme_sub_axis(
      text = element_text(size = 9, family = "Fira Code")
    )
}

# xreg <- tab_births |>
#   mutate(
#     days = lubridate::days_in_month(date),
#     births_adjusted = births * (365 / 12) / days,
#     y = log(births_adjusted)
#   ) |>
#   trendseries::augment_trends(
#     value_col = "y",
#     group_cols = "code_region",
#     methods = "stl"
#   ) |>
#   mutate(detrend = y - trend_stl, month_num = lubridate::month(date))

# summary(lm(detrend ~ factor(month_num), data = subset(xreg, code_region == 5)))

plot_final <- ggplot(
  tab_births_brazil,
  aes(x = year, y = month_label, fill = share)
) +
  geom_tile(color = "gray90", lwd = 0.1) +
  scale_fill_viridis_c(
    option = "mako",
    breaks = seq(6.5, 9.5, 0.5)
  ) +
  scale_x_continuous(
    breaks = 2003:2024,
    position = "top",
    expand = expansion(0)
  ) +
  labs(
    title = "Brazil: A country of Arians and Pisceans",
    subtitle = NULL,
    x = NULL,
    y = NULL,
    caption = "Source: IBGE (Civil Record Statistics 2024)",
    fill = "Share of annual births (%)"
  ) +
  theme_minimal(base_family = "Roboto Slab") +
  theme_sub_plot(
    title = element_text(size = 16, family = "Georgia"),
    subtitle = element_text(size = 10, color = "gray40"),
    caption = element_text(size = 8, color = "gray60"),
    margin = margin(15, 15, 15, 15),
    background = element_rect(fill = offwhite, color = offwhite)
  ) +
  theme_sub_panel(
    grid.minor = element_blank(),
    grid.major = element_blank(),
    background = element_rect(fill = offwhite, color = offwhite)
  ) +
  theme_sub_legend(
    background = element_rect(fill = offwhite, color = offwhite),
    title = element_text(size = 10, color = "gray40"),
    title.position = "top",
    justification = "left",
    position = "top",
    key.width = unit(2, "cm"),
    margin = margin(5, 0, 10, 0)
  ) +
  theme_sub_axis(
    text = element_text(size = 7, family = "Fira Code")
  )

ggsave(
  here::here("2026", "plots", "23_seasons.png"),
  plot_final,
  width = 8,
  height = 5,
  dpi = 400
)
