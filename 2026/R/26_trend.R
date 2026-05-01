library(readxl)
library(dplyr)
library(priceR)
library(RcppRoll)
import::from(here, here)

# urls <- c(
#   url_arbica = "https://www.cepea.org.br/br/indicador/series/cafe.aspx?id=23",
#   url_robusta = "https://www.cepea.org.br/br/indicador/series/cafe.aspx?id=24"
# )

## Import sheets -----------------------------------------------------------

arabica <- read_excel(
  here("2026/data/agriculture/CEPEA_cafe_arabica.xlsx"),
  skip = 3
)

robusta <- read_excel(
  here("2026/data/agriculture/CEPEA_cafe_robusta.xlsx"),
  skip = 3
)

## Clean -------------------------------------------------------------------

# Format dates and adjust dollar values for inflation

arabica <- arabica |>
  rename(date = 1, spot_rs = 2, spot_us = 3) |>
  mutate(date = as.Date(date, format = "%d/%m/%Y")) |>
  arrange(date)

robusta <- robusta |>
  rename(date = 1, spot_rs = 2, spot_us = 3) |>
  mutate(date = as.Date(date, format = "%d/%m/%Y")) |>
  arrange(date)

series <- bind_rows(
  list("arabica" = arabica, "robusta" = robusta),
  .id = "crop"
)

series$usd_2022 <- adjust_for_inflation(
  series$spot_us,
  country = "US",
  from_date = series$date,
  to_date = 2022
)

# 22-day moving average
series <- series |>
  filter(usd_2022 > 0) |>
  mutate(
    trend = RcppRoll::roll_meanr(usd_2022, n = 22, fill = NA),
    .by = "crop"
  )

ggplot(series, aes(date, trend, color = crop)) +
  geom_line()
