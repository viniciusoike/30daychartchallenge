library(dplyr)
library(stringr)

import::from(here, here)
import::from(readr, read_rds)

# Data ----

# Census 2022 mobility (SIDRA table 10332, variable 13377), cached at the
# municipality level by 07_multiscale.R. Columns: code_muni, mode, race, educ,
# value. No SIDRA download needed.
tab_modals <- read_rds(here("2026", "data", "census_modes", "modes_muni.rds"))

code_sp <- 3550308

# Mode mapping ----

# Re-group the 14 raw census modes into the 5 categories of interest. Unlike
# 05_experimental.R (which merges walking/cycling into "Active" and drops an
# "Other" bucket), here a pé and bicicleta are split and every remaining mode is
# folded into one of the five, so shares sum to 100%.
dim_mode <- tibble(
  mode = c(
    "A pé",
    "Automóvel",
    "BRT ou ônibus de trânsito rápido",
    "Bicicleta",
    "Caminhonete ou caminhão adaptado (pau de arara)",
    "Embarcação de médio e grande porte (acima de 20 pessoas)",
    "Embarcação de pequeno porte (até 20 pessoas)",
    "Motocicleta",
    "Mototáxi",
    "Outros",
    "Trem ou metrô",
    "Táxi ou assemelhados",
    "Van, perua ou assemelhados",
    "Ônibus"
  ),
  group = case_when(
    mode == "A pé" ~ "a pé",
    mode == "Bicicleta" ~ "bicicleta",
    str_detect(mode, "Embarcação") ~ "transporte público",
    mode %in%
      c(
        "BRT ou ônibus de trânsito rápido",
        "Ônibus",
        "Trem ou metrô",
        "Van, perua ou assemelhados",
        "Outros"
      ) ~ "transporte público",
    mode %in%
      c(
        "Automóvel",
        "Táxi ou assemelhados",
        "Caminhonete ou caminhão adaptado (pau de arara)"
      ) ~ "carro",
    mode %in% c("Motocicleta", "Mototáxi") ~ "moto"
  )
)

# Modal share ----

lvls_modes <- c("a pé", "bicicleta", "transporte público", "carro", "moto")

share_sp <- tab_modals |>
  filter(
    race == "Total",
    educ == "Total",
    mode != "Total",
    code_muni == code_sp
  ) |>
  left_join(dim_mode, by = "mode") |>
  summarise(trips = sum(value, na.rm = TRUE), .by = "group") |>
  mutate(
    share = trips / sum(trips) * 100,
    group = factor(group, levels = lvls_modes)
  ) |>
  arrange(group)

print(share_sp)
