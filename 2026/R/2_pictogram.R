library(sidrar)
library(dplyr)
import::from(stringr, str_detect)

dat <- get_sidra(
  api = "/t/9933/n1/all/n3/all/v/allxp/p/all/c1975/73086/c63/allxt/c125/allxt"
)

dat <- dat |>
  as_tibble() |>
  janitor::clean_names()

clean_dat <- dat |>
  mutate(
    grouped_property = case_when(
      str_detect(condicao_de_ocupacao_do_domicilio, "Próprio") ~ "Owned",
      condicao_de_ocupacao_do_domicilio == "Alugado" ~ "Rented",
      TRUE ~ "Other"
    ),
    grouped_type = case_when(
      str_detect(tipo_de_domicilio, "Casa") ~ "House",
      tipo_de_domicilio == "Apartamento" ~ "Apartment",
      TRUE ~ "Other"
    )
  ) |>
  select(
    code_state = brasil_e_unidade_da_federacao_codigo,
    property = condicao_de_ocupacao_do_domicilio,
    grouped_property,
    type = tipo_de_domicilio,
    grouped_type,
    value = valor
  )

clean_dat <- clean_dat |>
  mutate(code_state = as.numeric(code_state))

clean_dat <- clean_dat |>
  filter_out(
    property == "Próprio de algum morador" |
      property == "Cedido ou emprestado"
  ) |>
  summarise(
    total = sum(value, na.rm = TRUE),
    .by = c("code_state", "grouped_property", "grouped_type")
  ) |>
  mutate(share = total / sum(total) * 100, .by = c("code_state"))


clean_dat |>
  filter(code_state == 1) |>
  mutate(prop = round(share)) |>
  arrange(desc(prop))
