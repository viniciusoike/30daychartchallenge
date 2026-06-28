deaths <- sidrar::get_sidra(
  2654,
  variable = 343,
  period = "2022",
  classific = "c244",
  category = list(c(0))
)

deaths <- readxl::read_excel(
  here("data/day_10/obitos.xlsx"),
  skip = 6,
  .name_repair = janitor::make_clean_names,
  na = "-")

x1 = seq(5, 95, 5)
x2 = seq(9, 99, 5)

age_groups <- c("1 a 4 anos", str_c(x1, " a ", x2, " anos"), "100 anos ou mais")

dat <- deaths |>
  select(x_3:last_col(), -x_4) |>
  rename(age = x_3) |>
  pivot_longer(-age, values_to = "deaths") |>
  mutate(
    type = case_when(
      str_detect(name, "_2") ~ "natural",
      str_detect(name, "_3") ~ "non_natural",
      TRUE ~ "total"
    ),
    sex = case_when(
      str_detect(name, "homens") ~ "male",
      str_detect(name, "mulheres") ~ "female",
      TRUE ~ "total"
    )
  ) |>
  select(age, sex, type, deaths)

dat <- dat |>
  filter(age %in% age_groups) |>
  mutate(
    age_min = as.numeric(str_extract(age, "^[0-9]{1,3}")),
    age = factor(age),
    age = fct_reorder(age, age_min)
  )

ggplot(filter(dat, type != "total", sex != "total"), aes(age, deaths)) +
  geom_col()
