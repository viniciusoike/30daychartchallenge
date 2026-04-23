library(ragg)
library(dplyr)
library(tidyr)
library(ggplot2)
library(sf)
import::from(here, here)
import::from(readxl, read_excel)

dat <- readxl::read_excel(
  here("data/day_20/household_composition_br_uf.xlsx"),
  skip = 3)

clean_dat <- dat |>
  rename(code_state = 1, name_state = 2, type_hh = 3) |>
  fill(code_state, name_state) |>
  filter(!is.na(code_state)) |>
  pivot_longer(starts_with("2"), names_to = "year", values_to = "count") |>
  mutate(across(c(code_state, year, count), as.numeric))

brazil_alone <- clean_dat |>
  filter(code_state == 1, type_hh != "Total") |>
  mutate(share = count / sum(count) * 100, .by = c("year")) |>
  filter(type_hh == "Unipessoal")

ufs_alone <- clean_dat |>
  filter(code_state != 1, type_hh != "Total") |>
  mutate(share = count / sum(count) * 100, .by = c("year", "code_state"))

ufs_alone_23 <- filter(ufs_alone, year == 2023)

codes <- c(23, 15, 31, 33)

subdat <- ufs_alone |>
  filter(type_hh == "Unipessoal", code_state %in% codes)

dat <- bind_rows(subdat, brazil_alone)

dat <- dat |>
  mutate(
    is_brazil = factor(if_else(code_state == 1, 1L, 0L)),
    name_abbrev = case_when(
      code_state == 23 ~ "CE",
      code_state == 31 ~ "MG",
      code_state == 15 ~ "PA",
      code_state == 33 ~ "RJ",
      code_state == 1 ~ "BR (avg.)"
    ),
    name_abbrev = factor(name_abbrev,
                         levels = c("CE", "PA", "BR (avg.)", "MG", "RJ"))
  )

cores <- c("#114787", "#0B6C5D", "gray20", "#fb8500", "#C29A00")
offwhite <- "#fefefe"
font <- "Helvetica Neue"

xlab <- "<span style='font-size: 8pt'> </span>"

p <- ggplot(dat, aes(year, share)) +
  geom_line(
    aes(color = name_abbrev)
  ) +
  geom_point(
    aes(fill = name_abbrev),
    shape = 21,
    size = 3,
    stroke = 1.5,
    color = offwhite
  ) +
  geom_label(
    data = filter(dat, year == max(year)),
    aes(label = paste0(round(share, 1), "%")),
    nudge_x = 0.5,
    nudge_y = c(0, 0, 0.5, 0, 0),
    family = font
  ) +
  geom_text(
    data = data.frame(x = 2011.65, y = seq(8, 20, 2)),
    aes(x = x, y = y + 0.35, label = paste0(y, "%")),
    family = font,
    size = 4,
    color = "#6B6865"
  ) +
  scale_x_continuous(
    breaks = 2012:2023, labels = c(2012, 13:19, 2020, 21:23),
    expand = expansion(mult = c(0.04, 0.04))) +
  scale_y_continuous(breaks = seq(8, 20, 2)) +
  scale_fill_manual(name = "", values = cores) +
  scale_color_manual(name = "", values = cores) +
  labs(
    title = "The Rise of Solo-living in Brazil",
    subtitle = "Share of one-person households in Brazil, highlighting selected states. Since the Covid Pandemic, the number of people living alone\nincreased by 3 million, reaching 18 million in 2023.",
    caption = "Source: IBGE (PNADC/A)",
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_family = font) +
  theme(
    legend.position = "bottom",
    panel.background = element_rect(color = offwhite, fill = offwhite),
    plot.background = element_rect(color = offwhite, fill = offwhite),
    plot.title = element_text(size = 22),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_line(linetype = 1),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 10, color = "#6B6865"),
    #> Muda a cor do "tiquezinho" no eixo-x e deixa ele mais comprido
    axis.ticks.x = element_line(color = "#EADFD8"),
    axis.ticks.length = unit(7, "pt"),
    axis.text.y = element_blank()
  )

ggsave(here("plots/20_urbanization.png"), p, width = 9.6, height = 6)

# ufs_alone <- ufs_alone |>
#   filter(year == min(year) | year == max(year), type_hh == "Unipessoal") |>
#   mutate(
#     name_state = factor(name_state),
#     code_region = factor(substr(code_state, 1, 1))
#   )
#
# ufs_alone_wide <- ufs_alone |>
#   pivot_wider(
#     id_cols = c("code_region", "code_state", "name_state"),
#     names_from = "year",
#     names_prefix = "share_",
#     values_from = "share"
#     ) |>
#   mutate(name_state = forcats::fct_reorder(name_state, share_2012))


