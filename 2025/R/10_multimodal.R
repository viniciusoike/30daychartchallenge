# car accidents
# age of death
library(tidyverse)
import::from(here, here)
library(ggplot2)
library(ragg)

dat <- tibble(
  x = lubridate::wday(2:6, label = TRUE),
  y = c(47.6, 3.5, 1.8, 5.2, 41.9)
)

ggplot(dat, aes(x, y)) +
  geom_col(fill = "#023047") +
  geom_hline(yintercept = 0) +
  scale_y_continuous(expand = expansion(add = c(0, 1))) +
  labs(
    title = "Best days to work from home",
    caption = "Source: MyFakeData.com"
  ) +
  theme_minimal(base_family = "Microsoft Sans Serif") +
  theme(
    axis.text.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_text(size = 12),
    axis.title.x = element_blank()
  )


# Data --------------------------------------------------------------------

dat = qs::qread("/Users/viniciusoike/Documents/GitHub/weekly_viz/data/car_accidents_non_fatal.qs")

dat = dat %>%
  as_tibble() %>%
  filter(
    !is.na(lat),
    !is.na(lng)
  ) %>%
  select(lat, lng, victims_total, ts_year, ts_day_shift, ts_date, ts_time_accident, ts_weekday)

dat = dat %>%
  mutate(
    ts_weekday = stringr::str_to_title(ts_weekday),
    ts_weekday = factor(
      ts_weekday,
      levels = c("Domingo", "Segunda", "Terça", "Quarta", "Quinta", "Sexta", "Sábado")
    ),
    ts_date_full = paste(ts_date, ts_time_accident),
    ts_date_ymdhm = ymd_hm(ts_date_full),
    ts_time_accident = lubridate::hm(ts_time_accident),
    ts_hour = lubridate::hour(ts_time_accident)
  )

dat <- dat |>
  mutate(
    ts_halfhour = round_date(ts_date_ymdhm, "30 mins"),
    ts_minute = minute(ts_halfhour),
    ts_period = paste(ts_hour, str_pad(ts_minute, width = 2, side = "left", pad = "0"), sep = ":"),
    ts_p = hm(ts_period),
    seconds = period_to_seconds(ts_p))

dat <- dat |>
  mutate(
    period = factor(ts_period),
    period = fct_reorder(period, seconds)
  )

accidents_halfhour <- dat |>
  summarise(total = sum(victims_total), .by = "period") |>
  arrange(period)

ggplot(accidents_halfhour, aes(period, total)) +
  geom_col()

accidents_hour = dat |>
  filter(ts_year >= 2022, ts_year <= 2023) |>
  summarise(total = sum(victims_total), .by = c("ts_hour")) |>
  arrange(ts_hour) |>
  mutate(ts_hour = factor(ts_hour))


# Plot elements -----------------------------------------------------------

cores = c(
  "#07204E", "#07214F", "#072351", "#082453", "#082655", "#092857",
  "#092959", "#0A2B5B", "#0A2D5C", "#0B2E5E", "#0B2F5F", "#0C3160",
  "#0C3261", "#0D3463", "#0D3563", "#0D3563", "#0D3664", "#0D3764",
  "#0E3865", "#0E3965", "#0E3966", "#0E3A66", "#0F3C68", "#11406B",
  "#12436F", "#134772", "#154A75", "#164E79", "#18527C", "#195580",
  "#1A5983", "#1D5E87", "#20638B", "#236990", "#256E94", "#2A7598",
  "#307E9D", "#3787A2", "#3D90A7", "#4399AC", "#49A1B1", "#50AAB6",
  "#56B3BB", "#5CBBC0", "#67BFBB", "#73C3B5", "#7EC7AF", "#8ACBA9",
  "#96CEA1", "#A3D098", "#B0D38F", "#BDD586", "#C9D77E", "#D3D877",
  "#DEDA70", "#E8DB68", "#F2DC61", "#F2DA61", "#F3D860", "#F4D65F",
  "#F4D45E", "#F5D25D", "#F6CF5C", "#F6CD5C", "#F7CB5B", "#F8C85A",
  "#F8C55A", "#F8C35A", "#F8C05A", "#F8BD5A", "#F8BA5A", "#F8B85A",
  "#F8B55A", "#F8B25A", "#F7AF5A", "#F7AC5A", "#F7A85A", "#F7A55A",
  "#F6A15C", "#F59C5F", "#F59862", "#F49364", "#F38E67", "#F38A6A",
  "#F2856C", "#F2816F", "#F27C71", "#EB7673", "#E47175", "#DD6B77",
  "#D56679", "#CB607A", "#BF5A7B", "#B2547D", "#A64F7E", "#9A497F",
  "#8E4380", "#823D82", "#763783", "#6A3285"
)

inds = c(
  1, 6, 12, 24, 30, 36,
  45, 50, 56, 65, 66, 67,
  67, 71, 73, 77, 85, 90,
  95, 30, 27, 20, 15, 10
)

offwhite <- "#f5f5f5"

df_text <- tibble(
  label_x = rev(0:23),
  x = 1:24,
  y = 500,
  color = factor(c(rep(0, 6), rep(1, 12), rep(0, 6)))
)

dat <- accidents_hour |>
  mutate(ts_hour = fct_rev(ts_hour))

dat <- dat |>
  mutate(
    label_num = format(round(total, -2), big.mark = "."),
    color = factor(c(rep(0, 6), rep(1, 12), rep(0, 6)))
  )


# Plot 1 ------------------------------------------------------------------

# Horizontal plot with labels
ggplot(dat, aes(ts_hour, total)) +
  geom_col(aes(fill = ts_hour)) +
  geom_text(
    data = df_text,
    aes(x = x, y = y, label = label_x, color = color),
    family = "Roboto Slab") +
  geom_text(
    aes(y = total - 1000, label = label_num, color = color),
    family = "Roboto Slab") +
  scale_color_manual(values = c("white", "black")) +
  coord_flip() +
  geom_hline(yintercept = 0) +
  scale_y_continuous(labels = scales::label_number(big.mark = "."), expand = expansion(add = c(0, NA))) +
  scale_fill_manual(values = cores[inds]) +
  guides(fill = "none") +
  labs(
    title = "Every hour is rush hour",
    subtitle = "Car accidents in São Paulo by hour of the day (2023/2024).\nThe colors roughly indicate daylight patterns throughout the year.",
    x = "Hour of the day",
    y = "Car accidents",
    caption = "Source: INFOSIGA SP") +
  theme_minimal(base_family = "Roboto Slab") +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = offwhite, color = offwhite),
    plot.background = element_rect(fill = offwhite, color = offwhite),
    panel.grid.minor = element_blank(),
    panel.grid.major = element_blank(),
    axis.text.y = element_blank(),
    axis.text.x = element_blank()
    #axis.text.x = element_text(size = 12, family = "Roboto Slab")
  )


# Plot 2 ------------------------------------------------------------------

ggplot(accidents_hour, aes(ts_hour, total)) +
  geom_col(aes(fill = ts_hour)) +
  geom_text(
    data = df_text,
    aes(x = x, y = y + 500, label = rev(label_x), color = color),
    family = "Roboto Slab") +
  scale_color_manual(values = c("white", "black")) +
  geom_hline(yintercept = 0) +
  scale_y_continuous(
    labels = scales::label_number(big.mark = "."),
    expand = expansion(add = c(0, NA))) +
  scale_fill_manual(values = cores[inds]) +
  guides(fill = "none") +
  labs(
    title = "Every hour is rush hour",
    subtitle = "Car accidents in São Paulo by hour of the day (2023/2024).\nThe colors roughly indicate daylight patterns throughout the year.",
    x = "Hour of the day",
    y = "Car accidents",
    caption = "Source: INFOSIGA SP") +
  theme_minimal(base_family = "Roboto Slab") +
  theme(
    legend.position = "none",
    panel.background = element_rect(fill = offwhite, color = offwhite),
    plot.background = element_rect(fill = offwhite, color = offwhite),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    axis.text.x = element_blank()
    #axis.text.x = element_text(size = 12, family = "Roboto Slab")
  )


# ggsave(here("plots/10_multimodal_1.png"), p1)
# ggsave(here("plots/10_multimodal_3.png"), p2)
# library(multimode)
#
# modetest(sample(dat$ts_hour, 5000),method = 'SI')
#
# multimode::modetest(accidents_hour$total)
