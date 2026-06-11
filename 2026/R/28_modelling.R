# Day 28 - Modelling (Uncertainties): the Gompertz law of mortality ---------
# IBGE 2022 life table. The death hazard rises ~exponentially with age, so on
# a log scale mortality is nearly a straight line (Gompertz). We fit that line
# on the adult range and let the deviations tell the story: infant mortality,
# the young-male "accident hump", and old-age deceleration.

library(dplyr)
library(ggplot2)
library(ggtext)
import::from(readxl, read_excel)
import::from(here, here)
import::from(scales, label_log, label_number)

## Data ---------------------------------------------------------------------
# Column 1 is exact age X; column 2 is q(X) = probability of death within the
# year (per 1,000). The 90+ row is an open interval with q = 1,000 (certain
# death); drop it. Convert to the continuous hazard mu(X) = -log(1 - q).

read_lt <- function(file, sex) {
  raw <- read_excel(file, sheet = 1, col_names = FALSE, skip = 5)
  age_raw <- raw[[1]]
  age <- ifelse(grepl("ou mais", age_raw), 90L, suppressWarnings(as.integer(age_raw)))
  tibble(
    age = age,
    qx = suppressWarnings(as.numeric(raw[[2]])) / 1000
  ) |>
    filter(!is.na(age), !is.na(qx), qx < 1) |>
    mutate(mu = -log(1 - qx), sex = sex)
}

lt_dir <- here("2026", "data", "lifetable")

# Download the life tables from the IBGE FTP on first run (data/ is gitignored).
ftp <- "https://ftp.ibge.gov.br/Tabuas_Completas_de_Mortalidade/Tabuas_Completas_de_Mortalidade_2022/xlsx"
if (!dir.exists(lt_dir)) {
  dir.create(lt_dir, recursive = TRUE)
}
for (f in c("homens.xlsx", "mulheres.xlsx")) {
  dest <- file.path(lt_dir, f)
  if (!file.exists(dest)) {
    download.file(file.path(ftp, f), dest, mode = "wb", quiet = TRUE)
  }
}

lt <- bind_rows(
  read_lt(file.path(lt_dir, "homens.xlsx"), "Men"),
  read_lt(file.path(lt_dir, "mulheres.xlsx"), "Women")
) |>
  mutate(sex = factor(sex, levels = c("Men", "Women")))

## Model --------------------------------------------------------------------
# Gompertz: log mu(x) = log A + B x. Fit by OLS on the adult range where the
# law holds, then predict (with a 95% interval) across the full age axis so the
# extrapolation outside the window is visible. Doubling time = log(2) / B.

fit_lo <- 30
fit_hi <- 85

fit_gompertz <- function(d) {
  m <- lm(log(mu) ~ age, data = filter(d, age >= fit_lo, age <= fit_hi))
  grid <- tibble(age = seq(0, 90, by = 0.5))
  pr <- predict(m, newdata = grid, interval = "prediction", level = 0.95)
  grid |>
    mutate(
      fit = exp(pr[, "fit"]),
      lo = exp(pr[, "lwr"]),
      hi = exp(pr[, "upr"]),
      in_window = age >= fit_lo & age <= fit_hi
    )
}

preds <- lt |>
  group_by(sex) |>
  group_modify(~ fit_gompertz(.x)) |>
  ungroup()

doubling <- lt |>
  group_by(sex) |>
  summarise(
    B = coef(lm(log(mu) ~ age, data = pick(everything()) |> filter(age >= fit_lo, age <= fit_hi)))[["age"]],
    .groups = "drop"
  ) |>
  mutate(years = log(2) / B)

dt_men <- round(doubling$years[doubling$sex == "Men"], 1)

## Theme --------------------------------------------------------------------

base_text <- "Lato"
title_text <- "Lora"
offwhite <- "#f5f5dc"

colors_sex <- c("Men" = "#466C6F", "Women" = "#B35144")

theme_plot <- theme_minimal(base_family = base_text) +
  theme_sub_plot(
    background = element_rect(fill = offwhite, color = offwhite),
    margin = margin(14, 18, 8, 14),
    title = element_text(family = title_text, size = 18, hjust = 0),
    subtitle = element_textbox_simple(
      family = base_text,
      size = 9.5,
      color = "gray25",
      margin = margin(4, 0, 14, 0)
    ),
    caption = element_text(family = base_text, size = 6, color = "gray50", hjust = 0)
  ) +
  theme_sub_panel(
    grid.minor = element_blank(),
    grid.major = element_line(color = "gray80", linewidth = 0.3),
    background = element_rect(fill = offwhite, color = offwhite)
  ) +
  theme_sub_axis_x(
    line = element_line(color = "gray20", linewidth = 0.3),
    title = element_text(size = 9, color = "gray30")
  ) +
  theme(
    axis.text = element_text(size = 7.5, color = "gray30"),
    axis.title.y = element_text(size = 9, color = "gray30"),
    legend.position = "none"
  )

## Plot ---------------------------------------------------------------------

p <- ggplot() +
  # 95% prediction band of the fitted law
  geom_ribbon(
    data = preds,
    aes(age, ymin = lo, ymax = hi, fill = sex),
    alpha = 0.12
  ) +
  # fitted Gompertz line: solid inside the fit window, dashed where extrapolated
  geom_line(
    data = filter(preds, in_window),
    aes(age, fit, color = sex),
    linewidth = 0.7
  ) +
  geom_line(
    data = filter(preds, !in_window),
    aes(age, fit, color = sex, group = sex),
    linewidth = 0.5,
    linetype = "22"
  ) +
  # observed mortality
  geom_point(
    data = lt,
    aes(age, mu, color = sex),
    size = 1.1,
    alpha = 0.8
  ) +
  # annotations for the three places the straight line fails
  annotate(
    "richtext", x = 2, y = 0.02,
    label = "**Infant mortality**<br>far above the law",
    family = base_text, size = 2.5, color = "gray30",
    fill = NA, label.color = NA, hjust = 0
  ) +
  annotate(
    "richtext", x = 23, y = 0.0016,
    label = "**The accident hump** - young<br>men die well above what<br>the Gompertz law predicts",
    family = base_text, size = 2.5, color = colors_sex[["Men"]],
    fill = NA, label.color = NA, hjust = 0
  ) +
  annotate(
    "richtext", x = 47, y = 2.4e-4,
    label = paste0(
      "On the straight stretch, mortality<br>doubles roughly every **", dt_men,
      " years** of age"
    ),
    family = base_text, size = 2.5, color = "gray30",
    fill = NA, label.color = NA, hjust = 0
  ) +
  scale_x_continuous(breaks = seq(0, 90, 15), expand = expansion(c(0.01, 0.02))) +
  scale_y_log10(
    labels = label_log(),
    breaks = c(1e-4, 1e-3, 1e-2, 1e-1, 1)
  ) +
  annotation_logticks(sides = "l", colour = "gray60", size = 0.3) +
  scale_color_manual(values = colors_sex) +
  scale_fill_manual(values = colors_sex) +
  labs(
    title = "A straight line through death",
    subtitle = paste0(
      "Brazilian mortality, 2022. The yearly chance of dying climbs almost ",
      "perfectly *exponentially* with age, so on a log scale it is nearly a ",
      "straight line, the **Gompertz law**. The line is fit on ages ", fit_lo,
      "-", fit_hi, " (<span style='color:#466C6F'>**men**</span> and ",
      "<span style='color:#B35144'>**women**</span>) and extrapolated beyond. ",
      "The deviations are the story."
    ),
    x = "Exact age",
    y = "Annual mortality hazard (log scale)",
    caption = paste0(
      "Source: IBGE, Tábuas Completas de Mortalidade (2022). Hazard ",
      "μ(x) = -log(1 - q(x)); Gompertz fit by OLS on log μ over ages ",
      fit_lo, "-", fit_hi, ". @viniciusoike"
    )
  ) +
  theme_plot

ggsave(
  here("2026", "plots", "28_modelling.png"),
  p,
  width = 8,
  height = 6,
  dpi = 400
)
