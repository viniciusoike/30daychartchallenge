# Prompt: Distributions
# FlowingData (tribute) -- "The years you have left, probably"
# Homage to Nathan Yau's lifespan simulation, using IBGE's 2022 life table.

library(dplyr)
library(ggplot2)
library(ggdist)
library(tidyr)

import::from(readxl, read_excel)
import::from(here, here)

# Data --------------------------------------------------------------------
# IBGE Tabuas Completas de Mortalidade 2022 (xlsx per sex). Column 1 is exact
# age X, column 4 is l(X) = survivors at age X (radix 100,000), column 7 is
# E(X) = life expectancy. The top age is the open interval "90 ou mais".

read_lt <- function(file, sex) {
  raw <- read_excel(file, sheet = 1, col_names = FALSE, skip = 5)
  age_raw <- raw[[1]]
  age <- ifelse(
    grepl("ou mais", age_raw),
    90L,
    suppressWarnings(as.integer(age_raw))
  )
  tibble(
    age = age,
    lx = suppressWarnings(as.numeric(raw[[4]])),
    ex = suppressWarnings(as.numeric(raw[[7]]))
  ) |>
    filter(!is.na(age), !is.na(lx)) |>
    mutate(sex = sex)
}

lt_dir <- here("2026", "data", "lifetable")

# Download the life tables from the IBGE FTP on first run (data/ is gitignored).
ftp <- "https://ftp.ibge.gov.br/Tabuas_Completas_de_Mortalidade/Tabuas_Completas_de_Mortalidade_2022/xlsx"
if (!dir.exists(lt_dir)) {
  dir.create(lt_dir, recursive = TRUE)
}
for (f in c("homens.xlsx", "mulheres.xlsx", "ambos_os_sexos.xlsx")) {
  dest <- file.path(lt_dir, f)
  if (!file.exists(dest)) {
    download.file(file.path(ftp, f), dest, mode = "wb", quiet = TRUE)
  }
}

lt <- bind_rows(
  read_lt(file.path(lt_dir, "homens.xlsx"), "Men"),
  read_lt(file.path(lt_dir, "mulheres.xlsx"), "Women")
)

# Remaining-life distribution ---------------------------------------------
# Given alive at age a, the age-at-death CDF is F(x) = (S(a) - S(x)) / S(a),
# with S(x) = l(x) / radix. Within a year deaths are spread uniformly (linear
# l(x)); the 90+ interval is closed with a constant-hazard (exponential) tail
# of mean E(90), the standard life-table closeout. Validated to reproduce the
# published E(X) within ~0.03 years at every age.

remaining_quantiles <- function(d, a, probs) {
  radix <- d$lx[1]
  l90 <- tail(d$lx, 1)
  e90 <- tail(d$ex, 1)
  grid <- seq(a, 115, by = 0.02)
  S <- ifelse(
    grid <= 90,
    approx(d$age, d$lx, xout = pmin(grid, 90))$y / radix,
    (l90 / radix) * exp(-(grid - 90) / e90)
  )
  Sa <- approx(d$age, d$lx, xout = a)$y / radix
  cdf <- (Sa - S) / Sa
  age_at_death <- approx(cdf, grid, xout = probs, ties = "ordered")$y
  age_at_death - a
}

ref_ages <- c(20, 40, 60, 80)
probs <- (seq_len(100) - 0.5) / 100 # 100 equally-likely lives

dots <- list()
i <- 1
for (s in c("Men", "Women")) {
  for (a in ref_ages) {
    d <- filter(lt, sex == s)
    dots[[i]] <- tibble(sex = s, a = a, rem = remaining_quantiles(d, a, probs))
    i <- i + 1
  }
}

age_levels <- paste0("At age ", ref_ages)
dots <- bind_rows(dots) |>
  mutate(
    sex = factor(sex, levels = c("Men", "Women")),
    age_lab = factor(paste0("At age ", a), levels = age_levels)
  )

meds <- dots |>
  summarise(med = median(rem), .by = c("sex", "a")) |>
  mutate(
    sex = factor(sex, levels = c("Men", "Women")),
    age_lab = factor(paste0("At age ", a), levels = age_levels),
    lab = paste0(round(med), " more years")
  )

# Theme -------------------------------------------------------------------

base_text <- "Lato"
title_text <- "Lora"
offwhite <- "#f5f5dc"

colors_sex <- c("Men" = "#466C6F", "Women" = "#B35144")

theme_plot <- theme_minimal(base_family = base_text) +
  theme_sub_panel(
    grid.minor = element_blank(),
    grid.major.y = element_blank(),
    grid.major.x = element_line(color = "gray80", linewidth = 0.3),
    spacing.x = unit(12, "pt"),
    spacing.y = unit(10, "pt"),
    background = element_rect(fill = offwhite, color = offwhite)
  ) +
  theme_sub_plot(
    background = element_rect(fill = offwhite, color = offwhite),
    margin = margin(14, 16, 8, 14),
    title = element_text(family = title_text, size = 17, hjust = 0),
    subtitle = element_text(
      family = base_text,
      size = 9.5,
      color = "gray25",
      margin = margin(3, 0, 14, 0)
    ),
    caption = element_text(
      family = base_text,
      size = 6,
      color = "gray50",
      hjust = 0
    )
  ) +
  theme_sub_axis_x(
    line = element_line(color = "gray20", linewidth = 0.3),
    title = element_text(size = 9, color = "gray30"),
    text = element_text(size = 7.5, color = "gray30")
  ) +
  theme_sub_axis_y(
    text = element_blank(),
    title = element_blank()
  ) +
  theme_sub_strip(
    text = element_text(size = 10, family = base_text, color = "#ffffff"),
    background = element_rect(fill = "#0D1B2A", color = "#ffffff")
  ) +
  theme(
    legend.position = "none",
    panel.spacing.x = unit(14, "pt"),
    panel.spacing.y = unit(10, "pt")
  )

# Plot --------------------------------------------------------------------

panel <- ggplot(dots, aes(x = rem, fill = sex, color = sex)) +
  geom_dots(binwidth = 1.5, color = NA, overflow = "compress") +
  geom_hline(yintercept = 0, color = "gray10", linewidth = 0.3) +
  geom_vline(
    data = meds,
    aes(xintercept = med, color = sex),
    linewidth = 0.4,
    linetype = "22"
  ) +
  geom_text(
    data = meds,
    aes(x = med, y = 0.96, label = lab),
    inherit.aes = FALSE,
    hjust = -0.06,
    vjust = 1,
    size = 2.6,
    family = base_text,
    color = "gray25"
  ) +
  facet_grid(rows = vars(age_lab), cols = vars(sex)) +
  scale_x_continuous(
    breaks = seq(0, 80, 20),
    limits = c(0, 88),
    expand = expansion(c(0.01, 0.02))
  ) +
  scale_y_continuous(NULL, breaks = NULL, expand = expansion(c(0, 0.05))) +
  scale_fill_manual(values = colors_sex) +
  scale_color_manual(values = colorspace::darken(colors_sex, 0.2)) +
  labs(
    title = "The years you have left, probably",
    subtitle = "For a Brazilian alive today, each panel shows 100 equally-likely lives — one dot per possible number of remaining\nyears. Dashed line marks the median. As you survive longer, the cloud shifts left and tightens; women's runs a bit longer.",
    x = "More years of life",
    caption = "Source: IBGE, Tábuas Completas de Mortalidade (2022). Quantile dotplot of remaining lifespan; the 90+ age group is closed with a constant-hazard tail. @viniciusoike\nA tribute to Nathan Yau's \"Years You Have Left to Live, Probably\" (FlowingData, 2015)."
  ) +
  theme_plot

ggsave(
  here("2026/plots/12_flowingdata.png"),
  panel,
  width = 8,
  height = 8.5,
  dpi = 400
)
