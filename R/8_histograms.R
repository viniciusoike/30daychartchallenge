library(readxl)
library(dplyr)
library(ggplot2)
library(ragg)
library(patchwork)
import::from(here, here)


# Data --------------------------------------------------------------------



flags <- tibble(
  emoji = c("🇦🇷", "🇦🇺", "🇧🇷", "🇨🇱", "🇱🇾", "🇳🇴", "🇳🇿", "🇻🇪", "🇿🇲"),
  country = c("Argentina", "Australia", "Brazil", "Chile", "Libya", "Norawy", "New Zeland", "Venezuela", "Zambia"),
  countrycode = c("ARG", "AUS", "BRA", "CHL", "LBY", "NOR", "NZL", "VEN", "ZMB"),
  label = paste(countrycode, emoji)
)

dat <- read_excel(here("data/day_8/mpd2023_web.xlsx"), sheet = 5 )

countries <- c("ARG", "AUS", "BRA", "CHL", "LBY", "NOR", "NZL", "VEN", "ZMB")

subdat <- dat |>
  filter(
    countrycode %in% countries,
    year >= 1949,
    !is.na(gdppc)
    ) |>
  mutate(
    chg = (gdppc / lag(gdppc) - 1) * 100,
    .by = "countrycode"
  ) |>
  mutate(
    class = case_when(
      countrycode %in% c("ARG", "BRA", "NZL") ~ "agriculture",
      countrycode %in% c("AUS", "CHL", "ZMB") ~ "mining",
      countrycode %in% c("NOR", "VEN", "LBY") ~ "energy"
    )
  )

subdat <- left_join(subdat, flags, by = "countrycode")


# Plot elements -----------------------------------------------------------

font_title = "Gill Sans"
font_text = "Gill Sans"

# Colors
offwhite <- "#f5f5f5"
palette <- c("agriculture" = "#006D2C", "energy" = "#02818A", "mining" = "#2171b5")


# Plot function -----------------------------------------------------------

plot_histogram <- function(country, text_x = 5, text_y = 22.5) {

  sub <- subdat[subdat[["countrycode"]] == country, ]

  # Find the class and avg growth
  type <- unique(sub$class)
  avg_chg <- mean(sub$chg, na.rm = TRUE)

  # Number of bins
  nbins <- nclass.FD(na.omit(sub$chg))

  # Text label: avg growth
  text_label <- stringr::str_glue(
    "Avg. = {round(avg_chg, 1)}%"
  )

  ggplot(sub, aes(x = chg)) +
    geom_histogram(bins = 16, color = "white", fill = palette[type]) +
    geom_hline(yintercept = 0) +
    geom_vline(xintercept = 0) +
    geom_vline(xintercept = avg_chg, color = "gray30", linetype = 1) +
    geom_label(
      data = tibble(x = avg_chg + text_x, y = text_y, label = text_label),
      aes(x, y, label = label),
      family = font_text,
      size = 5
    ) +
    scale_x_continuous(limits = c(-14, 14), breaks = seq(-14, 14, 2)) +
    scale_y_continuous(limits = c(NA, 30)) +
    labs(x = "Annual GDP per capita growth (%)", y = "Frequency (N)", subtitle = unique(sub$label)) +
    theme_minimal(base_family = font_text) +
    theme(
      plot.background = element_rect(color = offwhite, fill = offwhite),
      panel.background = element_rect(color = offwhite, fill = offwhite),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(linetype = 2),
      axis.text.x = element_text(size = 12),
      axis.title.x = element_text(size = 14),
      plot.subtitle = element_text(size = 14, hjust = 0.5, family = font_title)
    )

}

params <- tibble(
  country = unique(subdat$countrycode),
  text_x = c(5, 5, 5, 5, 5, 6, 6, 5, 5),
  text_y = c(22.5, 28, 22.5, 22.5, 22.5, 22.5, 22.5, 22.5, 22.5)
)

plots <- purrr::pmap(params, plot_histogram)
names(plots) <- countries

adjust_plot <- function(p, type) {

  if (type == 1) {
    p <- p +
      labs(x = NULL, y = NULL) +
      theme(
        axis.text.x = element_blank(),
        axis.title.x = element_blank()
        )
  }

  if (type == 2) {
    p <- p +
      labs(x = NULL) +
      theme(
        axis.text.x = element_blank(),
        axis.title.x = element_blank()
      )
  }

  if (type == 3) {
    p <- p +
      labs(y = NULL)
  }

  if (type == 4) {
    p <- p +
      labs(x = NULL, y = NULL) +
      theme(
        axis.title.x = element_blank()
      )
  }

  return(p)

}


# Panel -------------------------------------------------------------------

bra <- plots$BRA +
  ggtitle("Agriculture") +
  theme(plot.title = element_text(size = 16, family = font_title, hjust = 0.5, color = palette["agriculture"]))

lby <- plots$LBY +
  ggtitle("Energy") +
  theme(plot.title = element_text(size = 16, family = font_title, hjust = 0.5, color = palette["energy"]))

aus <- plots$AUS +
  ggtitle("Mining") +
  theme(plot.title = element_text(size = 16, family = font_title, hjust = 0.5, color = palette["mining"]))

p1 <- adjust_plot(bra, 1) / adjust_plot(plots$ARG, 2) / adjust_plot(plots$NZL, 4)
p2 <- adjust_plot(lby, 1) / adjust_plot(plots$NOR, 1) / adjust_plot(plots$VEN, 3)
p3 <- adjust_plot(aus, 1) / adjust_plot(plots$CHL, 1) / adjust_plot(plots$ZMB, 4)

panel <- p1 | p2 | p3

panel <- panel + plot_annotation(
  title = "Comparing growth patterns among commodity exporters",
  subtitle = "Annual GDP per capita growth (%) from 1950 to 2022 among selected countries. Both x and y axis are fixed to facilitate cross-country comparisons.",
  caption = "Source: Maddison Project (2024).") &
  theme(
    plot.title = element_text(family = font_title, size = 22),
    plot.subtitle = element_text(family = font_text, size = 14, color = "gray20"),
    plot.caption = element_text(family = font_text, size = 12, color = "gray40"),
    plot.background = element_rect(color = offwhite, fill = offwhite)
    )

ggsave("plots/8_histograms.png", panel, width = 15, height = 10.9)
