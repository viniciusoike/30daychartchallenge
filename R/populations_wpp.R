library(wpp2024)
library(data.table)
library(ggplot2)
library(ggbump)
import::from(stringr, str_replace_all)

data(popproj1dt)
data(pop1dt)
data(UNlocations)

setDT(UNlocations)

dim_loc <- UNlocations[, .(name, country_code, location_type, tree_level)]
pop_proj <- popproj1dt[year %in% c("2040", "2060", "2080", "2100")]
pop_proj <- merge(
  pop_proj,
  dim_loc,
  by = c("name", "country_code"),
  all.x = TRUE
)

sel_cols <- c("name", "country_code", "year", "pop")

pop_hist <- pop1dt[year %in% c(1960, 1980, 2000, 2020), ..sel_cols]
pop_hist <- merge(
  pop_hist,
  dim_loc,
  by = c("name", "country_code"),
  all.x = TRUE
)
pop_hist <- pop_hist[tree_level == 5]

pop_country_hist <- pop_hist[order(-pop)][, ranking := order(-pop), by = "year"]


pop_country_proj <- pop_proj[tree_level == 5, ..sel_cols]
pop_country_proj <- pop_country_proj[order(-pop)][,
  ranking := order(-pop),
  by = "year"
]

short_names <- c(
  "Democratic Republic of the Congo" = "Congo",
  "United Republic of Tanzania" = "Tanzania",
  "Iran \\(Islamic Republic of\\)" = "Iran",
  "Bolivia \\(Plurinational State of\\)" = "Bolivia",
  "Venezuela \\(Bolivarian Republic of\\)" = "Venezuela",
  "Russian Federation" = "Russia",
  "Republic of Korea" = "South Korea",
  "United States of America" = "USA"
)
pop_country <- rbind(pop_country_proj, pop_country_hist, fill = TRUE)
pop_country[, name := str_replace_all(name, short_names)]

sel_countries <- c("USA", "Brazil", "Nigeria", "India", "Italy")

pop_country[, is_highlight := 0]
pop_country[name == "Brazil", is_highlight := 1]
pop_country[, is_highlight := factor(is_highlight)]
pop_country[, name_highlight := NA_character_]
pop_country[is_highlight == 1, name_highlight := name]

sel_year <- seq(1960, 2100, 40)


plot_col <- function(y) {
  subdata <- subset(pop_country, year == y)
  subdata <- subdata[order(pop)]
  subdata[, name := factor(name, levels = unique(subdata$name))]

  font_text <- "Helvetice Neue"

  ggplot(subdata, aes(name, pop, fill = is_highlight)) +
    geom_col(width = 0.8) +
    geom_text(
      aes(label = round(pop / 1000)),
      family = font_text,
      size = 3,
      color = "black",
      nudge_y = 100000
    ) +
    geom_hline(yintercept = 0) +
    coord_flip() +
    scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
    guides(fill = "none") +
    labs(subtitle = y) +
    theme_minimal() +
    theme(
      panel.grid = element_blank()
    )
}

library(patchwork)

p1 <- plot_col(1980)
p2 <- plot_col(2020)
p3 <- plot_col(2060)
p4 <- plot_col(2100)

p1 | p2 | p3 | p4


subdata <- subset(pop_country_proj, year == 2040)
subdata <- subdata[order(pop)]
subdata[, name := factor(name, levels = unique(subdata$name))]

library(ragg)

font_text <- "Helvetice Neue"

ggplot(subdata, aes(name, pop, fill = is_highlight)) +
  geom_col(width = 0.8) +
  geom_text(
    aes(label = round(pop / 1000)),
    family = font_text,
    size = 3,
    color = "black",
    nudge_y = 100000
  ) +
  geom_hline(yintercept = 0) +
  coord_flip() +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  guides(fill = "none") +
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  )
