library(ggplot2)
library(showtext)

# Fonts matching SCMP's web style
font_add_google("Merriweather", "merriweather")
font_add_google("Roboto", "roboto")
showtext_auto()

# SCMP-inspired color palette
scmp_red <- "#D7282F"
scmp_blue <- "#1D6FA4"
scmp_dark <- "#1A1A1A"
scmp_gray <- "#9E9E9E"
scmp_bg <- "#F7F4EF" # warm off-white, common in SCMP web charts

theme_scmp <- function(base_size = 12) {
  theme_minimal(base_size = base_size) +
    theme(
      text = element_text(family = "roboto", color = scmp_dark),
      plot.title = element_text(
        family = "merriweather",
        face = "bold",
        size = base_size * 1.6,
        color = scmp_dark,
        margin = margin(b = 4)
      ),
      plot.subtitle = element_text(
        family = "roboto",
        size = base_size * 1.0,
        color = "#444444",
        margin = margin(b = 12)
      ),
      plot.caption = element_text(
        family = "roboto",
        size = base_size * 0.75,
        color = scmp_gray,
        hjust = 0
      ),
      plot.background = element_rect(fill = scmp_bg, color = NA),
      panel.background = element_rect(fill = scmp_bg, color = NA),
      panel.grid.major = element_line(color = "#DEDAD5", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      axis.text = element_text(
        family = "roboto",
        color = "#555555",
        size = base_size * 0.85
      ),
      axis.title = element_blank(), # SCMP rarely shows axis titles
      axis.ticks = element_blank(),
      legend.position = "none" # prefer direct labels
    )
}
