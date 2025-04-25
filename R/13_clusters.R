library(ggplot2)
library(sf)

poa <- aopdata::read_access("Porto Alegre", mode = "car", geometry = TRUE)
spo <- aopdata::read_access("sao paulo", mode = "car", geometry = TRUE)


ggplot() +
  geom_sf(data = poa, aes(fill = sqrt(CMATT15)), color = "white", linewidth = 0.1) +
  scale_fill_viridis_c() +
  coord_sf()

BAMMtools::getJenksBreaks(spo$CMATT30, k = 8)

library(ragg)

font <- "DIN Alternate"

scale_option = "inferno"
scale_name = "Job Acessibility Index (30 min. car)"
scale_breaks = c(0.2, 0.8)
scale_labels = c("Less Job\nOpportunities", "More Job\nOpportunities")

offwhite <- "#f6eee3"

scale_zero_one <- function(x) {
  (x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
}

spo <- spo |>
  dplyr::mutate(
    scale_jobs = scale_zero_one(ifelse(CMATT15 > 0, sqrt(CMATT15), NA))
  )

plot_map <- ggplot() +
  geom_sf(data = spo, fill = "gray80", color = NA) +
  geom_sf(
    data = dplyr::filter(spo, scale_jobs > 0),
    aes(fill = scale_jobs, color = scale_jobs)
  ) +
  scale_fill_viridis_c(
    option = scale_option,
    name = scale_name,
    breaks = scale_breaks,
    labels = scale_labels
  ) +
  scale_color_viridis_c(
    option = scale_option,
    name = scale_name,
    breaks = scale_breaks,
    labels = scale_labels
  ) +
  coord_sf(xlim = c(-46.82, -46.37), ylim = c(-23.98, -23.365)) +
  ggtitle("Acessibility to Opportunities in São Paulo") +
  ggthemes::theme_map(base_family = font) +
  theme(
    panel.background = element_rect(fill = offwhite, color = offwhite),
    plot.background = element_rect(fill = offwhite, color = offwhite),
    legend.background = element_rect(fill = offwhite, color = offwhite),
    legend.position = "top",
    legend.title.position = "top",
    legend.title = element_text(size = 14, hjust = 0.5),
    legend.justification = 0.5,
    legend.key.size = unit(0.5, "cm"),
    legend.key.width = unit(1.25, "cm"),
    legend.text = element_text(size = 10),
    legend.margin = margin(),
    plot.margin = margin(10, 5, 5, 10),
    plot.title = element_text(size = 18, hjust = 0.5),
    plot.subtitle = element_text(size = 10, hjust = 0.5)
  )

ggsave(here::here("plots/13_clusters.png"), plot_map, width = 8, height = 9)
