library(astsa)
library(ggplot2)
library(dplyr)
library(ragg)

# Converte o objeto para data.frame
temperature <- tibble(
  year = as.numeric(time(gtemp_both)),
  temp = as.numeric(gtemp_both)
)

# Paleta de cores
blues <- RColorBrewer::brewer.pal(9, "Blues")[4:9]
reds <- RColorBrewer::brewer.pal(9, "Reds")[4:9]

palette <- c(rev(blues), reds)

# Data.frames auxiliares para plotar as anotações de texto
df_aux_title <- tibble(x = 1940, y = 0, label = "The Climate Issue")
df_aux_anos <- tibble(
  label = seq(1860, 2020, 40),
  x = c(1865, 1900, 1940, 1980, 2010)
)

p <- ggplot() +
  geom_tile(data = temperature, aes(x = year, y = 0, fill = temp)) +
  geom_text(
    data = df_aux_anos,
    aes(x = x, y = 0, label = label),
    vjust = 1.5,
    colour = "white",
    size = 5,
    family = "Georgia") +
  geom_text(
    data = df_aux_title,
    aes(x = x, y = 0.05, label = label),
    family = "Georgia",
    size = 10,
    colour = "white") +
  geom_hline(yintercept = 0, colour = "white", linewidth = 1) +
  scale_fill_stepsn(colors = palette, breaks = round(seq(-0.6, 1.2, 0.2), 1)) +
  guides(fill = "none") +
  labs(x = NULL, y = NULL) +
  theme_void() +
  theme(
    axis.title = element_blank(),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.background = element_rect(fill = NA),
    plot.margin = margin(c(0, 0, 0, 0))
  )

ggsave(here::here("plots/11_stripes.png"), p, width = 6, height = 8)
