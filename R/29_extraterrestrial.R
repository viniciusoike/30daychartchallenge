library(rvest)
library(dplyr)
library(purrr)
library(tidyverse)

url <- "https://en.wikipedia.org/wiki/List_of_films_featuring_extraterrestrials"
wikipage <- read_html(url)

alien_movies <- wikipage |>
  html_table() %>%
  .[[1]]


# Box-office data ---------------------------------------------------------

# Structure urls
url_mojo <- str_glue("https://www.boxofficemojo.com/year/{y}/?ref_=bo_yl_table_2", y = 1977:2024)

# Functions for extraction

extract_table <- function(url) {

  parsed <- rvest::read_html(url)
  tbl <- rvest::html_table(parsed)
  tbl <- tbl[[1]]

  return(tbl)

}

extract_number <- Vectorize(function(x) {
  y <- paste(unlist(stringr::str_extract_all(x, "\\d+")), collapse = "")
  return(as.numeric(y))
})

clean_table <- function(.dat) {
  .dat |>
    janitor::clean_names() |>
    dplyr::mutate(dplyr::across(c(budget, gross, theaters, total_gross), extract_number))
}

# Wrapper function to import boxoffice information for a given url/year
get_boxoffice <- function(url) {

  year <- stringr::str_extract(url, "(?<=year/)[0-9]{4}")
  tbl <- extract_table(url)
  tbl <- clean_table(tbl)
  tbl$calendar_year <- year
  return(tbl)

}
# Safely get all links
safe_get_boxoffice <- purrr::safely(get_boxoffice)
ls_boxoffice <- purrr::map(url_mojo, safe_get_boxoffice)
# Check failures
any(sapply(boxoffice, is.null) == TRUE)
# Stack data together
boxoffice <- bind_rows(map(ls_boxoffice, pluck, 1))

boxoffice <- boxoffice |>
  mutate(
    calendar_year = as.numeric(calendar_year),
    decade = calendar_year %/% 10 * 10
  )

# Alien Movies ratings
alien <- tribble(
  ~film, ~year, ~imdb_rating, ~metacritic_rating,
  "Alien", 1979, 8.5, 8.9,
  "Aliens", 1986, 8.4, 8.4,
  "Alien 3", 1992, 6.4, 5.9,
  "Alien Resurrection", 1997, 6.2, 6.2,
  "Prometheus", 2012, 7.0, 6.4,
  "Alien: Covenant", 2017, 6.4, 6.5,
  "Alien: Romulus", 2024, 7.1, 6.4,
  "Alien vs Predator", 2004, 5.7, 2.9,
  "Alien vs Predator: Requiem", 2007, 4.6, 2.9
)

alien <- alien |>
  mutate(film = factor(film), film = forcats::fct_reorder(film, metacritic_rating))

alien <- alien |>
  mutate(
    pos_text = max(c(imdb_rating, metacritic_rating)) + 0.25,
    .by = "film"
  )
library(emoGG)

emoji_search("film")

ggplot(alien, aes(film)) +
 geom_emoji(aes(y = imdb_rating), emoji = "1f37f") +
  geom_emoji(aes(y = metacritic_rating), emoji = "1f3a6")
  geom_point(aes(y = metacritic_rating), size = 3, shape = 2) +
  geom_label(
    aes(y = pos_text, label = str_wrap(film, 18)),
    size = 4,
    hjust = 0,
    family = "Futura") +
  scale_y_continuous(breaks = seq(3, 9, 1)) +
  coord_flip() +
  theme_minimal(base_family = "Futura") +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.y = element_blank()
  )

## Join datasets -----------------------------------------------------------

sub_movies <- alien_movies |>
  select(
    name_movie = Title,
    year_movie = Year
  ) |>
  filter(year_movie >= 1977, year_movie <= 2024) |>
  mutate(
    uk1 = row_number(),
    key = paste(year_movie, name_movie))

swap_names <- function(text) {

  replacement <- c(
    "Alien³" = "Alien 3"
  )

  str_replace_all(text, replacement)

}

sub_boxoffice <- boxoffice |>
  select(
    name_movie = release,
    year_movie = calendar_year,
    gross,
    total_gross
  ) |>
  mutate(
    name_movie = swap_names(name_movie),
    key = paste(year_movie, name_movie),
    uk2 = row_number()
  )

# boxoffice |>
#   filter(str_detect(release, "Venom"))
#
# sub_boxoffice <- sub_boxoffice |>
#   summarise(total = max(total_gross), year = min(year_movie), .by = "name_movie") |>
#   mutate(
#     uk2 = row_number(),
#     year = ifelse(name_movie == "Venom", 2018, year),
#     key = paste(year, name_movie)
#     )

library(fedmatch)

fmerge <- fedmatch::merge_plus(
  sub_movies,
  sub_boxoffice,
  by = "key",
  unique_key_1 = "uk1",
  unique_key_2 = "uk2",
  match_type = "fuzzy"
  )

sub <- fmerge$matches

cpi <- read_csv("data/day_29/us_cpi.csv")
names(cpi) <- c("year", "rate")
cpi <- bind_rows(cpi, tibble(year = 2024, rate = 2.9))

cpi <- cpi |>
  mutate(
    cumulative = cumprod(1 + rate / 100)
  )

base_index <- cpi |>
  filter(year == 2019) |>
  pull(cumulative)

cpi_adjust <- cpi |>
  mutate(
    base = local(base_index),
    inflation_factor = base / cumulative
  ) |>
  select(year, inflation_factor)

yearly_bo <- sub |>
  summarise(
    total = sum(total_gross, na.rm = TRUE),
    .by = "year_movie_1"
  ) |>
  arrange(year_movie_1)

yearly_bo <- yearly_bo |>
  left_join(cpi_adjust, by = c("year_movie_1" = "year")) |>
  mutate(adjusted = total * inflation_factor / 1e6)

# Plot elements -----------------------------------------------------------


# Plot --------------------------------------------------------------------

sub |>
  filter(year_movie_1 %in% 2000:2020) |>
  group_by(year_movie_1) |>
  slice_max(total_gross, n = 1)



sel_years <- c(1982, 1986, 1993, 1996, 1997, 2002, 2009, 2017, 2019, 2024)

df_text <- sub |>
  filter(year_movie_1 %in% sel_years) |>
  group_by(year_movie_1) |>
  slice_max(total_gross, n = 1)

df_label <- yearly_bo |>
  filter(year_movie_1 %in% sel_years) |>
  select(year_movie_1, adjusted) |>
  left_join(df_text, by = c("year_movie_1")) |>
  select(x = year_movie_1, y = adjusted, label = name_movie_1)

library(showtext)
library(ragg)
font_add_google("IBM Plex Mono", "IBM Plex Mono")
# font_add_google("Fira Code", "Fira Code")
showtext_auto()

font <- "IBM Plex Mono"
# font <- "Fira Code"

df_axis <- tibble(
  x = 1972,
  y = seq(500, 3000, 500),
  formatted_y = format(y, big.mark = ","),
  label = if_else(y == 3000, paste(formatted_y, "Million US$ (constant 2019)"), formatted_y)
)

df_segment <- df_label |>
  mutate(
    xend = x,
    yend = if_else(x == 1996, y + 1000, y + 500)
  )

space_background <- "#03214a"
pal <- c("#D88F00", "#ffffff")
pal <- c("#258073", "#ffffff")
pal_scifi <- c("#06d0ce", "#09f6f8", "#076a6c", "#051416", "#d5dadb", "#04454a", "#098b94", "#05947c", "#fefefe", "#022123")
space_background <- pal_scifi[10]
color_text_label <- pal_scifi[2]
color_axis_label <- pal_scifi[5]
fill_text_label <- pal_scifi[4]

p_col <- ggplot(yearly_bo, aes(year_movie_1, adjusted)) +
  geom_col(fill = pal_scifi[7]) +
  geom_hline(yintercept = 0, color = pal_scifi[9], linewidth = 1) +
  geom_segment(
    data = df_segment,
    aes(x = x, xend = xend, y = y, yend = yend),
    color = "gray90",
  ) +
  geom_text(
    data = df_axis,
    aes(x, y + 75, label = label),
    size = 3,
    family = font,
    color = color_axis_label,
    hjust = 0
  ) +
  geom_label(
    data = df_label,
    aes(x, y = y + 500, label = str_wrap(label, 11)),
    family = font,
    size = 3,
    nudge_y = c(0, 0, 0, 500, 0, 0, 0, 0, 0, 0),
    nudge_x = c(0, 0, 0, 0, 0, 0, 0, -1.5, 0, 1.5),
    color = color_text_label,
    fill = fill_text_label
  ) +
  scale_x_continuous(breaks = c(1977, seq(1980, 2020, 5), 2024)) +
  scale_y_continuous(breaks = seq(500, 3000, 500), expand = expansion(mult = c(0, 0.1))) +
  labs(
    x = NULL,
    y = NULL,
    title = "Billions from outer space",
    subtitle = "US box-office earnings from major films featuring some form of extraterrestrial life.",
    caption = "Sources: Wikipedia (List of films featuring extraterrestrials); Box Office Mojo; FRED."
  ) +
  theme_minimal(base_family = font) +
  theme(
    text = element_text(color = "#fefefe"),
    axis.text = element_text(color = "#fefefe", size = 12),
    axis.text.y = element_blank(),
    axis.ticks.x = element_line(colour = "#fefefe", linewidth = 0.5, lineend = "round"),
    panel.background = element_rect(fill = space_background, color = space_background),
    plot.background = element_rect(fill = space_background, color = space_background),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    plot.title = element_text(size = 20),
    plot.margin = margin(20, 10, 5, 10)
  )
showtext_opts(dpi = 300)
showtext_auto()
ggsave("plots/29_extraterrestrial.png", p_col, width = 9, height = 6)
