library(echarts4r)
library(dplyr)
library(metrosp)
library(trendseries)

sub_series <- passengers_entrance |>
  filter(
    date >= "2019-01-01",
    date <= "2024-12-31",
    metric == "Average on Business Days"
  ) |>
  filter_out(line_number %in% c(15, 99))

base_index <- sub_series |>
  filter(year == 2019) |>
  summarise(base = mean(value, na.rm = TRUE), .by = "line_number")

index_series <- sub_series |>
  left_join(base_index, by = "line_number") |>
  mutate(index = value / base * 100) |>
  augment_trends(
    value_col = "index",
    group_cols = "line_number",
    methods = "stl",
    params = list(robust = TRUE)
  ) |>
  mutate(
    line_name = factor(
      line_name,
      levels = c("Blue", "Green", "Red", "Yellow", "Lilac")
    )
  ) |>
  group_by(line_name)

echarts_theme_vini <- function(
  palette = c("#171796", "#007A5E", "#ED2E38", "#FFD525", "#874ABF", "#8F8F8C"),
  font = "Avenir, 'Helvetica Neue', Arial, sans-serif",
  ink = "#1B2A3A",
  muted = "#6B7280",
  grid = "#E6E8EB",
  bg = "#f8fbf8"
) {
  axis_label <- list(color = muted, fontFamily = font, fontSize = 12)

  list(
    color = palette, # drives the default series palette
    backgroundColor = bg,
    textStyle = list(fontFamily = font, color = ink),

    title = list(
      left = "left",
      textStyle = list(
        fontFamily = font,
        color = ink,
        fontWeight = "bold",
        fontSize = 22
      ),
      subtextStyle = list(fontFamily = font, color = muted, fontSize = 16)
    ),

    line = list(lineStyle = list(width = 2.5)),

    # value axis = no spine, light dashed horizontal gridlines
    valueAxis = list(
      axisLine = list(show = FALSE),
      axisTick = list(show = FALSE),
      axisLabel = axis_label,
      splitLine = list(
        show = TRUE,
        lineStyle = list(color = grid, type = "dashed")
      )
    ),
    timeAxis = list(
      axisLine = list(show = TRUE, lineStyle = list(color = muted)),
      axisLabel = axis_label,
      splitLine = list(show = FALSE)
    ),

    legend = list(
      textStyle = list(color = ink, fontFamily = font),
      icon = "roundRect",
      itemWidth = 12,
      itemHeight = 12
    ),
    tooltip = list(
      backgroundColor = "rgba(255,255,255,0.96)",
      borderColor = grid,
      borderWidth = 1,
      textStyle = list(color = ink, fontFamily = font),
      axisPointer = list(
        type = "line",
        lineStyle = list(color = muted, type = "dashed")
      )
    )
  )
}

e_theme_vini <- function(e, ...) {
  theme <- jsonlite::toJSON(echarts_theme_vini(...), auto_unbox = TRUE)
  echarts4r::e_theme_custom(e, as.character(theme), name = "custom")
}

metro_chart <- e_charts(index_series, x = date) |>
  e_line(serie = trend_stl, showSymbol = FALSE, smooth = TRUE) |>
  e_tooltip(trigger = "axis") |>
  e_title(
    "Ridership on São Paulo Metro is still below Pre-pandemic levels",
    stringr::str_wrap(
      "Average monthly passenger entrances on the São Paulo Metro system (only business days) across lines 1 to 5. Line 15-Gray is removed from the analysis since most of its stations were inaugurated after 2020.",
      81
    ),
    itemGap = 15,
    left = "10%",
    right = "10%"
  ) |>
  e_grid(
    top = "20%",
    left = "10%",
    right = "10%",
    bottom = "10%"
  ) |>
  e_theme_vini()

metro_chart

## Export ----
htmlwidgets::saveWidget(
  metro_chart,
  tmp <- tempfile(fileext = ".html"),
  selfcontained = TRUE
)
webshot2::webshot(
  tmp,
  file = "2026/plots/22_new_tool.png",
  vwidth = 800,
  vheight = 600,
  zoom = 1, # 3x for crisp, high-DPI output
  delay = 2 # let the chart finish rendering before capture
)
