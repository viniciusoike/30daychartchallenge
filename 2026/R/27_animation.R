# Day 27 - Animation (Uncertainties): 100 futures for Brazilian home prices --
# A hypothetical outcome plot. Each frame replays one real five-year stretch
# of the BCB residential price index (IVG-R, deflated by IPCA) since 2001,
# starting from today's price. Ghost paths pile up on the left; the endpoints
# stack into a dot histogram on the right. No model, just history on repeat.

library(dplyr)
library(ggplot2)
library(ggtext)
library(patchwork)
import::from(here, here)
import::from(lubridate, years)
import::from(ragg, agg_png)
import::from(rbcb, get_series)
import::from(magick, image_read, image_join, image_animate, image_write)

# Data ----------------------------------------------------------------------
# IVG-R (SGS 21340): value of homes used as mortgage collateral.
# IPCA (SGS 433): monthly consumer price inflation, in percent.

data_dir <- here("2026", "data", "ivgr")
cache <- file.path(data_dir, "ivgr_ipca.rds")

if (!file.exists(cache)) {
  dir.create(data_dir, recursive = TRUE, showWarnings = FALSE)
  raw <- get_series(
    c(ivgr = 21340, ipca = 433),
    start_date = "2001-01-01",
    as = "tibble"
  )
  saveRDS(raw, cache)
}
raw <- readRDS(cache)

# Wrangle -------------------------------------------------------------------
# Real index rebased so the latest month = 100.

prices <- inner_join(raw$ivgr, raw$ipca, by = "date") |>
  arrange(date) |>
  mutate(
    real = ivgr / cumprod(1 + ipca / 100),
    real = 100 * real / last(real)
  )

origin <- max(prices$date)
horizon <- 60
n_futures <- 100

history <- prices |>
  filter(date >= origin - years(10)) |>
  select(date, value = real)

## Replay every five-year window --------------------------------------------
# Window k starts at month k; its path is rescaled to start at 100 and
# shifted forward so it begins at the forecast origin.

n_windows <- nrow(prices) - horizon

windows <- lapply(seq_len(n_windows), \(k) {
  idx <- k:(k + horizon)
  path <- tibble(
    step = 0:horizon,
    date = seq(origin, by = "month", length.out = horizon + 1),
    value = 100 * prices$real[idx] / prices$real[k],
    replay_start = prices$date[k],
    replay_end = prices$date[k + horizon]
  )
  return(path)
})
windows <- bind_rows(windows, .id = "window")

set.seed(2027)
draws <- sample(unique(windows$window), n_futures)

futures <- windows |>
  filter(window %in% draws) |>
  mutate(
    frame = match(window, draws),
    change = last(value) / 100 - 1,
    direction = if_else(change > 0, "up", "down"),
    .by = window
  ) |>
  arrange(frame, step)

endpoints <- futures |>
  filter(step == horizon) |>
  arrange(frame)

## Dot histogram ------------------------------------------------------------
# Bin endpoints on the price axis; each future becomes one dot in its bin.
# Bins are anchored at 100 so no bin mixes rises and falls.

bin_width <- 5

endpoints <- endpoints |>
  mutate(bin = 100 + bin_width * (floor((value - 100) / bin_width) + 0.5)) |>
  mutate(slot = row_number(), .by = bin)

max_slot <- max(endpoints$slot)

# Plot ----------------------------------------------------------------------

offwhite <- "#f5f5dc"
col_up <- "#3E6B6F"
col_down <- "#B5523B"
col_hist <- "#1A1A1A"
pal <- c(up = col_up, down = col_down)

font_title <- "Lora"
font_text <- "Lato"

y_limits <- c(70, 215)
y_breaks <- seq(75, 200, 25)

theme_plot <- theme_minimal(base_family = font_text, base_size = 11) +
  theme_sub_plot(
    title = element_text(family = font_title, size = 20, color = col_hist),
    title.position = "plot",
    subtitle = element_textbox_simple(
      size = 11,
      color = "gray25",
      lineheight = 1.15,
      margin = margin(t = 6, b = 12)
    ),
    caption = element_text(size = 8, color = "gray50", hjust = 0),
    caption.position = "plot",
    background = element_rect(fill = offwhite, color = NA),
    margin = margin(18, 18, 10, 18)
  ) +
  theme_sub_panel(
    grid.minor = element_blank(),
    grid.major.x = element_blank(),
    grid.major.y = element_line(color = "gray85", linewidth = 0.3)
  ) +
  theme_sub_axis_bottom(
    text = element_text(color = "gray35"),
    line = element_line(color = "gray30", linewidth = 0.4)
  ) +
  theme_sub_axis_left(text = element_text(color = "gray35")) +
  theme(legend.position = "none")

## Frame builder ------------------------------------------------------------
# `i` is the number of futures drawn so far. `i = n_futures` with
# `highlight = FALSE` gives the static poster.

plot_frame <- function(i, highlight = TRUE) {
  drawn <- filter(futures, frame <= i)
  ghosts <- if (highlight) filter(drawn, frame < i) else drawn
  current <- filter(futures, frame == i)
  current_end <- filter(endpoints, frame == i)
  dots <- filter(endpoints, frame <= i)

  p_paths <- ggplot(mapping = aes(date, value)) +
    annotate(
      "rect",
      xmin = origin,
      xmax = max(futures$date),
      ymin = -Inf,
      ymax = Inf,
      fill = "white",
      alpha = 0.35
    ) +
    geom_hline(
      yintercept = 100,
      color = "gray40",
      linewidth = 0.3,
      linetype = 2
    ) +
    geom_line(
      data = ghosts,
      aes(group = window, color = direction),
      linewidth = 0.35,
      alpha = if (highlight) 0.18 else 0.28
    ) +
    geom_line(data = history, color = col_hist, linewidth = 0.9) +
    annotate(
      "text",
      x = origin - 60,
      y = y_limits[1] + 4,
      label = "← Observed",
      hjust = 1,
      family = font_text,
      size = 3.3,
      color = "gray40"
    ) +
    annotate(
      "text",
      x = origin + 60,
      y = y_limits[1] + 4,
      label = "Replayed →",
      hjust = 0,
      family = font_text,
      size = 3.3,
      color = "gray40"
    ) +
    scale_color_manual(values = pal) +
    scale_x_date(
      date_breaks = "2 years",
      date_labels = "%Y",
      expand = expansion(mult = c(0.01, 0.02))
    ) +
    scale_y_continuous(breaks = y_breaks, position = "left") +
    coord_cartesian(ylim = y_limits, clip = "off") +
    labs(x = NULL, y = NULL)

  if (highlight) {
    label_replay <- sprintf(
      "<span style='color:gray40'>Future %d of %d · replaying</span><br><b>%s – %s</b>",
      i,
      n_futures,
      format(current$replay_start[1], "%b %Y"),
      format(current$replay_end[1], "%b %Y")
    )
    label_change <- scales::label_percent(
      accuracy = 1,
      style_positive = "plus",
      style_negative = "minus"
    )(current_end$change)

    p_paths <- p_paths +
      geom_line(
        data = current,
        aes(color = direction),
        linewidth = 1.3
      ) +
      geom_point(
        data = current_end,
        aes(color = direction),
        size = 2.6
      ) +
      annotate(
        "text",
        x = current_end$date + 45,
        y = current_end$value,
        label = label_change,
        hjust = 0,
        family = font_text,
        fontface = "bold",
        size = 4,
        color = pal[current_end$direction]
      ) +
      annotate(
        "richtext",
        x = min(history$date),
        y = y_limits[2],
        label = label_replay,
        hjust = 0,
        vjust = 1,
        family = font_text,
        size = 3.6,
        lineheight = 1.2,
        fill = NA,
        label.color = NA,
        label.padding = unit(0, "pt")
      )
  }

  p_dots <- ggplot(dots, aes(slot, bin, color = direction)) +
    geom_hline(
      yintercept = 100,
      color = "gray40",
      linewidth = 0.3,
      linetype = 2
    ) +
    geom_point(size = 1.7) +
    scale_color_manual(values = pal) +
    scale_x_continuous(limits = c(0.5, max_slot + 0.5), expand = expansion(0)) +
    scale_y_continuous(breaks = y_breaks) +
    coord_cartesian(ylim = y_limits, clip = "off") +
    labs(x = NULL, y = NULL)

  if (highlight) {
    p_dots <- p_dots +
      geom_point(
        data = current_end,
        shape = 21,
        size = 4.2,
        stroke = 0.9,
        fill = NA,
        color = col_hist
      )
  }

  i_up <- sum(dots$direction == "up")
  tally <- sprintf(
    paste0(
      "<b style='color:%s'>rose in %d</b> and ",
      "<b style='color:%s'>fell in %d</b> of %d futures"
    ),
    col_up,
    i_up,
    col_down,
    i - i_up,
    i
  )
  intro <- sprintf(
    paste0(
      "Each future replays one real five-year stretch of inflation-adjusted ",
      "home prices since 2001, starting from today's price (%s = 100). "
    ),
    format(origin, "%B %Y")
  )
  # Start years checked against `windows`: stretches starting 2002-2010 all
  # end higher, 2011 is mixed, and every one from 2012 on ends lower.
  subtitle <- if (highlight) {
    paste0(
      intro,
      "So far, prices ",
      tally,
      ". The dots on the right stack where each one ends."
    )
  } else {
    paste0(
      intro,
      "Prices ",
      tally,
      ". It looks like a coin toss, but history splits in two: nearly every ",
      "stretch that began before 2011 rode the mortgage boom up; every one ",
      "that began from 2012 on ended lower."
    )
  }

  # Dot panel drops its axes: the price scale is shared with the paths.
  p_dots <- p_dots +
    theme_plot +
    theme_sub_axis_bottom(text = element_blank(), line = element_blank()) +
    theme_sub_axis_left(text = element_blank())

  p <- (p_paths + theme_plot) +
    p_dots +
    plot_layout(widths = c(3, 1)) +
    plot_annotation(
      title = "Where will Brazilian home prices be in 2031?",
      subtitle = subtitle,
      caption = paste0(
        "Real residential price index (IVG-R deflated by IPCA), rebased to ",
        format(origin, "%b %Y"),
        " = 100. ",
        n_futures,
        " of the ",
        n_windows,
        " five-year windows since ",
        format(min(prices$date), "%b %Y"),
        ", drawn at random.\nSource: Banco Central do Brasil (SGS 21340, 433) ",
        "• @viniciusoike"
      ),
      theme = theme_plot
    )

  return(p)
}

# Save ----------------------------------------------------------------------

## Static poster ------------------------------------------------------------

ggsave(
  here("2026/plots/27_animation.png"),
  plot_frame(n_futures, highlight = FALSE),
  width = 10,
  height = 7,
  dpi = 300,
  device = agg_png
)

## Animation ----------------------------------------------------------------
# One PNG per frame, then stitched with magick. The poster closes the loop.

frame_dir <- file.path(tempdir(), "day27_frames")
dir.create(frame_dir, showWarnings = FALSE)

frame_files <- vapply(
  seq_len(n_futures),
  \(i) {
    path <- file.path(frame_dir, sprintf("frame_%03d.png", i))
    ggsave(
      path,
      plot_frame(i),
      width = 10,
      height = 7,
      dpi = 100,
      device = agg_png
    )
    return(path)
  },
  character(1)
)

poster_file <- file.path(frame_dir, "frame_poster.png")
ggsave(
  poster_file,
  plot_frame(n_futures, highlight = FALSE),
  width = 10,
  height = 7,
  dpi = 100,
  device = agg_png
)

# First futures linger so the reader learns the grammar; later ones speed up.
delays <- c(rep(80, 5), rep(35, 15), rep(15, n_futures - 20), 500)

gif <- image_read(c(frame_files, poster_file)) |>
  image_join() |>
  image_animate(delay = delays, optimize = TRUE)

image_write(gif, here("2026/plots/27_animation.gif"))
