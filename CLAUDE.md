# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What this is

Personal submissions for the **#30DayChartChallenge** (a yearly April data-viz event, one chart per daily prompt). Not a package or app — a collection of standalone R scripts, each producing one publication-quality PNG. The author is Brazilian; most charts use Brazilian data and English-language labels/captions.

## Repository layout

Work is organized by year (`2025/`, `2026/`), each with the same three subfolders:

- `YEAR/R/NN_<prompt>.R` — one self-contained script per day, prefixed with the **zero-padded day number** (e.g. `01_part_to_whole.R`, `29_monochrome.R`). The script reads data, builds the plot, and `ggsave()`s the output. No shared helper/source files — each script stands alone.
- `YEAR/data/` — input data. **Gitignored** (the `.gitignore` `data/` rule matches these at any depth), so never assume a script's inputs are present; they may need to be re-downloaded or regenerated.
- `YEAR/plots/NN_<prompt>.png` — committed output images, referenced by the README gallery.
- `2026/ideas.txt` — the planning checklist. Each day is listed under its prompt category; `[X]` marks a completed day. Consult and update this when adding a day.
- `README.md` — a gallery embedding the committed PNGs, newest year first. Add an entry here when a new chart is finished (commits like "Update README gallery with Days …" do this).
- `plots/` (repo root) — scratch/temp output (`temp_*.png`); not the canonical location.

There is no build, test, or lint setup. The Rproj uses 2-space indent, UTF-8.

## Running a script

Each day's script is run on its own, top to bottom, from the project root:

```r
source("2026/R/29_monochrome.R")   # or run interactively in RStudio
```

Paths are resolved with `here::here(...)` or project-root-relative strings (`"2026/data/..."`), so the working directory must be the repo root. Scripts end with a `ggsave(here::here("YEAR/plots/NN_*.png"), ...)`.

**Exploratory tails:** many scripts (e.g. `01_part_to_whole.R`) keep large blocks of commented-out or superseded plotting code after the final `ggsave`. The canonical chart is the one that gets saved — don't treat the trailing experiments as live code.

## Conventions to match when adding or editing a day

- **Stack:** `tidyverse`/`ggplot2`, `patchwork` for multi-panel composition, `ggtext`/`geom_richtext` for styled labels, `scales` for axis/label formatting. Native pipe `|>`.
- **Imports:** prefer `library()` for the core stack and `import::from(pkg, fn)` for one-off functions (e.g. `import::from(sidrar, get_sidra)`).
- **`theme_sub_*()` helpers** (`theme_sub_panel`, `theme_sub_axis_x`, `theme_sub_plot`, `theme_sub_legend`, `theme_sub_strip`, …) are part of **ggplot2 4.0.0+** (this machine has 4.0.3) — no extra package needed. They are terser, organised shortcuts for `theme()`: e.g. `theme_sub_axis_bottom(title, text, line, ticks)` expands to `theme(axis.title.x.bottom, axis.text.x.bottom, axis.line.x.bottom, axis.ticks.x.bottom)`. The `...` is unused; pass arguments by name. **These require ggplot2 ≥ 4.0.0** — scripts will fail on older ggplot2.
- **Theming pattern:** define a `theme_plot` once near the top as `theme_minimal(base_family = ...) + theme_sub_*(...)`, then add it to each plot. A warm `offwhite` background (`"#f5f5dc"` / `"#f8fbf8"`) recurs across days.
- **Fonts:** custom families are referenced by name (e.g. `Lato`, `Lora`, `Roboto Slab`, `Georgia`). Some days load Google Fonts via `showtext` + `sysfonts` (`font_add_google()`, `showtext_auto()`, `showtext_opts(dpi = 300)`); fonts must be available on the system or rendering falls back.
- **Brazilian data sources:** IBGE via `sidrar::get_sidra()` (SIDRA tables/APIs), `geobr` for state/region geometries, `rbcb` for central-bank series, CEPEA Excel files, plus international sources (`maddison`, OWID CSVs under `data/`). Captions credit the source and tag `@viniciusoike`.
- **Number formatting** is frequently localized to pt-BR (`big.mark = "."`, `decimal.mark = ","`), sometimes set globally via `options(scales.big.mark = ".")`.
- **Output sizing:** `ggsave()` with explicit `width`/`height` and `dpi = 300`–`400`.

## Code style (from global user prefs)

Use RStudio section headers — `# Section ----`, `## Subsection ----` with trailing dashes filling to ~76 chars. No box-style `====` borders. Keep comments concise.
