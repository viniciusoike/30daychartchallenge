library(httr)
library(dplyr)
library(stringr)
import::from(here, here)

safe_download <- purrr::safely(download.file)

outdir <- here("2026", "data", "fenabrave")
fs::dir_create(outdir)

url_exists <- function(url) {
  tryCatch(
    {
      resp <- httr::HEAD(url)
      httr::status_code(resp) == 200L
    },
    error = function(e) FALSE
  )
}

download_fenabrave <- function() {
  ano <- 2018:2026
  mes <- 1:12
  mes <- str_pad(mes, 2, pad = "0")
  ano_mes <- str_c(rep(ano, each = 12), "_", rep(mes, length(ano)))

  res <- vector("list", length(ano_mes))
  names(res) <- ano_mes

  cli::cli_progress_bar("Downloading FENABRAVE files", total = length(ano_mes))

  for (i in seq_along(ano_mes)) {
    am <- ano_mes[[i]]
    destfile <- fs::path(outdir, str_glue("{am}.pdf"))

    cli::cli_progress_update()

    if (file.exists(destfile)) {
      cli::cli_alert_info("Already exists: {.path {destfile}}")
      next
    }

    urls <- c(
      str_glue("http://www.fenabrave.org.br/portal/files/{am}_02.pdf"),
      str_glue("http://www.fenabrave.org.br/portal/files/{am}_2.pdf"),
      str_glue("http://www.fenabrave.org.br/portal/files/2_{am}_2.pdf")
    )

    found <- FALSE
    for (url in urls) {
      if (url_exists(url)) {
        cli::cli_alert_info("Downloading {.url {url}}")
        res[[i]] <- safe_download(
          url,
          destfile = destfile,
          mode = "wb",
          quiet = TRUE
        )
        found <- TRUE
        break
      }
    }

    if (!found) {
      cli::cli_warn("No valid URL found for {am} — tried all 3 patterns")
    }

    Sys.sleep(3 + runif(1, 0, 5))
  }

  cli::cli_progress_done()
  return(invisible(res))
}

# download_fenabrave()

# OBS: very complex PDF. Managed extraction with Preview (mac) + Claude

library(tibble)
library(dplyr)
# fmt: tabular
df_2018 <- tribble(
  ~segmento       , ~jan_2018_A , ~dez_2017_B , ~acum_2018_C , ~jan_2017_D , ~acum_2017_E ,
  "A) Autos"      ,      148898 ,      171978 ,       148898 ,      121378 ,       121378 ,
  "B) Com. Leves" ,       26656 ,       32865 ,        26656 ,       22175 ,        22175 ,
  "A + B"         ,      175554 ,      204843 ,       175554 ,      143553 ,       143553 ,
  "C) Caminhões"  ,        4594 ,        6174 ,         4594 ,        2940 ,         2940 ,
  "D) Ônibus"     ,        1115 ,        1603 ,         1115 ,         707 ,          707 ,
  "C + D"         ,        5709 ,        7777 ,         5709 ,        3647 ,         3647 ,
  "Subtotal"      ,      181263 ,      212620 ,       181263 ,      147200 ,       147200 ,
  "E) Motos"      ,       77031 ,       77458 ,        77031 ,       67601 ,        67601 ,
  "F) Impl. Rod." ,        2526 ,        2546 ,         2526 ,        1528 ,         1528 ,
  "Outros"        ,        8272 ,        8629 ,         8272 ,        7809 ,         7809
)
# fmt: tabular
df_2019 <- tribble(
  ~segmento       , ~jan_2019_A , ~dez_2018_B , ~acum_2019_C , ~jan_2018_D , ~acum_2018_E ,
  "A) Autos"      ,      163796 ,      189389 ,       163796 ,      149041 ,       149041 ,
  "B) Com. Leves" ,       26956 ,       35588 ,        26956 ,       26496 ,        26496 ,
  "A + B"         ,      190752 ,      224977 ,       190752 ,      175537 ,       175537 ,
  "C) Caminhões"  ,        6932 ,        7603 ,         6932 ,        4593 ,         4593 ,
  "D) Ônibus"     ,        2113 ,        1949 ,         2113 ,        1115 ,         1115 ,
  "C + D"         ,        9045 ,        9552 ,         9045 ,        5708 ,         5708 ,
  "Subtotal"      ,      199797 ,      234529 ,       199797 ,      181245 ,       181245 ,
  "E) Motos"      ,       90722 ,       84067 ,        90722 ,       77021 ,        77021 ,
  "F) Impl. Rod." ,        4391 ,        4102 ,         4391 ,        2391 ,         2391 ,
  "Outros"        ,        8409 ,        8426 ,         8409 ,        8418 ,         8418 ,
  "Total"         ,      303319 ,      331124 ,       303319 ,      269075 ,       269075
)
# fmt: tabular
df_2020 <- tribble(
  ~segmento       , ~jan_2020_A , ~dez_2019_B , ~acum_2020_C , ~jan_2019_D , ~acum_2019_E ,
  "A) Autos"      ,      154572 ,      215171 ,       154572 ,      163779 ,       163779 ,
  "B) Com. Leves" ,       29553 ,       36624 ,        29553 ,       26861 ,        26861 ,
  "A + B"         ,      184125 ,      251795 ,       184125 ,      190640 ,       190640 ,
  "C) Caminhões"  ,        7186 ,        8328 ,         7186 ,        6932 ,         6932 ,
  "D) Ônibus"     ,        2153 ,        2434 ,         2153 ,        2203 ,         2203 ,
  "C + D"         ,        9339 ,       10762 ,         9339 ,        9135 ,         9135 ,
  "Subtotal"      ,      193464 ,      262557 ,       193464 ,      199775 ,       199775 ,
  "E) Motos"      ,       91689 ,       94108 ,        91689 ,       90713 ,        90713 ,
  "F) Impl. Rod." ,        4632 ,        4987 ,         4632 ,        4390 ,         4390 ,
  "Outros"        ,        8632 ,        9124 ,         8632 ,        8417 ,         8417
)
# fmt: tabular
df_2021 <- tribble(
  ~segmento       , ~jan_2021_A , ~dez_2020_B , ~acum_2021_C , ~jan_2020_D , ~acum_2020_E ,
  "A) Autos"      ,      130800 ,      194668 ,       130800 ,      154556 ,       154556 ,
  "B) Com. Leves" ,       31767 ,       38127 ,        31767 ,       29556 ,        29556 ,
  "A + B"         ,      162567 ,      232795 ,       162567 ,      184112 ,       184112 ,
  "C) Caminhões"  ,        7262 ,        9639 ,         7262 ,        7181 ,         7181 ,
  "D) Ônibus"     ,        1324 ,        1551 ,         1324 ,        2158 ,         2158 ,
  "C + D"         ,        8586 ,       11190 ,         8586 ,        9339 ,         9339 ,
  "Subtotal"      ,      171153 ,      243985 ,       171153 ,      193451 ,       193451 ,
  "E) Motos"      ,       85839 ,       98829 ,        85839 ,       91691 ,        91691 ,
  "F) Impl. Rod." ,        6726 ,        7354 ,         6726 ,        4645 ,         4645 ,
  "Outros"        ,       10375 ,       12974 ,        10375 ,        8672 ,         8672
)
# fmt: tabular
df_2022 <- tribble(
  ~segmento       , ~jan_2022_A , ~dez_2021_B , ~acum_2022_C , ~jan_2021_D , ~acum_2021_E ,
  "A) Autos"      ,       92212 ,      156132 ,        92212 ,      130778 ,       130778 ,
  "B) Com. Leves" ,       24389 ,       37409 ,        24389 ,       31753 ,        31753 ,
  "A + B"         ,      116601 ,      193541 ,       116601 ,      162531 ,       162531 ,
  "C) Caminhões"  ,        8517 ,       11985 ,         8517 ,        7259 ,         7259 ,
  "D) Ônibus"     ,        1368 ,        1536 ,         1368 ,        1324 ,         1324 ,
  "C + D"         ,        9885 ,       13521 ,         9885 ,        8583 ,         8583 ,
  "Subtotal"      ,      126486 ,      207062 ,       126486 ,      171114 ,       171114 ,
  "E) Motos"      ,       89685 ,      112401 ,        89685 ,       85832 ,        85832 ,
  "F) Impl. Rod." ,        6724 ,        8154 ,         6724 ,        6727 ,         6727 ,
  "Outros"        ,        7888 ,        9987 ,         7888 ,       10374 ,        10374
)
# fmt: tabular
df_2023 <- tribble(
  ~segmento       , ~jan_2023_A , ~dez_2022_B , ~acum_2023_C , ~jan_2022_D , ~acum_2022_E ,
  "A) Autos"      ,      103872 ,      164160 ,       103872 ,       92204 ,        92204 ,
  "B) Com. Leves" ,       26588 ,       38005 ,        26588 ,       24422 ,        24422 ,
  "A + B"         ,      130460 ,      202165 ,       130460 ,      116626 ,       116626 ,
  "C) Caminhões"  ,       10215 ,       12064 ,        10215 ,        8507 ,         8507 ,
  "D) Ônibus"     ,        2152 ,        2680 ,         2152 ,        1334 ,         1334 ,
  "C + D"         ,       12367 ,       14744 ,        12367 ,        9841 ,         9841 ,
  "Subtotal"      ,      142827 ,      216909 ,       142827 ,      126467 ,       126467 ,
  "E) Motos"      ,      110493 ,      132128 ,       110493 ,       89665 ,        89665 ,
  "F) Impl. Rod." ,        6539 ,        7891 ,         6539 ,        6725 ,         6725 ,
  "Outros"        ,        8522 ,        9955 ,         8522 ,        7888 ,         7888
)
# fmt: tabular
df_2024 <- tribble(
  ~segmento       , ~jan_2024_A , ~dez_2023_B , ~acum_2024_C , ~jan_2023_D , ~acum_2023_E ,
  "A) Autos"      ,      118507 ,      187746 ,       118507 ,      103858 ,       103858 ,
  "B) Com. Leves" ,       33555 ,       48844 ,        33555 ,       26580 ,        26580 ,
  "A + B"         ,      152062 ,      236590 ,       152062 ,      130438 ,       130438 ,
  "C) Caminhões"  ,        7993 ,       10068 ,         7993 ,       10209 ,        10209 ,
  "D) Ônibus"     ,        1546 ,        1877 ,         1546 ,        2153 ,         2153 ,
  "C + D"         ,        9539 ,       11945 ,         9539 ,       12362 ,        12362 ,
  "Subtotal"      ,      161601 ,      248535 ,       161601 ,      142800 ,       142800 ,
  "E) Motos"      ,      143327 ,      132752 ,       143327 ,      110541 ,       110541 ,
  "F) Impl. Rod." ,        7072 ,        7874 ,         7072 ,        6540 ,         6540 ,
  "Outros"        ,       10505 ,       10850 ,        10505 ,        8524 ,         8524
)
# fmt: tabular
df_2025 <- tribble(
  ~segmento       , ~jan_2025_A , ~dez_2024_B , ~acum_2025_C , ~jan_2024_D , ~acum_2024_E ,
  "A) Autos"      ,      123359 ,      189839 ,       123359 ,      118490 ,       118490 ,
  "B) Com. Leves" ,       36487 ,       53824 ,        36487 ,       33555 ,        33555 ,
  "A + B"         ,      159846 ,      243663 ,       159846 ,      152045 ,       152045 ,
  "C) Caminhões"  ,        9170 ,       11159 ,         9170 ,        7991 ,         7991 ,
  "D) Ônibus"     ,        2191 ,        2594 ,         2191 ,        1546 ,         1546 ,
  "C + D"         ,       11361 ,       13753 ,        11361 ,        9537 ,         9537 ,
  "Subtotal"      ,      171207 ,      257416 ,       171207 ,      161582 ,       161582 ,
  "E) Motos"      ,      151954 ,      151921 ,       151954 ,      143328 ,       143328 ,
  "F) Impl. Rod." ,        6203 ,        6820 ,         6203 ,        7081 ,         7081 ,
  "Outros"        ,       12052 ,       12226 ,        12052 ,       10502 ,        10502
)
# fmt: tabular
df_2026 <- tribble(
  ~segmento       , ~jan_2026_A , ~dez_2025_B , ~acum_2026_C , ~jan_2025_D , ~acum_2025_E ,
  "A) Autos"      ,      125136 ,      210714 ,       125136 ,      123344 ,       123344 ,
  "B) Com. Leves" ,       37348 ,       56380 ,        37348 ,       36518 ,        36518 ,
  "A + B"         ,      162484 ,      267094 ,       162484 ,      159862 ,       159862 ,
  "C) Caminhões"  ,        6379 ,        9765 ,         6379 ,        9131 ,         9131 ,
  "D) Ônibus"     ,        1675 ,        2549 ,         1675 ,        2191 ,         2191 ,
  "C + D"         ,        8054 ,       12314 ,         8054 ,       11322 ,        11322 ,
  "Subtotal"      ,      170538 ,      279408 ,       170538 ,      171184 ,       171184 ,
  "E) Motos"      ,      178537 ,      193168 ,       178537 ,      151955 ,       151955 ,
  "F) Impl. Rod." ,        4344 ,        5416 ,         4344 ,        6203 ,         6203 ,
  "Outros"        ,       13294 ,       14481 ,        13294 ,       12052 ,        12052
)

# Combined long tibble — January figures only, 2018–2026
df_combined <- bind_rows(
  df_2018 |> select(segmento, jan = jan_2018_A) |> mutate(year = 2018),
  df_2019 |> select(segmento, jan = jan_2019_A) |> mutate(year = 2019),
  df_2020 |> select(segmento, jan = jan_2020_A) |> mutate(year = 2020),
  df_2021 |> select(segmento, jan = jan_2021_A) |> mutate(year = 2021),
  df_2022 |> select(segmento, jan = jan_2022_A) |> mutate(year = 2022),
  df_2023 |> select(segmento, jan = jan_2023_A) |> mutate(year = 2023),
  df_2024 |> select(segmento, jan = jan_2024_A) |> mutate(year = 2024),
  df_2025 |> select(segmento, jan = jan_2025_A) |> mutate(year = 2025),
  df_2026 |> select(segmento, jan = jan_2026_A) |> mutate(year = 2026)
)

# Atomic segments only (no aggregates)
segmentos_atomicos <- c(
  "A) Autos",
  "B) Com. Leves",
  "C) Caminhões",
  "D) Ônibus",
  "E) Motos",
  "F) Impl. Rod.",
  "Outros"
)

df_atomicos <- df_combined |>
  filter(segmento %in% segmentos_atomicos)

# readr::write_csv(df_atomicos, "2026/data/fenabrave/tabelas_janeiro_2018_2026.csv")

library(readxl)
import::from(here, here)
datadir <- here("2026", "data")

sheets <- excel_sheets(here(datadir, "motorizacao", "tabela.xlsx"))

read_excel(here(datadir, "motorizacao", "tabela.xlsx"), sheet = sheets[2])
