# ======================================================================
# 03_data_import_clean.R
# Importación, depuración y unión de:
# - non_net_users_num.csv
# - gini.csv
# Salidas:
# - data/processed/non_net_users_clean.csv
# - data/processed/gini_clean.csv
# - data/processed/combined_panel.csv
# - outputs/data_dictionary.csv
# ======================================================================

# 0) Paquetes -----------------------------------------------------------
required_pkgs <- c(
  "readr","dplyr","tidyr","stringr","janitor","lubridate","fs","here"
)

to_install <- setdiff(required_pkgs, rownames(installed.packages()))
if (length(to_install) > 0) install.packages(to_install, quiet = TRUE)

library(readr)
library(dplyr)
library(tidyr)
library(stringr)
library(janitor)
library(lubridate)
library(fs)
library(here)

# 1) Estructura de carpetas --------------------------------------------
dir_create(here("data"))
dir_create(here("data","raw"))
dir_create(here("data","processed"))
dir_create(here("scripts"))
dir_create(here("outputs"))

# 2) Rutas de entrada ---------------------------------------------------
# Si trabajas local con los adjuntos, mueve los CSV a data/raw y usa:
path_nonnet <- here("data","raw","non_net_users_num.csv")
path_gini   <- here("data","raw","gini.csv")

# Si prefieres que el script descargue desde GitHub cuando ya estén subidos:
# url_nonnet <- "https://raw.githubusercontent.com/<user>/<repo>/main/data/raw/non_net_users_num.csv"
# url_gini   <- "https://raw.githubusercontent.com/<user>/<repo>/main/data/raw/gini.csv"
# download.file(url_nonnet, destfile = path_nonnet, mode = "wb")
# download.file(url_gini,   destfile = path_gini,   mode = "wb")

stopifnot(file_exists(path_nonnet), file_exists(path_gini))

# 3) Funciones auxiliares -----------------------------------------------
clean_keys <- function(df, country_col = "country", year_col = "year") {
  df |>
    rename(country = all_of(country_col), year = all_of(year_col)) |>
    mutate(
      country = country |> as.character() |> str_squish() |> str_to_title(),
      year    = suppressWarnings(as.integer(year))
    )
}

validate_year <- function(df, min_year = 1900, max_year = year(Sys.Date()) + 1) {
  df |> filter(!is.na(year), year >= min_year, year <= max_year)
}

dedupe_by_key <- function(df, key = c("country","year")) {
  df |>
    group_by(across(all_of(key))) |>
    summarise(across(everything(), ~ {
      # Si hay duplicados, por defecto nos quedamos con el último no NA
      x <- .
      x[rev(seq_along(x))[which(!is.na(rev(x)))[1]]] %||% NA
    }), .groups = "drop")
}

describe_df <- function(df) {
  tibble(
    variable = names(df),
    class = vapply(df, \(x) paste(class(x), collapse = ","), character(1)),
    n_missing = vapply(df, \(x) sum(is.na(x)), integer(1))
  )
}

`%||%` <- function(a, b) if (!is.null(a)) a else b

# 4) Importación --------------------------------------------------------
nonnet_raw <- read_csv(
  file = path_nonnet,
  show_col_types = FALSE,
  na = c("", "NA", "NaN", "NULL", ".", "..")
) |> clean_names()

gini_raw <- read_csv(
  file = path_gini,
  show_col_types = FALSE,
  na = c("", "NA", "NaN", "NULL", ".", "..")
) |> clean_names()

# 5) Inspección rápida (opcional de consola) ---------------------------
# glimpse(nonnet_raw); glimpse(gini_raw)

# 6) Normalización de claves y tipos -----------------------------------
# Intentamos adivinar columnas de país y año comunes
# Si tus CSV ya se llaman country/year perfecto; si no, ajusta los nombres aquí.
guess_country_col <- function(df) {
  cand <- intersect(names(df), c("country","pais","location","entity","name"))
  if (length(cand) == 0) stop("No encuentro columna de país. Renombra en el CSV o ajusta la función.")
  cand[1]
}
guess_year_col <- function(df) {
  cand <- intersect(names(df), c("year","anio","ano","date","date_year","time","yr"))
  if (length(cand) == 0) stop("No encuentro columna de año. Renombra en el CSV o ajusta la función.")
  cand[1]
}

nonnet <- nonnet_raw |>
  clean_keys(country_col = guess_country_col(nonnet_raw),
             year_col    = guess_year_col(nonnet_raw)) |>
  validate_year() |>
  mutate(across(where(is.double), ~ ifelse(is.infinite(.x), NA, .x)))

gini <- gini_raw |>
  clean_keys(country_col = guess_country_col(gini_raw),
             year_col    = guess_year_col(gini_raw)) |>
  validate_year() |>
  mutate(across(where(is.double), ~ ifelse(is.infinite(.x), NA, .x)))

# 7) Renombrado mínimo y coerción de métricas --------------------------
# Intenta detectar la variable principal en cada dataset si no tiene nombre obvio
# Ajusta si sabes el nombre exacto.
detect_value_col <- function(df, prefer = c("non_net_users","non_internet_users","non_users","value","gini","gini_index","gini_coefficient")) {
  v <- intersect(names(df), prefer)
  if (length(v) > 0) return(v[1])
  # si no, toma la primera numérica que no sea year
  num_cols <- names(df)[vapply(df, is.numeric, logical(1))]
  setdiff(num_cols, "year")[1] %||% stop("No se pudo detectar columna de valor.")
}

nonnet_value_col <- detect_value_col(nonnet, prefer = c("non_net_users","non_internet_users","non_users","value"))
gini_value_col   <- detect_value_col(gini,   prefer = c("gini","gini_index","gini_coefficient","value"))

nonnet_clean <- nonnet |>
  select(country, year, !!sym(nonnet_value_col)) |>
  rename(non_net_users_num = !!sym(nonnet_value_col)) |>
  mutate(non_net_users_num = suppressWarnings(as.numeric(non_net_users_num))) |>
  dedupe_by_key()

gini_clean <- gini |>
  select(country, year, !!sym(gini_value_col)) |>
  rename(gini = !!sym(gini_value_col)) |>
  mutate(gini = suppressWarnings(as.numeric(gini))) |>
  dedupe_by_key()

# 8) Validaciones cruzadas ---------------------------------------------
# Países que no hacen match entre ambas tablas
left_only <- anti_join(nonnet_clean, gini_clean, by = c("country","year")) |>
  count(country, sort = TRUE)
right_only <- anti_join(gini_clean, nonnet_clean, by = c("country","year")) |>
  count(country, sort = TRUE)

# Puedes guardar estos reportes para revisar
write_csv(left_only,  here("outputs","_nonnet_without_gini.csv"))
write_csv(right_only, here("outputs","_gini_without_nonnet.csv"))

# 9) Unión y features básicos ------------------------------------------
combined <- nonnet_clean |>
  inner_join(gini_clean, by = c("country","year")) |>
  arrange(country, year)

# Ejemplos de features simples. Ajusta a tu caso real:
# - growth de non_net_users (variación anual)
combined <- combined |>
  group_by(country) |>
  arrange(year, .by_group = TRUE) |>
  mutate(
    non_net_users_yoy = non_net_users_num - lag(non_net_users_num),
    gini_yoy          = gini - lag(gini),
    period            = case_when(
      year < 2000 ~ "pre_2000",
      year >= 2000 & year < 2010 ~ "2000s",
      year >= 2010 & year < 2020 ~ "2010s",
      year >= 2020 ~ "2020s",
      TRUE ~ "unknown"
    )
  ) |>
  ungroup()

# 10) Diccionario de datos rápido ---------------------------------------
dict_nonnet <- describe_df(nonnet_clean) |> mutate(dataset = "non_net_users_clean")
dict_gini   <- describe_df(gini_clean)   |> mutate(dataset = "gini_clean")
dict_comb   <- describe_df(combined)     |> mutate(dataset = "combined_panel")

data_dictionary <- bind_rows(dict_nonnet, dict_gini, dict_comb)
write_csv(data_dictionary, here("outputs","data_dictionary.csv"))

# 11) Guardar salidas procesadas ---------------------------------------
write_csv(nonnet_clean, here("data","processed","non_net_users_clean.csv"))
write_csv(gini_clean,   here("data","processed","gini_clean.csv"))
write_csv(combined,     here("data","processed","combined_panel.csv"))

# 12) Mensajes finales --------------------------------------------------
cat("\nListo. Archivos procesados en data/processed/ y reportes en outputs/.\n")
cat("Registros combinados:", nrow(combined), " | Países:", n_distinct(combined$country), "\n")
