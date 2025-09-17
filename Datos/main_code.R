# ============================
# 1) Paquetes
# ============================

pkgs <- c("dplyr","tidyr","stringr","readr","janitor",
          "countrycode","plm","lmtest","sandwich","broom",
          "leaflet","classInt","rnaturalearth","rnaturalearthdata","sf")
to_install <- setdiff(pkgs, rownames(installed.packages()))
if (length(to_install) > 0) install.packages(to_install, quiet = TRUE)


if (!requireNamespace("here", quietly = TRUE)) install.packages("here")
library(here)

invisible(lapply(pkgs, library, character.only = TRUE))


#Rutas de entrada ---------------------------------------------------
path_nonnet <- here("Datos", "non_net_users_num.csv")
path_gini   <- here("Datos", "gini.csv")
path_pop  <- here("Datos", "pop.csv")

# URLs en crudo (raw) de GitHub para descargar automáticamente los CSV al proyecto.
url_nonnet <- "https://raw.githubusercontent.com/lherreraj/proyecto-ciencia-datos/main/Datos/non_net_users_num.csv"
url_gini   <- "https://raw.githubusercontent.com/lherreraj/proyecto-ciencia-datos/main/Datos/gini.csv"
url_pop   <- "https://raw.githubusercontent.com/lherreraj/proyecto-ciencia-datos/main/Datos/pop.csv"


# Descarga los ficheros a las rutas definidas. El modo "wb" asegura escritura binaria correcta.
if (!file.exists(path_nonnet)) {
  download.file(url_nonnet, destfile = path_nonnet, mode = "wb", quiet = TRUE)
}
if (!file.exists(path_gini)) {
  download.file(url_gini,   destfile = path_gini,   mode = "wb", quiet = TRUE)
}
if (!file.exists(path_pop)) {
  download.file(url_pop,    destfile = path_pop,    mode = "wb", quiet = TRUE)
}

# Validación temprana: detiene la ejecución si falta algún archivo.
stopifnot(file.exists(path_nonnet), file.exists(path_gini), file.exists(path_pop))

#Funciones auxiliares
#---------------------------
#Describir DF
# describe_df: genera un diccionario rápido de un data.frame con:
# - nombre de variable
# - clase (tipo de dato)
# - número de valores faltantes
# - número de valores únicos
describe_df <- function(df) {
    tibble::tibble(
      variable   = names(df),
      class      = vapply(df, function(x) paste(class(x), collapse = ","), character(1)),
      n_missing  = vapply(df, function(x) sum(is.na(x)), integer(1)),
      n_unique   = vapply(df, function(x) dplyr::n_distinct(x), integer(1))
    )
}


# Función para convertir "100k", "1.2M", "850" a numérico
# parse_abbrev_num: estandariza cifras abreviadas con sufijos k, m, b a escala numérica
# - convierte a minúsculas
# - trata valores nulos comunes
# - detecta sufijo y aplica multiplicador
# Nota: se asume que los años se quedan en ancho. Esta función se aplicará
# a columnas de años en nonnet y población antes de pivotar.
parse_abbrev_num <- function(x) {
  y <- tolower(str_squish(as.character(x)))
  y[y %in% c("", "na", "nan", "null", ".", "..")] <- NA
  
  out <- rep(NA_real_, length(y))
  
  # Detecta si termina en k, m o b
  has_suf <- !is.na(y) & str_detect(y, "[kmb]$")
  
  if (any(has_suf, na.rm = TRUE)) {
    yy <- y[has_suf]
    # Extraer número y sufijo con regex robusto
    m <- str_match(yy, "^([0-9]+(?:[\\.,][0-9]+)?)\\s*([kmb])$")
    base <- suppressWarnings(as.numeric(str_replace(m[,2], ",", ".")))
    mult <- case_when(
      m[,3] == "k" ~ 1e3,
      m[,3] == "m" ~ 1e6,
      m[,3] == "b" ~ 1e9,
      TRUE ~ 1
    )
    out[has_suf] <- base * mult
  }
  
  return(out)
}



# Importación --------------------------------------------------------
# Carga los tres datasets desde las rutas locales definidas.
# Se mantiene la estructura original de nombres de columnas sin clean_names().
nonnet_data <- read_csv(
  file = path_nonnet,
  show_col_types = FALSE,
  na = c("", "NA", "NaN", "NULL", ".", "..")
) #|> clean_names()

gini_data <- read_csv(
  file = path_gini,
  show_col_types = FALSE,
  na = c("", "NA", "NaN", "NULL", ".", "..")
) #|> clean_names()

pop_data <- read_csv(
  file = path_pop,
  show_col_types = FALSE,
  na = c("", "NA", "NaN", "NULL", ".", "..")
) #|> clean_names()

# Resumen de nonnet_data
# Diagnóstico descriptivo de columnas, clases y faltantes.
describe_df(nonnet_data)

# Resumen de gini_data
describe_df(gini_data)

# Resumen de gini_data
describe_df(pop_data)


# 1) Columnas de años: todo menos "country"
# Se asume forma ancha, con una columna "country" y el resto años.
year_cols_nonnet <- setdiff(names(nonnet_data), "country")
year_cols_gini   <- setdiff(names(gini_data),   "country")
year_cols_pop   <- setdiff(names(pop_data),   "country")

#    Convierte "100k", "1.2M", etc. a numérico
# Aplica el parser a nonnet y población para homogeneizar a valores numéricos.
nonnet_data[year_cols_nonnet] <- lapply(nonnet_data[year_cols_nonnet], parse_abbrev_num)
pop_data[year_cols_pop] <- lapply(pop_data[year_cols_pop], parse_abbrev_num)

# Intersección de años presentes en los tres datasets para garantizar alineación.
years_common <- Reduce(intersect, list(year_cols_nonnet, year_cols_gini, year_cols_pop))

# Subconjuntos alineados en forma ancha, preservando "country".
nonnet_aligned <- nonnet_data[, c("country", years_common)]
gini_aligned   <- gini_data[,   c("country", years_common)]
pop_aligned   <- pop_data[,   c("country", years_common)]


# ============================
# 3) Pasar a formato largo y unir
# ============================
# Se pivotan los tres dataframes a largo con columnas: country, year, valor.
# Esto simplifica la unión y el análisis temporal.
nn_long <- nonnet_aligned |>
  pivot_longer(-country, names_to = "year", values_to = "nonnet") |>
  mutate(year = as.integer(year))

gini_long <- gini_aligned |>
  pivot_longer(-country, names_to = "year", values_to = "gini") |>
  mutate(year = as.integer(year),
         gini = suppressWarnings(as.numeric(gini)))

pop_long <- pop_aligned |>
  pivot_longer(-country, names_to = "year", values_to = "pop") |>
  mutate(year = as.integer(year))

# Unión en panel: cada fila es un país-año con nonnet, gini y población.
panel <- nn_long |>
  inner_join(gini_long, by = c("country","year")) |>
  inner_join(pop_long,  by = c("country","year"))

# ============================
# 4) Normalización y ratio
# ============================
# Se normaliza el no uso de internet por población del país y año.
# Se crea el indicador ratio = gini / nonnet_norm para explorar la relación relativa.
panel <- panel |>
  mutate(
    # proporción de no usuarios sobre la población (0 a 1). 
    nonnet_norm = ifelse(pop > 0, nonnet / pop, NA_real_),
    ratio = ifelse(!is.na(nonnet_norm) & nonnet_norm > 0, gini / nonnet_norm, NA_real_)
  )

# Limpieza básica
# - eliminación de observaciones con NA o infinitos
panel <- panel |>
  filter(!is.na(gini), !is.na(nonnet_norm), is.finite(nonnet_norm), is.finite(gini)) |>
  mutate(
    country_std = str_to_title(trimws(country)),
    iso3c = countrycode(country_std, "country.name", "iso3c", warn = FALSE)
  ) |>
  filter(!is.na(iso3c))

# ============================
# 5) Correlaciones
# ============================
# Cálculo de correlaciones globales entre gini y nonnet_norm:
# - Pearson para relación lineal
# - Spearman para relación monótona
cor_global_p  <- cor(panel$gini, panel$nonnet_norm, use = "pairwise", method = "pearson")
cor_global_s  <- cor(panel$gini, panel$nonnet_norm, use = "pairwise", method = "spearman")

# Separamos la variación BETWEEN (entre países) y WITHIN (intra-país a lo largo del tiempo)
panel <- panel |>
  group_by(country_std) |>
  mutate(
    gini_c     = gini - mean(gini, na.rm = TRUE),
    nonnet_c   = nonnet_norm - mean(nonnet_norm, na.rm = TRUE)
  ) |>
  ungroup() |>
  group_by(year) |>
  mutate(
    gini_t     = gini - mean(gini, na.rm = TRUE),
    nonnet_t   = nonnet_norm - mean(nonnet_norm, na.rm = TRUE)
  ) |>
  ungroup()

# Correlación entre países: compara promedios por país
cor_between <- cor(
  panel |> distinct(country_std, gini_country = gini - mean(gini), nonnet_country = nonnet_norm - mean(nonnet_norm))
  |> pull(gini_country),
  panel |> distinct(country_std, gini_country = gini - mean(gini), nonnet_country = nonnet_norm - mean(nonnet_norm))
  |> pull(nonnet_country),
  use = "pairwise"
)

# Correlación within: tras demeaning a nivel país
cor_within <- cor(panel$gini_c, panel$nonnet_c, use = "pairwise")

# Reporte breve de resultados en consola
cat("Correlación global Pearson gini vs nonnet_norm:", round(cor_global_p,3), "\n")
cat("Correlación global Spearman gini vs nonnet_norm:", round(cor_global_s,3), "\n")
cat("Correlación BETWEEN países:", round(cor_between,3), "\n")
cat("Correlación WITHIN país:", round(cor_within,3), "\n")


# ============================
# 7) Mapa interactivo por año del ratio
# ============================
# Obtener shapes de países
# Descarga geometrías a nivel país 
world <- rnaturalearth::ne_countries(scale = "medium", returnclass = "sf")
world <- world |>
  mutate(iso3c = ifelse(!is.na(iso_a3_eh), iso_a3_eh, iso_a3)) |>
  select(iso3c, name, geometry)

# Datos del ratio por año
# Agrega el ratio por país-año (media por si hubiera duplicados) y filtra valores finitos.
ratio_year <- panel |>
  select(iso3c, country_std, year, ratio) |>
  group_by(iso3c, country_std, year) |>
  summarise(ratio = mean(ratio, na.rm = TRUE), .groups = "drop") |>
  filter(is.finite(ratio))

# Rango común para la paleta
# Construye paleta de colores basada en cuantiles para una distribución estable.
ratio_all <- ratio_year$ratio
pal <- colorBin("YlOrRd", domain = ratio_all, bins = classInt::classIntervals(ratio_all, n = 7, style = "quantile")$brks, na.color = "#cccccc")


# Utilidad para resolver qué columna usar según 'var'
.resolve_var <- function(var) {
  var <- tolower(var)
  if (var %in% c("gini")) return(list(col = "gini", title = "Coeficiente Gini"))
  if (var %in% c("nonnet", "nonnet_norm", "nonnet_normalizado")) {
    return(list(col = "nonnet_norm", title = "No uso de internet normalizado"))
  }
  if (var %in% c("ratio", "gini_nonnet_ratio", "gini_over_nonnet")) {
    return(list(col = "ratio", title = "Ratio: gini / nonnet_norm"))
  }
  stop("Variable no soportada. Usa 'gini', 'ratio' o 'nonnet'.")
}

# Paleta basada en cuantiles para estabilidad visual
.make_palette <- function(vec) {
  vec <- vec[is.finite(vec)]
  brks <- classInt::classIntervals(vec, n = 7, style = "quantile")$brks
  colorBin("YlOrRd", domain = vec, bins = brks, na.color = "#cccccc")
}

# Mapa por año y variable
# map_var_year: genera un mapa coroplético interactivo por año y variable elegida:
# - une datos del panel a la geometría por ISO3
# - arma paleta por cuantiles y leyenda
# - etiqueta emergente con país, año y valor formateado
map_var_year <- function(panel, world, sel_year, var = c("gini","ratio","nonnet")) {
  vinfo <- .resolve_var(var[1])
  vcol  <- vinfo$col
  vttl  <- vinfo$title
  
  df <- panel |>
    filter(year == sel_year) |>
    select(iso3c, country_std, year, !!sym(vcol)) |>
    rename(val = !!sym(vcol)) |>
    distinct()
  
  shp <- world |>
    left_join(df, by = "iso3c")
  
  pal <- .make_palette(df$val)
  
  leaflet(shp, options = leafletOptions(worldCopyJump = TRUE)) |>
    addTiles() |>
    addPolygons(
      stroke = TRUE, weight = 0.5, color = "#555555",
      fillColor = ~pal(val), fillOpacity = 0.85,
      label = ~sprintf("<b>%s</b><br>Año: %s<br>%s: %s",
                       ifelse(is.na(country_std), name, country_std),
                       sel_year, vttl,
                       ifelse(is.finite(val), format(round(val, 4), big.mark = ","), "NA")) |>
        lapply(htmltools::HTML),
      highlightOptions = highlightOptions(weight = 2, color = "#000", bringToFront = TRUE)
    ) |>
    addLegend(position = "bottomright", pal = pal, values = df$val,
              title = vttl, opacity = 0.85)
}

#Ejemplo de uso
map_var_year(panel, world, 1990, var = c("nonnet"))#c("gini","ratio","nonnet")

map_var_year(panel, world, 1990, var = c("gini"))#c("gini","ratio","nonnet")

map_var_year(panel, world, 1990, var = c("ratio"))#c("gini","ratio","nonnet")
