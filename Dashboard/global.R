suppressPackageStartupMessages({
  library(dplyr); library(tidyr); library(stringr)
  library(leaflet); library(classInt); library(sf)
})

message("WD de la app: ", getwd())

# Carga tu script 
ruta <- "Datos/main_code.R"
if (!file.exists(ruta)) stop("No se encuentra '../Datos/main_code.R'. Revisar la estructura de carpetas.")
source(ruta, local = FALSE)

# Validaciones mínimas
if (!exists("panel")) stop("Tras source(main_code.R) no existe 'panel'. Asegúrate de asignarlo en el nivel superior.")
if (!exists("world")) stop("Tras source(main_code.R) no existe 'world'. Asegúrate de asignarlo en el nivel superior.")
req_cols <- c("country_std","iso3c","year","gini","nonnet_norm","ratio")
faltan <- setdiff(req_cols, names(panel))
if (length(faltan)) stop(paste("A 'panel' le faltan columnas:", paste(faltan, collapse=", ")))
if (!inherits(world, "sf") || !"iso3c" %in% names(world)) stop("'world' debe ser sf y contener 'iso3c'.")

message("OK: panel n=", nrow(panel), " | world n=", nrow(world))
