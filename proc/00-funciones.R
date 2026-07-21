# =====================================================================
# 00-funciones.R
# Configuracion comun y funciones compartidas por todo el pipeline.
#
# Este script NO produce datos: solo define rutas, paquetes, parametros
# y funciones auxiliares. Se carga una unica vez desde proc-final.R.
#
# Regla general del proyecto:
#   - input/original/ datos crudos, de SOLO LECTURA (nunca se escribe aqui)
#   - input/temp/     productos intermedios, regenerables (.rds)
#   - output/         entregables finales (.rdata)
#   - proc/           todos los scripts
# =====================================================================


## ---------------------------------------------------------------------
## 1. PAQUETES
## ---------------------------------------------------------------------
## Se centralizan aqui para que ningun script de etapa cargue paquetes
## por su cuenta y el entorno sea identico en toda la corrida.

if (!requireNamespace("pacman", quietly = TRUE)) install.packages("pacman")

pacman::p_load(
  tidyverse,   # dplyr, tidyr, purrr, stringr, readr, ggplot2
  readxl,      # lectura de .xlsx
  janitor,     # clean_names()
  httr,        # consultas HTTP a las APIs
  jsonlite     # parseo de respuestas JSON
)

# Version minima de tidyr por separate_longer_delim() (tidyr >= 1.3.0)
if (utils::packageVersion("tidyr") < "1.3.0") {
  stop("Se requiere tidyr >= 1.3.0. Ejecuta install.packages('tidyr').")
}


## ---------------------------------------------------------------------
## 2. RUTAS
## ---------------------------------------------------------------------

## Todas las rutas son relativas a la raiz del proyecto: hay que abrir el
## .Rproj (o fijar setwd() en la raiz) antes de ejecutar.

RUTAS <- list(
  input  = file.path("input", "original"),
  temp   = file.path("input", "temp"),
  output = "output"
)

invisible(lapply(RUTAS, dir.create, showWarnings = FALSE, recursive = TRUE))

ruta_input  <- function(...) file.path(RUTAS$input,  ...)
ruta_temp   <- function(...) file.path(RUTAS$temp,   ...)
ruta_output <- function(...) file.path(RUTAS$output, ...)


## ---------------------------------------------------------------------
## 3. PARAMETROS DE LA CORRIDA
## ---------------------------------------------------------------------
## Se definen con `if (!exists(...))` para que proc-final.R pueda fijarlos
## antes de cargar este archivo y sobreescribir los valores por defecto.

# FORZAR_API = TRUE vuelve a consultar ORCID, Crossref, OpenAlex, SciELO y
# Scopus aunque exista cache en input/temp/. Con FALSE (default) el pipeline
# reutiliza las descargas previas, lo que permite re-ejecutarlo en minutos.
if (!exists("FORZAR_API")) FORZAR_API <- FALSE

# Ventana temporal de las publicaciones consideradas.
if (!exists("ANIO_INICIO")) ANIO_INICIO <- 2020
if (!exists("ANIO_FIN"))    ANIO_FIN    <- 2025

# Jerarquias academicas incluidas en el analisis.
JERARQUIAS_VALIDAS <- c("Titular", "Asociado", "Asistente",
                        "Adjunto", "Instructor")

# Tope de horas semanales de una jornada completa (para consolidar contratos).
JORNADA_COMPLETA <- 44


## ---------------------------------------------------------------------
## 4. CACHE DE LLAMADAS A APIs
## ---------------------------------------------------------------------

#' Devuelve el objeto guardado en `ruta` o evalua `expr` y lo guarda.
#'
#' `expr` se evalua de forma perezosa: si el cache existe y `forzar` es
#' FALSE, la consulta a la API nunca llega a ejecutarse.
usar_cache <- function(ruta, expr, forzar = FORZAR_API) {
  if (!forzar && file.exists(ruta)) {
    message("  [cache] ", ruta)
    return(readRDS(ruta))
  }
  valor <- expr                       # aqui recien se evalua la promesa
  saveRDS(valor, ruta)
  message("  [guardado] ", ruta)
  valor
}


## ---------------------------------------------------------------------
## 5. NORMALIZACION DE IDENTIFICADORES
## ---------------------------------------------------------------------
## Todo cruce entre bases usa estas funciones. Es la unica garantia de que
## un mismo ISSN, DOI o RUT se escriba igual en todas las etapas.

#' ISSN a 8 caracteres, solo digitos y X, sin guiones.
norm_issn <- function(x) {
  x |>
    as.character() |>
    str_to_upper() |>
    str_remove_all("[^0-9X]") |>
    str_pad(width = 8, side = "left", pad = "0") |>
    na_if("00000000")
}

#' DOI en minuscula y sin prefijo de resolucion (https://doi.org/, doi:).
norm_doi <- function(x) {
  x |>
    as.character() |>
    str_trim() |>
    str_to_lower() |>
    str_remove("^https?://(dx\\.)?doi\\.org/") |>
    str_remove("^doi:\\s*") |>
    str_squish() |>
    na_if("")
}

#' RUT a 10 caracteres, sin puntos ni guion, con digito verificador.
norm_rut <- function(x) {
  x |>
    as.character() |>
    str_remove_all("[^0-9kK]") |>
    str_to_upper() |>
    str_pad(width = 10, side = "left", pad = "0") |>
    na_if("0000000000")
}

#' Texto libre (titulos, nombres de revista) reducido a una clave estable.
norm_texto <- function(x) {
  x |>
    as.character() |>
    str_squish() |>
    str_to_upper() |>
    na_if("") |>
    na_if("NA") |>
    na_if("NULL")
}

#' Clave de publicacion: DOI cuando existe, titulo normalizado si no.
#' Permite deduplicar libros y capitulos, que suelen venir sin DOI.
clave_publicacion <- function(doi, titulo) {
  coalesce(norm_doi(doi), paste0("TIT:", norm_texto(titulo)))
}


## ---------------------------------------------------------------------
## 6. EXTRACCION SEGURA DE VALORES DESDE JSON
## ---------------------------------------------------------------------
## Las APIs devuelven campos que pueden venir como NULL, vector o lista
## anidada. Estas funciones garantizan siempre un escalar del tipo pedido,
## lo que evita el error "Can't combine <character> and <list>" al unir
## resultados de distintos registros.

safe_chr <- function(x) {
  if (is.null(x)) return(NA_character_)
  if (is.list(x)) x <- unlist(x, use.names = FALSE)
  if (length(x) == 0) return(NA_character_)
  as.character(x[[1]])
}

safe_num <- function(x) {
  if (is.null(x)) return(NA_real_)
  if (is.list(x)) x <- unlist(x, use.names = FALSE)
  if (length(x) == 0) return(NA_real_)
  suppressWarnings(as.numeric(x[[1]]))
}

safe_paste <- function(x, sep = "; ") {
  if (is.null(x)) return(NA_character_)
  if (is.list(x)) x <- unlist(x, use.names = FALSE)
  if (length(x) == 0) return(NA_character_)
  paste(as.character(x), collapse = sep)
}

# Operador de respaldo (base R >= 4.4 y rlang ya lo exportan).
if (!exists("%||%")) {
  `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
}


## ---------------------------------------------------------------------
## 7. RECODIFICACIONES DEL DOMINIO
## ---------------------------------------------------------------------
## Definidas una sola vez porque se aplican tanto a la base SEPAVID como
## a la base ORCID. Duplicarlas era la principal fuente de divergencia
## entre ambas ramas del pipeline.

#' Reparticion administrativa -> departamento academico.
recodificar_departamento <- function(reparticion) {
  r <- str_to_lower(reparticion)
  case_when(
    str_detect(r, "psicolog")       ~ "Psicología",
    str_detect(r, "sociolog")       ~ "Sociología",
    str_detect(r, "antropolog")     ~ "Antropología",
    str_detect(r, "trabajo social") ~ "Trabajo social",
    str_detect(r, "educaci")        ~ "Educación",
    str_detect(r, "postgrado")      ~ "Postgrado",
    .default = NA_character_
  )
}

#' Cargo contractual -> jerarquia academica.
#' Ayudantes y personas "en evaluacion" quedan como NA y se excluyen
#' despues via JERARQUIAS_VALIDAS.
recodificar_jerarquia <- function(cargo) {
  x <- str_to_lower(cargo)
  case_when(
    str_detect(x, "titular")      ~ "Titular",
    str_detect(x, "asociado")     ~ "Asociado",
    str_detect(x, "asistente")    ~ "Asistente",
    str_detect(x, "postdoctoral") ~ "Investigador Postdoctoral",
    str_detect(x, "adjunto")      ~ "Adjunto",
    str_detect(x, "instructor")   ~ "Instructor",
    .default = NA_character_
  )
}

#' Homogeneiza los tipos documentales de SEPAVID y ORCID a un vocabulario
#' unico: book / book-chapter / journal-article. Cualquier otro tipo
#' queda como NA y se descarta.
recodificar_tipo_doc <- function(x) {
  y <- str_to_lower(str_squish(x))
  case_when(
    str_detect(y, "^(libro|book)$")           ~ "book",
    str_detect(y, "cap[ií]tulo|book-chapter") ~ "book-chapter",
    str_detect(y, "art[ií]culo|journal-article") ~ "journal-article",
    .default = NA_character_
  )
}

#' Limpia un apellido: vacios, "NA"/"NULL"/"s/n" y cadenas de solo
#' puntuacion pasan a NA real.
limpiar_apellido <- function(x) {
  x <- str_trim(as.character(x)) |> na_if("")
  x <- if_else(
    str_detect(x, regex("^(na|null|n/a|s/n|sin dato)$", ignore_case = TRUE)),
    NA_character_, x
  )
  if_else(str_detect(x, "^[[:punct:][:space:]]*$"), NA_character_, x)
}


## ---------------------------------------------------------------------
## 8. CONSOLIDACION DE LA PLANTA ACADEMICA
## ---------------------------------------------------------------------

#' Colapsa a un registro por RUT.
#'
#' Un academico puede aparecer en varias lineas (distintas reparticiones
#' o contratos). Criterio:
#'   - Si las horas suman <= JORNADA_COMPLETA, son fracciones de un mismo
#'     cargo: se conserva la primera linea con la suma de horas.
#'   - Si superan la jornada, son contratos distintos: se conserva el de
#'     mayor carga horaria.
consolidar_horas <- function(datos) {
  datos <- datos |>
    mutate(horas_reales = suppressWarnings(as.numeric(horas_reales))) |>
    group_by(rut) |>
    mutate(suma_horas = sum(horas_reales, na.rm = TRUE)) |>
    ungroup()

  casos_suma <- datos |>
    filter(suma_horas <= JORNADA_COMPLETA) |>
    group_by(rut) |>
    slice(1) |>
    mutate(horas_reales = suma_horas) |>
    ungroup()

  casos_max <- datos |>
    filter(suma_horas > JORNADA_COMPLETA) |>
    group_by(rut) |>
    slice_max(horas_reales, n = 1, with_ties = FALSE) |>
    ungroup()

  bind_rows(casos_suma, casos_max) |>
    select(-suma_horas)
}


## ---------------------------------------------------------------------
## 9. UTILIDADES PARA CATALOGOS DE REVISTAS
## ---------------------------------------------------------------------

#' Detecta las columnas de ISSN de un catalogo (issn, eissn, e_issn, ...).
#' Evita depender del nombre exacto que produzca clean_names() en cada
#' archivo fuente.
detectar_cols_issn <- function(datos) {
  sel <- names(datos)[str_detect(str_to_lower(names(datos)),
                                 "^(e[-_]?)?issn$")]
  if (length(sel) == 0) {
    stop("No se encontraron columnas de ISSN. Columnas disponibles: ",
         paste(names(datos), collapse = ", "))
  }
  sel
}

#' Vector de ISSN normalizados y unicos presentes en un catalogo.
issn_de_catalogo <- function(datos) {
  datos |>
    select(all_of(detectar_cols_issn(datos))) |>
    mutate(across(everything(), as.character)) |>
    pivot_longer(everything(), values_to = "issn") |>
    mutate(issn = norm_issn(issn)) |>
    filter(!is.na(issn)) |>
    distinct(issn) |>
    pull(issn)
}

#' Pasa un catalogo a formato largo (issn, <atributo>) conservando una
#' columna de atributo ya calculada.
catalogo_a_largo <- function(datos, columna_atributo) {
  cols_issn <- detectar_cols_issn(datos)
  datos |>
    select(all_of(c(columna_atributo, cols_issn))) |>
    mutate(across(all_of(cols_issn), as.character)) |>
    pivot_longer(all_of(cols_issn), values_to = "issn") |>
    mutate(issn = norm_issn(issn)) |>
    filter(!is.na(issn), !is.na(.data[[columna_atributo]])) |>
    distinct(issn, .data[[columna_atributo]])
}

#' Falla temprano y con un mensaje claro si un insumo no trae las
#' columnas que el script espera.
verificar_columnas <- function(datos, columnas, nombre_fuente) {
  faltan <- setdiff(columnas, names(datos))
  if (length(faltan) > 0) {
    stop("En '", nombre_fuente, "' faltan las columnas: ",
         paste(faltan, collapse = ", "),
         "\nColumnas disponibles: ", paste(names(datos), collapse = ", "))
  }
  invisible(TRUE)
}

#' Lee un objeto especifico desde un .rdata sin contaminar el entorno
#' global. Evita el efecto colateral clasico de load(), que crea objetos
#' con nombres que el script no controla.
leer_rdata <- function(ruta, objeto) {
  entorno <- new.env()
  load(ruta, envir = entorno)
  if (!objeto %in% ls(entorno)) {
    stop("El archivo '", ruta, "' no contiene el objeto '", objeto,
         "'. Contiene: ", paste(ls(entorno), collapse = ", "))
  }
  get(objeto, envir = entorno)
}

#' Verifica que los archivos de entrada existan antes de empezar.
verificar_archivos <- function(rutas) {
  faltan <- rutas[!file.exists(rutas)]
  if (length(faltan) > 0) {
    stop("Faltan archivos de entrada:\n  ", paste(faltan, collapse = "\n  "))
  }
  invisible(TRUE)
}
