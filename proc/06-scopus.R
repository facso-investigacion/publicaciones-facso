# =====================================================================
# 06-scopus.R  |  Etapa 6 de 6
#
# Anade dos bloques de informacion:
#   a) Atributos de la PUBLICACION, via API de Scopus (Elsevier) por DOI:
#      citas, palabras clave y resumen.
#   b) Atributos de la REVISTA, via SCImago por ISSN: SJR y cuartil.
#
# Entradas : input/temp/base-consolidada.rds (etapa 3)
#            input/temp/revistas-issn.rds    (etapa 3)
#            input/original/scimagojr.csv       (se descarga si no existe)
# Salidas  : input/temp/scopus-publicaciones.rds (doi -> citas, keywords, abstract)
#            input/temp/sjr-revista.rds          (revista_id -> sjr, cuartil_sjr)
#
# CREDENCIALES
#   La API key se obtiene en https://dev.elsevier.com/ y el token
#   institucional en apisupport@elsevier.com. NUNCA deben escribirse en el
#   codigo: se leen de las variables de entorno SCOPUS_API_KEY y
#   SCOPUS_INSTTOKEN, definidas en ~/.Renviron o en el .Renviron del
#   proyecto. Si no hay API key, esta etapa omite el bloque (a) y el
#   pipeline continua sin citas ni resumenes.
#
# CITA SCImago (uso no comercial):
#   SCImago (n.d.). SJR - SCImago Journal & Country Rank.
#   https://www.scimagojr.com
# =====================================================================


SCOPUS_API_KEY   <- Sys.getenv("SCOPUS_API_KEY")
SCOPUS_INSTTOKEN <- Sys.getenv("SCOPUS_INSTTOKEN")
if (identical(SCOPUS_INSTTOKEN, "")) SCOPUS_INSTTOKEN <- NULL


## ---------------------------------------------------------------------
## 1. API DE SCOPUS: ATRIBUTOS DE LA PUBLICACION
## ---------------------------------------------------------------------

obtener_scopus <- function(doi,
                           api_key   = SCOPUS_API_KEY,
                           insttoken = SCOPUS_INSTTOKEN) {

  vacio <- tibble(scopus_citas = NA_real_, scopus_keywords = NA_character_,
                  scopus_abstract = NA_character_)
  if (is.na(doi)) return(vacio)

  cabeceras <- c("X-ELS-APIKey" = api_key, "Accept" = "application/json")
  if (!is.null(insttoken)) cabeceras["X-ELS-Insttoken"] <- insttoken

  resp <- tryCatch(
    GET(paste0("https://api.elsevier.com/content/abstract/doi/", doi),
        add_headers(.headers = cabeceras)),
    error = function(e) NULL
  )

  if (is.null(resp) || status_code(resp) != 200) {
    if (!is.null(resp)) {
      message("    HTTP ", status_code(resp), " para DOI ", doi)
    }
    return(vacio)
  }

  datos <- tryCatch(
    content(resp, as = "text", encoding = "UTF-8") |> fromJSON(flatten = TRUE),
    error = function(e) NULL
  )
  coredata <- datos$`abstracts-retrieval-response`$coredata
  if (is.null(coredata)) return(vacio)

  # Las keywords llegan como data.frame con columna "$" o como lista plana,
  # segun cuantas tenga el registro.
  kw <- tryCatch(datos$`abstracts-retrieval-response`$authkeywords$`author-keyword`,
                 error = function(e) NULL)
  keywords <- if (is.data.frame(kw) && "$" %in% names(kw)) safe_paste(kw$`$`)
              else if (!is.null(kw)) safe_paste(kw)
              else NA_character_

  tibble(
    scopus_citas    = safe_num(coredata$`citedby-count`),
    scopus_keywords = keywords,
    scopus_abstract = safe_chr(coredata$`dc:description`)
  )
}

#' Consulta una lista de DOI con guardado incremental y reanudacion.
consultar_scopus <- function(dois, pausa_seg = 1,
                             archivo_parcial = ruta_temp("scopus-parcial.rds")) {

  acumulado <- if (file.exists(archivo_parcial)) readRDS(archivo_parcial) else tibble()
  ya_hechos <- if (nrow(acumulado) > 0) acumulado$doi else character()
  pendientes <- setdiff(dois, ya_hechos)

  if (length(pendientes) == 0) return(acumulado)
  message("  DOI pendientes en Scopus: ", length(pendientes), " de ", length(dois))

  for (i in seq_along(pendientes)) {
    doi <- pendientes[i]
    message(sprintf("  [%d/%d] DOI %s", i, length(pendientes), doi))

    fila <- tryCatch(
      bind_cols(tibble(doi = doi), obtener_scopus(doi)),
      error = function(e) {
        message("    -> error: ", conditionMessage(e))
        tibble(doi = doi)
      }
    )

    acumulado <- bind_rows(acumulado, fila)
    saveRDS(acumulado, archivo_parcial)   # checkpoint
    Sys.sleep(pausa_seg)
  }

  acumulado
}


## ---------------------------------------------------------------------
## 2. EJECUCION DEL BLOQUE POR DOI
## ---------------------------------------------------------------------

base_consolidada <- readRDS(ruta_temp("base-consolidada.rds"))

dois <- base_consolidada$doi |> na.omit() |> unique()

if (identical(SCOPUS_API_KEY, "")) {
  message("  ADVERTENCIA: SCOPUS_API_KEY no definida. Se omiten citas, ",
          "keywords y abstract de Scopus.")
  scopus_publicaciones <- tibble(doi = character(), scopus_citas = numeric(),
                                 scopus_keywords = character(),
                                 scopus_abstract = character())
} else {
  scopus_publicaciones <- usar_cache(
    ruta_temp("scopus-publicaciones-crudo.rds"),
    consultar_scopus(dois)
  ) |>
    select(doi, scopus_citas, scopus_keywords, scopus_abstract) |>
    distinct(doi, .keep_all = TRUE)
}

saveRDS(scopus_publicaciones, ruta_temp("scopus-publicaciones.rds"))


## ---------------------------------------------------------------------
## 3. SCIMAGO: SJR Y CUARTIL POR REVISTA
## ---------------------------------------------------------------------
## El ranking se publica cada junio. El archivo usa ";" como separador y
## coma decimal, y concatena los ISSN de la revista en una sola columna.

RUTA_SCIMAGO <- ruta_input("scimagojr.csv")

descargar_scimago <- function(anio = ANIO_FIN - 1, destino = RUTA_SCIMAGO) {
  message("  Descargando ranking SCImago ", anio, " ...")
  resp <- GET(
    paste0("https://www.scimagojr.com/journalrank.php?out=xls&year=", anio),
    add_headers(`User-Agent` = "Mozilla/5.0"),
    write_disk(destino, overwrite = TRUE)
  )
  if (status_code(resp) != 200) {
    stop("Fallo la descarga de SCImago (HTTP ", status_code(resp),
         "). Descargalo manualmente a ", destino)
  }
  destino
}

if (!file.exists(RUTA_SCIMAGO)) descargar_scimago()

sjr_por_issn <- read_delim(RUTA_SCIMAGO, delim = ";",
                           locale = locale(decimal_mark = ","),
                           show_col_types = FALSE) |>
  clean_names() |>
  separate_longer_delim(issn, delim = ",") |>
  mutate(issn = norm_issn(issn)) |>
  filter(!is.na(issn)) |>
  select(issn, sjr, sjr_best_quartile, h_index, title_scimago = title) |>
  distinct(issn, .keep_all = TRUE)

# Si una revista aparece con varios ISSN en SCImago, se prioriza la
# entrada con cuartil informado y, entre ellas, el mayor SJR.
sjr_revista <- readRDS(ruta_temp("revistas-issn.rds")) |>
  left_join(sjr_por_issn, by = "issn") |>
  arrange(revista_id, is.na(sjr_best_quartile), desc(sjr)) |>
  slice(1, .by = revista_id) |>
  select(revista_id, sjr, cuartil_sjr = sjr_best_quartile, h_index)

saveRDS(sjr_revista, ruta_temp("sjr-revista.rds"))


## ---------------------------------------------------------------------
## 4. VERIFICACIONES
## ---------------------------------------------------------------------

print(count(sjr_revista, cuartil_sjr))
message("  Publicaciones con datos de Scopus: ",
        sum(!is.na(scopus_publicaciones$scopus_citas)))
