rm(list = ls())

# =========================================================================
# Pipeline: SCOPUS (Elsevier) + SCImago (SJR)
# Enriquece base_final con:
#   - Por DOI (atributos de la publicacion):
#       citas, keywords, abstract
#   - Por ISSN (atributos de la revista):
#       SJR y cuartil SJR
#
# Consistente con proc-indexaciones.R y proc-idiomas.R:
#   el ISSN es el identificador de revista; id_pub identifica la
#   publicacion y permite repropagar los atributos de revista.
# =========================================================================
#
# CREDENCIALES:
#   - API key: https://dev.elsevier.com/ (Create API Key)
#   - Institutional token (opcional): apisupport@elsevier.com
#   Se recomienda NO dejar la clave en el codigo. Definela como variable
#   de entorno SCOPUS_API_KEY (p. ej. en ~/.Renviron) y el script la lee.
# =========================================================================

library(httr)
library(jsonlite)
library(tidyverse)   # dplyr, purrr, tibble, stringr, tidyr, readr
library(janitor)

# -------------------------------------------------------------------------
# 0. Credenciales (leidas desde el entorno; nunca en texto plano)
# -------------------------------------------------------------------------
SCOPUS_API_KEY   <- Sys.getenv("SCOPUS_API_KEY")
SCOPUS_INSTTOKEN <- Sys.getenv("SCOPUS_INSTTOKEN")
if (identical(SCOPUS_INSTTOKEN, "")) SCOPUS_INSTTOKEN <- NULL
if (identical(SCOPUS_API_KEY, "")) {
  stop("Define SCOPUS_API_KEY como variable de entorno antes de ejecutar.")
}

# -------------------------------------------------------------------------
# Normalizacion de ISSN (identica a los otros scripts, para cruzar bien)
# -------------------------------------------------------------------------
norm_issn <- function(x) {
  x %>%
    str_to_upper() %>%
    str_remove_all("[^0-9X]") %>%
    str_pad(width = 8, side = "left", pad = "0") %>%
    na_if("00000000")
}

# -------------------------------------------------------------------------
# Helpers para extraer valores escalares de forma segura
# -------------------------------------------------------------------------
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

# -------------------------------------------------------------------------
# 1. Abstract Retrieval API -> datos del articulo por DOI
# -------------------------------------------------------------------------
get_scopus_abstract <- function(doi, api_key = SCOPUS_API_KEY,
                                insttoken = SCOPUS_INSTTOKEN) {
  
  vacio <- tibble(
    scopus_eid       = NA_character_,
    scopus_citas     = NA_real_,
    scopus_tipo_doc  = NA_character_,
    scopus_keywords  = NA_character_,
    scopus_abstract  = NA_character_,
    scopus_issn      = NA_character_,
    scopus_revista   = NA_character_
  )
  
  if (is.na(doi) || is.null(doi)) return(vacio)
  
  url <- paste0("https://api.elsevier.com/content/abstract/doi/", doi)
  
  headers <- c("X-ELS-APIKey" = api_key, "Accept" = "application/json")
  if (!is.null(insttoken)) headers["X-ELS-Insttoken"] <- insttoken
  
  resp <- tryCatch(GET(url, add_headers(.headers = headers)),
                   error = function(e) NULL)
  
  if (is.null(resp) || status_code(resp) != 200) {
    if (!is.null(resp)) {
      message("  Scopus abstract - HTTP ", status_code(resp), " para DOI ", doi)
    }
    return(vacio)
  }
  
  d <- tryCatch(
    content(resp, as = "text", encoding = "UTF-8") |> fromJSON(flatten = TRUE),
    error = function(e) NULL
  )
  if (is.null(d)) return(vacio)
  
  core <- d$`abstracts-retrieval-response`
  if (is.null(core)) return(vacio)
  
  coredata <- core$coredata
  if (is.null(coredata)) return(vacio)
  
  kw_raw <- tryCatch(core$authkeywords$`author-keyword`, error = function(e) NULL)
  keywords <- NA_character_
  if (!is.null(kw_raw)) {
    if (is.data.frame(kw_raw) && "$" %in% names(kw_raw)) {
      keywords <- safe_paste(kw_raw$`$`)
    } else if (is.list(kw_raw) || is.character(kw_raw)) {
      keywords <- safe_paste(kw_raw)
    }
  }
  
  tibble(
    scopus_eid      = safe_chr(coredata$eid),
    scopus_citas    = safe_num(coredata$`citedby-count`),
    scopus_tipo_doc = safe_chr(coredata$subtypeDescription),
    scopus_keywords = keywords,
    scopus_abstract = safe_chr(coredata$`dc:description`),
    scopus_issn     = safe_chr(coredata$`prism:issn`),
    scopus_revista  = safe_chr(coredata$`prism:publicationName`)
  )
}

# -------------------------------------------------------------------------
# 2. Pipeline por DOI: solo atributos de la publicacion
# -------------------------------------------------------------------------
## Se eliminan las metricas de revista de esta etapa: SJR y cuartil se
## resuelven despues por ISSN contra SCImago, para no depender de que la
## API reconozca cada DOI y para mantener consistencia con los otros scripts.
run_pipeline_scopus <- function(dois, pausa_seg = 1,
                                archivo_incremental = "output/scopus_parcial.csv") {
  
  resultado <- list()
  
  for (idx in seq_along(dois)) {
    doi <- dois[idx]
    message(sprintf("[%d/%d] Procesando DOI: %s", idx, length(dois), doi))
    
    fila <- tryCatch({
      sc <- get_scopus_abstract(doi)
      bind_cols(tibble(doi = doi), sc)
    }, error = function(e) {
      message("  -> Error con ", doi, ": ", conditionMessage(e))
      tibble(doi = doi)
    })
    
    resultado[[idx]] <- fila
    
    # Guardado incremental por si la corrida se interrumpe
    acumulado <- bind_rows(resultado)
    write_csv(acumulado, archivo_incremental)
    
    Sys.sleep(pausa_seg)
  }
  
  bind_rows(resultado)
}

# =========================================================================
# 3. SCImago (SJR): descarga y carga
# =========================================================================
# Separador ";", decimal coma, columna issn con ambos ISSN concatenados.
# Columna clave: sjr_best_quartile (Q1-Q4). Se actualiza cada junio.
# Cita (uso no comercial): SCImago (n.d.). SJR - SCImago Journal & Country
# Rank. https://www.scimagojr.com
# =========================================================================

descargar_scimago <- function(anio = 2024, destino = "input/scimago_ranking.csv") {
  url <- paste0("https://www.scimagojr.com/journalrank.php?out=xls&year=", anio)
  resp <- GET(
    url,
    add_headers(`User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0 Safari/537.36"),
    write_disk(destino, overwrite = TRUE)
  )
  if (status_code(resp) == 200) {
    message("Descargado correctamente en: ", destino)
  } else {
    message("Error en la descarga. Codigo HTTP: ", status_code(resp))
  }
  destino
}

# Carga SCImago y lo deja en formato largo por ISSN normalizado, de modo
# que cruce con la misma clave que el resto del pipeline.
cargar_scimago <- function(ruta = "input/scimagojr.csv") {
  sjr <- read_delim(
    ruta, delim = ";",
    locale = locale(decimal_mark = ","),
    show_col_types = FALSE
  ) |>
    clean_names()
  
  sjr |>
    mutate(issn_split = str_split(issn, ",")) |>
    unnest(issn_split) |>
    mutate(issn = norm_issn(issn_split)) |>       # misma normalizacion global
    filter(!is.na(issn)) |>
    select(issn, sjr, sjr_best_quartile, h_index,
           categories, areas, title_scimago = title) |>
    distinct(issn, .keep_all = TRUE)
}

# =========================================================================
# 4. EJECUCION
# =========================================================================

# --- 4.1 Base sobre la que se integra todo: base_final (tras idiomas) ---
load("output/base_final.rdata")   # trae base_final con id_pub, doi, issn*

# --- 4.2 Consulta por DOI a Scopus (atributos de la publicacion) ---
dois <- base_final$doi |>
  na.omit() |>
  unique() |>
  trimws()

resultado_scopus <- run_pipeline_scopus(dois)

# Atributos de publicacion que se conservan (por DOI)
scopus_por_doi <- resultado_scopus |>
  select(doi, scopus_citas, scopus_keywords, scopus_abstract)

# --- 4.3 SJR y cuartil por ISSN (atributos de la revista) ---
sjr_long <- cargar_scimago("input/scimagojr.csv")

# Tabla larga de ISSN de la base, identica a los otros scripts
cols_issn_base <- intersect(c("issn", "issn_p", "issn_e"),
                            names(base_final))

base_issn_largo <- base_final |>
  select(id_pub, all_of(cols_issn_base)) |>
  pivot_longer(all_of(cols_issn_base), values_to = "issn") |>
  separate_rows(issn, sep = ";") |>
  mutate(issn = norm_issn(issn)) |>
  filter(!is.na(issn)) |>
  distinct(id_pub, issn)

# Resolver SJR/cuartil por ISSN y propagar a cada publicacion (id_pub).
# Si una publicacion matchea por varios ISSN, se prioriza la fila con
# cuartil informado y, entre ellas, el mejor SJR.
sjr_por_pub <- base_issn_largo |>
  left_join(sjr_long, by = "issn") |>
  group_by(id_pub) |>
  arrange(is.na(sjr_best_quartile), desc(sjr)) |>
  slice(1) |>
  ungroup() |>
  select(id_pub, sjr, cuartil_sjr = sjr_best_quartile)

# --- 4.4 Integrar todo en base_final ---
base_final <- base_final |>
  left_join(scopus_por_doi, by = "doi") |>
  left_join(sjr_por_pub, by = "id_pub")

save(base_final, file = "output/base_final.rdata")
write_csv(base_final, "output/base_final.csv")

# --- 4.5 Verificaciones ---
count(base_final, cuartil_sjr)
summary(base_final$sjr)
sum(!is.na(base_final$scopus_citas))
