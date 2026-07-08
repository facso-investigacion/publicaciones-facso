# =========================================================
# Pipeline: ORCID -> Crossref / OpenAlex
# Extrae DOIs de un autor desde ORCID y enriquece cada
# publicacion con datos de revista e indexacion.
# =========================================================

# Paquetes necesarios
# install.packages(c("httr", "jsonlite", "dplyr", "purrr"))

library(httr)
library(jsonlite)
library(dplyr)
library(purrr)
library(readxl)
library(janitor)
library(stringr)

# -----------------------------------------------------------
# 1. Obtener las publicaciones (works) de un autor en ORCID
# -----------------------------------------------------------
get_orcid_works <- function(orcid_id) {
  url <- paste0("https://pub.orcid.org/v3.0/", orcid_id, "/works")
  
  resp <- GET(url, add_headers(Accept = "application/json"))
  stop_for_status(resp)
  
  data <- content(resp, as = "text", encoding = "UTF-8") |> fromJSON(flatten = TRUE)
  groups <- data$group
  
  # Autor sin publicaciones registradas
  if (is.null(groups) || (is.data.frame(groups) && nrow(groups) == 0) || length(groups) == 0) {
    return(tibble(
      titulo = character(), tipo = character(), anio = character(),
      put_code = character(), doi = character()
    ))
  }
  
  # IMPORTANTE: groups es un data.frame (por flatten = TRUE de fromJSON).
  # map_dfr(groups, ...) iteraria sobre COLUMNAS, no filas -> hay que
  # iterar explicitamente por indice de fila.
  n_groups <- if (is.data.frame(groups)) nrow(groups) else length(groups)
  
  works <- map_dfr(seq_len(n_groups), function(i) {
    # Cada grupo puede tener varias fuentes reportando la misma obra;
    # tomamos el primer summary (primera fuente) del grupo.
    summ_list <- groups$`work-summary`[[i]]
    summ <- if (is.data.frame(summ_list)) summ_list[1, ] else summ_list[[1]]
    
    doi <- NA_character_
    ext_col <- "external-ids.external-id"
    if (ext_col %in% names(summ)) {
      ext <- summ[[ext_col]][[1]]
      if (is.data.frame(ext) && "external-id-type" %in% names(ext)) {
        doi_row <- ext[ext$`external-id-type` == "doi", ]
        if (nrow(doi_row) > 0) doi <- doi_row$`external-id-value`[1]
      }
    }
    
    get_val <- function(campo) {
      if (campo %in% names(summ)) safe_chr(summ[[campo]]) else NA_character_
    }
    
    tibble(
      titulo   = get_val("title.title.value"),
      tipo     = get_val("type"),
      anio     = get_val("publication-date.year.value"),
      put_code = get_val("put-code"),
      doi      = doi
    )
  })
  
  works
}

# Helpers para extraer siempre un valor escalar, sin importar si el
# campo del JSON viene como NULL, vector, o lista anidada.
# Esto evita el error "Can't combine ... <character> and ... <list>"
# al juntar resultados de distintos DOIs.
safe_chr <- function(x) {
  if (is.null(x)) return(NA_character_)
  if (is.list(x)) x <- unlist(x, use.names = FALSE)
  if (length(x) == 0) return(NA_character_)
  as.character(x[[1]])
}

safe_num <- function(x) {
  if (is.null(x)) return(NA_integer_)
  if (is.list(x)) x <- unlist(x, use.names = FALSE)
  if (length(x) == 0) return(NA_integer_)
  suppressWarnings(as.integer(x[[1]]))
}

# -----------------------------------------------------------
# 2. Enriquecer un DOI con datos de Crossref
# -----------------------------------------------------------
get_crossref_metadata <- function(doi) {
  vacio <- tibble(revista = NA_character_, issn = NA_character_, editorial = NA_character_)
  
  if (is.na(doi)) return(vacio)
  
  url <- paste0("https://api.crossref.org/works/", doi)
  resp <- tryCatch(GET(url), error = function(e) NULL)
  
  if (is.null(resp) || status_code(resp) != 200) return(vacio)
  
  msg <- tryCatch(
    content(resp, as = "text", encoding = "UTF-8") |> fromJSON(flatten = TRUE),
    error = function(e) NULL
  )
  if (is.null(msg) || is.null(msg$message)) return(vacio)
  msg <- msg$message
  
  issn_val <- msg$ISSN
  issn_txt <- if (is.null(issn_val) || length(issn_val) == 0) {
    NA_character_
  } else {
    paste(unlist(issn_val, use.names = FALSE), collapse = "; ")
  }
  
  tibble(
    revista   = safe_chr(msg$`container-title`),
    issn      = issn_txt,
    editorial = safe_chr(msg$publisher)
  )
}

# -----------------------------------------------------------
# 3. Enriquecer un DOI con datos de OpenAlex (indexacion, OA)
# -----------------------------------------------------------
get_openalex_metadata <- function(doi) {
  vacio <- tibble(oa_status = NA_character_, citas = NA_integer_, revista_openalex = NA_character_)
  
  if (is.na(doi)) return(vacio)
  
  url <- paste0("https://api.openalex.org/works/https://doi.org/", doi)
  resp <- tryCatch(GET(url), error = function(e) NULL)
  
  if (is.null(resp) || status_code(resp) != 200) return(vacio)
  
  d <- tryCatch(
    content(resp, as = "text", encoding = "UTF-8") |> fromJSON(flatten = TRUE),
    error = function(e) NULL
  )
  if (is.null(d)) return(vacio)
  
  tibble(
    oa_status        = safe_chr(d$open_access.oa_status),
    citas            = safe_num(d$cited_by_count),
    revista_openalex = safe_chr(d$primary_location.source.display_name)
  )
}



# Helper tipo %||% (por si no está disponible)
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# -----------------------------------------------------------
# 4. Pipeline completo
# -----------------------------------------------------------
run_pipeline <- function(orcid_id) {
  works <- get_orcid_works(orcid_id)
  
  message("Publicaciones encontradas: ", nrow(works))
  
  enriched <- works |>
    mutate(
      crossref = map(doi, get_crossref_metadata),
      openalex = map(doi, get_openalex_metadata)
    ) |>
    tidyr::unnest(crossref) |>
    tidyr::unnest(openalex)
  
  enriched
}

# -----------------------------------------------------------
# 5. Pipeline para una LISTA de autores
# -----------------------------------------------------------
run_pipeline_multi <- function(orcid_ids, pausa_seg = 1,
                               archivo_incremental = "publicaciones_orcid_parcial.csv") {
  resultado <- list()
  
  for (idx in seq_along(orcid_ids)) {
    id <- orcid_ids[idx]
    message(sprintf("[%d/%d] Procesando ORCID: %s", idx, length(orcid_ids), id))
    
    out <- tryCatch(
      run_pipeline(id) |> mutate(orcid_id = id, .before = 1),
      error = function(e) {
        message("  -> Error con ", id, ": ", conditionMessage(e))
        tibble(orcid_id = id)  # fila vacia si falla, para no cortar el loop
      }
    )
    
    resultado[[idx]] <- out
    
    # Guardado incremental: reescribe el CSV con lo acumulado hasta ahora
    acumulado <- bind_rows(resultado)
    write.csv(acumulado, archivo_incremental, row.names = FALSE)
    
    Sys.sleep(pausa_seg)  # pausa cortesía entre autores (evita rate limiting)
  }
  
  bind_rows(resultado)
}

# -----------------------------------------------------------
# Ejemplo de uso - un solo autor
# -----------------------------------------------------------
# orcid_id <- "0000-0002-1825-0097"
# resultado <- run_pipeline(orcid_id)
# write.csv(resultado, "publicaciones_orcid.csv", row.names = FALSE)

# -----------------------------------------------------------
# Ejemplo de uso - lista de autores
# -----------------------------------------------------------
  orcid_ids <- c(
    "0000-0002-0999-4983",
    "0000-0002-2974-9678",
    "0000-0001-8318-675X",
    "0000-0003-4289-6530",
    "0000-0003-0392-7031",
    "0000-0003-1265-7854",
    "0000-0003-0837-3699",
    "0000-0002-8044-0895",
    "0000-0002-4635-9380",
    "0000-0001-6398-5204",
    "0000-0003-0903-1789",
    "0000-0002-5959-9439",
    "0000-0002-9197-3611",
    "0000-0002-0365-5526",
    "0000-0002-0410-0725",
    "0000-0002-9280-7316",
    "0000-0002-6158-2433",
    "0000-0002-9036-3766",
    "0000-0002-5801-4232",
    "0000-0002-1757-6976",
    "0000-0001-6355-2396",
    "0000-0001-7855-4300",
    "0000-0002-2370-6909",
    "0000-0002-2844-619X",
    "0000-0003-2337-7188",
    "0000-0001-9297-3480",
    "0000-0001-6713-0166",
    "0000-0003-4582-0507",
    "0000-0001-6975-3789",
    "0000-0002-3282-6670",
    "0000-0003-3905-9520",
    "0000-0003-2459-3042",
    "0000-0002-1824-464X",
    "0000-0002-4023-2389",
    "0000-0001-6945-5327",
    "0000-0002-8282-1122",
    "0000-0001-9108-3347",
    "0000-0002-0691-5231",
    "0009-0006-5464-8349",
    "0000-0003-2499-2430",
    "0000-0002-1462-504X",
    "0000-0002-7772-3544",
    "0000-0002-7252-6605",
    "0000-0001-5053-0198",
    "0009-0005-6437-2043",
    "0000-0001-8211-1457",
    "0000-0001-6301-7609",
    "0000-0003-0063-4452",
    "0000-0001-8176-6195",
    "0000-0003-3016-042X",
    "0009-0000-0774-043X",
    "0000-0003-1502-581X",
    "0000-0002-1957-080X",
    "0000-0003-1226-337X",
    "0000-0001-5385-290X",
    "0000-0002-3143-8502",
    "0000-0001-6795-2573",
    "0000-0002-7761-0953",
    "0000-0001-7740-2559",
    "0000-0002-1874-9031",
    "0000-0002-9673-5727",
    "0000-0001-6901-0846",
    "0009-0000-5427-0565",
    "0000-0002-6956-2357",
    "0000-0001-7401-3283",
    "0000-0002-7713-8230",
    "0000-0003-1283-5710",
    "0000-0001-8080-4049",
    "0000-0003-2102-6966",
    "0000-0003-2443-3858",
    "0000-0002-8160-5380"
  )
resultado_multi <- run_pipeline_multi(orcid_ids)

save(resultado_multi, file="output/orcid.rdata")

load("output/orcid.rdata")
load("output/publicaciones_facso.rdata")
load("input/primera_jeraq.rdata")
load("input/consolidado-publicaciones.rdata")


books <- resultado_multi |> filter(tipo=="book" | tipo=="book-chapter",
                                   anio>2019 & anio<2026)

articulos_doi <- resultado_multi |>  filter(tipo=="journal-article" & !is.na(doi), anio>2019 & anio<2026)

articulos_doi_subset <- articulos_doi |>  filter(!doi %in% consolidado_long$doi) |>  distinct(doi, .keep_all = T)

### Cargar base colab y academicos

acad <- read_xlsx("input/acad.xlsx")
colab <- read_xlsx("input/colab.xlsx")


acad <- acad |> clean_names() |> 
  mutate(
    nombre_completo = paste0(nombres, " ", paterno, " ", materno),
    # Limpieza de materno: NA, vacío, "NULL"/"NA" en texto, o solo simbolos/puntuacion -> NA real
    materno_limpio = materno |>
      str_trim() |>
      na_if("") |>
      (\(x) if_else(str_detect(x, regex("^(na|null|n/a|s/n|sin dato)$", ignore_case = TRUE)), NA_character_, x))() |>
      (\(x) if_else(str_detect(x, "^[[:punct:][:space:]]*$"), NA_character_, x))(),
    
    apellidos = paste(paterno, coalesce(materno_limpio, "")) |> str_squish()
  )  |> 
  select(reparticion,
         cargo,
         sexo,
         edad,
         rut,
         apellidos,
         nombre_completo,
         horas_reales) |> 
  group_by(rut) %>%
  mutate(suma_horas = sum(horas_reales, na.rm = TRUE)) %>%
  {
    # Casos donde la suma es <= 44: colapsar sumando horas
    casos_suma <- filter(., suma_horas <= 44) %>%
      slice(1) %>%                          # se queda con un registro representativo
      mutate(horas_reales = suma_horas)      # pero con la suma de horas
    
    # Casos donde la suma es > 44: quedarse con el de mayor horas_reales
    casos_max <- filter(., suma_horas > 44) %>%
      slice_max(horas_reales, n = 1, with_ties = FALSE)
    
    bind_rows(casos_suma, casos_max)
  } %>%
  ungroup() %>%
  select(-suma_horas)


colab <- colab |>  clean_names() |> 
  mutate(
    # Limpieza de materno: NA, vacío, "NULL"/"NA" en texto, o solo simbolos/puntuacion -> NA real
    materno_limpio = ap_materno |>
      str_trim() |>
      na_if("") |>
      (\(x) if_else(str_detect(x, regex("^(na|null|n/a|s/n|sin dato)$", ignore_case = TRUE)), NA_character_, x))() |>
      (\(x) if_else(str_detect(x, "^[[:punct:][:space:]]*$"), NA_character_, x))(),
    
    apellidos = paste(ap_paterno, coalesce(materno_limpio, "")) |> str_squish()
  )  |> 
  select(id_orcid, apellidos)

primera_jeraq <- primera_jeraq |>  rename(rut= rut_investigador)


acad_colab <- acad |> 
  left_join(colab, by="apellidos") |> 
  left_join(primera_jeraq, by="rut")

base_books <- books |> 
  mutate(id_orcid = orcid_id) |> 
  left_join(acad_colab, by = "id_orcid")


acad_articulos_doi <- articulos_doi_subset |> 
  mutate(id_orcid = orcid_id) |> 
  left_join(acad_colab, by="id_orcid") |> 
  filter(anio>= jerarquizacion)

base_orcid <- bind_rows(acad_articulos_doi, base_books)


base_orcid <- base_orcid |> 
  mutate(departamento = case_when(
    str_detect(str_to_lower(reparticion), "psicología|psicologia") ~ "Psicología",
    str_detect(str_to_lower(reparticion), "sociología|sociologia") ~ "Sociología",
    str_detect(str_to_lower(reparticion), "antropología|antropologia") ~ "Antropología",
    str_detect(str_to_lower(reparticion), "trabajo social") ~ "Trabajo social",
    str_detect(str_to_lower(reparticion), "educación|educacion") ~ "Educación",
    str_detect(str_to_lower(reparticion), "postgrado") ~ "Postgrado",
    TRUE ~ NA),
    jerarquia = case_when(
      str_detect(str_to_lower(cargo), "titular")      ~ "Titular",
      str_detect(str_to_lower(cargo), "asociado")     ~ "Asociado",
      str_detect(str_to_lower(cargo), "asistente")    ~ "Asistente",
      str_detect(str_to_lower(cargo), "postdoctoral") ~ "Investigador Postdoctoral",
      str_detect(str_to_lower(cargo), "adjunto")      ~ "Adjunto",
      str_detect(str_to_lower(cargo), "instructor")   ~ "Instructor",
      str_detect(str_to_lower(cargo), "ayudante")     ~ NA_character_,
      str_detect(str_to_lower(cargo), "evaluado")     ~ NA_character_)) |> 
  filter(jerarquia %in% c("Titular", "Asociado", "Asistente", "Adjunto", "Instructor")) |> 
  select(titulo,
         nombre_revista=revista,
         anio,
         doi,
         tipo_documento = tipo,
         rut,
         sexo,
         edad,
         nombre_completo,
         horas_reales,
         issn, departamento, jerarquia)

save(base_orcid, file="output/base_orcid.rdata")
# save(base_books, file="output/base_books.rdata")
