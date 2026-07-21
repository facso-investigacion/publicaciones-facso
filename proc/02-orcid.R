# =====================================================================
# 02-orcid.R  |  Etapa 2 de 6
#
# Recupera las publicaciones registradas por cada academico en ORCID y
# las enriquece con metadatos de revista (Crossref) e impacto/acceso
# abierto (OpenAlex). Sirve para capturar produccion que no fue declarada
# en SEPAVID, sobre todo libros y capitulos.
#
# Entradas : input/original/orcid-ids.csv        (columna id_orcid)
#            input/original/colab.xlsx           (ORCID <-> apellidos)
#            input/original/primera_jeraq.rdata  (objeto primera_jeraq)
#            input/temp/acad.rds              (etapa 1)
#            input/temp/sepavid-publicaciones.rds (etapa 1)
# Salidas  : input/temp/orcid-crudo.rds          (respuesta cruda de las APIs)
#            input/temp/orcid-publicaciones.rds  (base depurada y cruzada)
#            input/temp/orcid-libros.rds         (libros y capitulos validados)
#
# APIs consultadas (todas publicas, sin credenciales):
#   ORCID     https://pub.orcid.org/v3.0/
#   Crossref  https://api.crossref.org/works/
#   OpenAlex  https://api.openalex.org/works/
# =====================================================================


## ---------------------------------------------------------------------
## 1. CONSULTA A ORCID: OBRAS DE UN AUTOR
## ---------------------------------------------------------------------

obtener_obras_orcid <- function(id_orcid) {
  vacio <- tibble(titulo = character(), tipo = character(),
                  anio = character(), put_code = character(),
                  doi = character())
  
  resp <- GET(paste0("https://pub.orcid.org/v3.0/", id_orcid, "/works"),
              add_headers(Accept = "application/json"))
  stop_for_status(resp)
  
  datos  <- content(resp, as = "text", encoding = "UTF-8") |>
    fromJSON(flatten = TRUE)
  grupos <- datos$group
  
  if (is.null(grupos) || length(grupos) == 0 ||
      (is.data.frame(grupos) && nrow(grupos) == 0)) {
    return(vacio)
  }
  
  # `grupos` es un data.frame por efecto de flatten = TRUE. Iterar con
  # map_dfr(grupos, ...) recorreria COLUMNAS, no filas: hay que iterar
  # explicitamente por indice de fila.
  n_grupos <- if (is.data.frame(grupos)) nrow(grupos) else length(grupos)
  
  map_dfr(seq_len(n_grupos), function(i) {
    # Cada grupo reune la misma obra reportada por varias fuentes;
    # se toma el primer resumen.
    resumenes <- grupos$`work-summary`[[i]]
    resumen   <- if (is.data.frame(resumenes)) resumenes[1, ] else resumenes[[1]]
    
    # El DOI vive dentro de la lista de identificadores externos.
    doi <- NA_character_
    col_ext <- "external-ids.external-id"
    if (col_ext %in% names(resumen)) {
      ext <- resumen[[col_ext]][[1]]
      if (is.data.frame(ext) && "external-id-type" %in% names(ext)) {
        fila_doi <- ext[ext$`external-id-type` == "doi", ]
        if (nrow(fila_doi) > 0) doi <- fila_doi$`external-id-value`[1]
      }
    }
    
    valor <- function(campo) {
      if (campo %in% names(resumen)) safe_chr(resumen[[campo]]) else NA_character_
    }
    
    tibble(
      titulo   = valor("title.title.value"),
      tipo     = valor("type"),
      anio     = valor("publication-date.year.value"),
      put_code = valor("put-code"),
      doi      = doi
    )
  })
}


## ---------------------------------------------------------------------
## 2. ENRIQUECIMIENTO POR DOI
## ---------------------------------------------------------------------

#' Crossref: nombre de revista, ISSN y editorial.
obtener_crossref <- function(doi) {
  vacio <- tibble(revista = NA_character_, issn = NA_character_,
                  editorial = NA_character_)
  if (is.na(doi)) return(vacio)
  
  resp <- tryCatch(GET(paste0("https://api.crossref.org/works/", doi)),
                   error = function(e) NULL)
  if (is.null(resp) || status_code(resp) != 200) return(vacio)
  
  datos <- tryCatch(
    content(resp, as = "text", encoding = "UTF-8") |> fromJSON(flatten = TRUE),
    error = function(e) NULL
  )
  if (is.null(datos) || is.null(datos$message)) return(vacio)
  msg <- datos$message
  
  # Una revista puede declarar ISSN impreso y electronico: se conservan
  # ambos separados por ";" y se desagregan en la etapa 03.
  issn <- msg$ISSN
  issn <- if (is.null(issn) || length(issn) == 0) NA_character_
  else paste(unlist(issn, use.names = FALSE), collapse = "; ")
  
  tibble(
    revista   = safe_chr(msg$`container-title`),
    issn      = issn,
    editorial = safe_chr(msg$publisher)
  )
}

#' OpenAlex: estado de acceso abierto, citas y nombre de revista.
obtener_openalex <- function(doi) {
  vacio <- tibble(oa_status = NA_character_, citas = NA_real_,
                  revista_openalex = NA_character_)
  if (is.na(doi)) return(vacio)
  
  resp <- tryCatch(
    GET(paste0("https://api.openalex.org/works/https://doi.org/", doi)),
    error = function(e) NULL
  )
  if (is.null(resp) || status_code(resp) != 200) return(vacio)
  
  datos <- tryCatch(
    content(resp, as = "text", encoding = "UTF-8") |> fromJSON(flatten = TRUE),
    error = function(e) NULL
  )
  if (is.null(datos)) return(vacio)
  
  tibble(
    oa_status        = safe_chr(datos$open_access.oa_status),
    citas            = safe_num(datos$cited_by_count),
    revista_openalex = safe_chr(datos$primary_location.source.display_name)
  )
}


## ---------------------------------------------------------------------
## 3. PIPELINE POR AUTOR Y POR LISTA DE AUTORES
## ---------------------------------------------------------------------

pipeline_orcid <- function(id_orcid) {
  obras <- obtener_obras_orcid(id_orcid)
  message("    obras encontradas: ", nrow(obras))
  
  obras |>
    mutate(
      crossref = map(doi, obtener_crossref),
      openalex = map(doi, obtener_openalex)
    ) |>
    unnest(crossref) |>
    unnest(openalex)
}

#' Recorre la lista de ORCID con guardado incremental y reanudacion.
#'
#' Si la corrida se interrumpe, al volver a ejecutarla se saltan los ORCID
#' ya descargados en `archivo_parcial`. `pausa_seg` evita el rate limiting
#' de las APIs.
pipeline_orcid_multi <- function(ids_orcid, pausa_seg = 1,
                                 archivo_parcial = ruta_temp("orcid-parcial.rds")) {
  
  acumulado <- if (file.exists(archivo_parcial)) readRDS(archivo_parcial) else tibble()
  ya_hechos <- if (nrow(acumulado) > 0) unique(acumulado$id_orcid) else character()
  pendientes <- setdiff(ids_orcid, ya_hechos)
  
  if (length(pendientes) == 0) return(acumulado)
  message("  ORCID pendientes: ", length(pendientes), " de ", length(ids_orcid))
  
  for (i in seq_along(pendientes)) {
    id <- pendientes[i]
    message(sprintf("  [%d/%d] ORCID %s", i, length(pendientes), id))
    
    fila <- tryCatch(
      pipeline_orcid(id) |> mutate(id_orcid = id, .before = 1),
      error = function(e) {
        message("    -> error: ", conditionMessage(e))
        tibble(id_orcid = id)   # fila vacia: no corta el recorrido
      }
    )
    
    acumulado <- bind_rows(acumulado, fila)
    saveRDS(acumulado, archivo_parcial)   # checkpoint
    Sys.sleep(pausa_seg)
  }
  
  acumulado
}


## ---------------------------------------------------------------------
## 4. DESCARGA (con cache)
## ---------------------------------------------------------------------

verificar_archivos(ruta_input("orcid-ids.csv"))

ids_orcid <- read_csv(ruta_input("orcid-ids.csv"), show_col_types = FALSE) |>
  pull(id_orcid) |>
  unique()

orcid_crudo <- usar_cache(
  ruta_temp("orcid-crudo.rds"),
  pipeline_orcid_multi(ids_orcid)
)


## ---------------------------------------------------------------------
## 5. SELECCION DE OBRAS DENTRO DEL ALCANCE
## ---------------------------------------------------------------------
## De ORCID interesan dos cosas:
##   a) libros y capitulos (SEPAVID los subregistra);
##   b) articulos con DOI que NO fueron declarados en SEPAVID.

sepavid <- readRDS(ruta_temp("sepavid-publicaciones.rds"))

orcid_obras <- orcid_crudo |>
  mutate(
    anio           = suppressWarnings(as.integer(anio)),
    doi            = norm_doi(doi),
    titulo         = str_squish(titulo),
    tipo_documento = recodificar_tipo_doc(tipo)
  ) |>
  filter(!is.na(tipo_documento),
         between(anio, ANIO_INICIO, ANIO_FIN))

orcid_libros_raw <- orcid_obras |>
  filter(tipo_documento %in% c("book", "book-chapter"))

orcid_articulos <- orcid_obras |>
  filter(tipo_documento == "journal-article",
         !is.na(doi),
         !doi %in% sepavid$doi) |>       # ambos DOI ya normalizados
  distinct(doi, .keep_all = TRUE)


## ---------------------------------------------------------------------
## 6. VINCULAR ORCID CON LA PLANTA ACADEMICA
## ---------------------------------------------------------------------
## colab.xlsx es el puente ORCID -> persona. El unico campo comun con
## acad.xlsx son los apellidos, por lo que el cruce es sensible a
## homonimos: se emite una advertencia si los hay.

verificar_archivos(c(ruta_input("colab.xlsx"), ruta_input("primera_jeraq.rdata")))

acad <- readRDS(ruta_temp("acad.rds"))

colab <- read_xlsx(ruta_input("colab.xlsx")) |>
  clean_names() |>
  mutate(
    ap_materno = limpiar_apellido(ap_materno),
    apellidos  = str_squish(paste(ap_paterno, coalesce(ap_materno, "")))
  ) |>
  select(id_orcid, apellidos) |>
  distinct()

duplicados <- colab |> count(apellidos) |> filter(n > 1)
if (nrow(duplicados) > 0) {
  message("  ADVERTENCIA: ", nrow(duplicados),
          " apellidos duplicados en colab.xlsx; el cruce por apellidos ",
          "puede generar filas espurias.")
}

# Fecha de primera jerarquizacion: solo se cuenta la produccion posterior
# al ingreso a la carrera academica.
primera_jeraq <- leer_rdata(ruta_input("primera_jeraq.rdata"), "primera_jeraq") |>
  rename(rut = rut_investigador) |>
  mutate(rut = norm_rut(rut))

acad_orcid <- acad |>
  left_join(colab, by = "apellidos") |>
  left_join(primera_jeraq, by = "rut") |>
  filter(!is.na(id_orcid))


## ---------------------------------------------------------------------
## 7. LISTA DE VALIDACION DE LIBROS (sin filtro de jerarquia)
## ---------------------------------------------------------------------
## Esta lista solo responde una pregunta: "¿este libro/capitulo existe,
## segun el propio ORCID de su autor?". Por eso exige unicamente que el
## autor este vinculado a un ORCID y que la obra sea posterior a su
## jerarquizacion -- A PROPOSITO no exige jerarquia %in% JERARQUIAS_VALIDAS.
##
## Si aqui se exigiera jerarquia valida (como en adjuntar_academico(), mas
## abajo), un libro correctamente declarado en SEPAVID por un academico
## vigente se perderia con solo que el cruce ORCID <-> acad_orcid llegara
## con jerarquia en NA (p. ej. por un desfase de colab.xlsx). Eso reduce el
## conteo final sin dejar rastro en ninguna etapa intermedia.

libros_lookup <- orcid_libros_raw |>
  inner_join(acad_orcid, by = "id_orcid") |>
  filter(anio >= jerarquizacion) |>
  transmute(clave_pub = clave_publicacion(doi, titulo), titulo, anio, rut)

saveRDS(libros_lookup, ruta_temp("orcid-libros.rds"))


## ---------------------------------------------------------------------
## 8. BASE ORCID DEPURADA (esta si exige jerarquia valida)
## ---------------------------------------------------------------------
## A diferencia de la lista de validacion, orcid_publicaciones es la base
## que se fusiona con SEPAVID en la etapa 03: aqui si corresponde exigir
## jerarquia, porque cada fila representa produccion que se le atribuye a
## un academico concreto.

adjuntar_academico <- function(obras) {
  obras |>
    inner_join(acad_orcid, by = "id_orcid") |>
    filter(anio >= jerarquizacion,
           jerarquia %in% JERARQUIAS_VALIDAS)
}

orcid_libros <- adjuntar_academico(orcid_libros_raw)

orcid_publicaciones <- bind_rows(orcid_libros,
                                 adjuntar_academico(orcid_articulos)) |>
  transmute(
    titulo,
    revista = coalesce(revista, revista_openalex),
    anio,
    doi,
    tipo_documento,
    issn,                       # puede traer varios ISSN separados por ";"
    rut, nombre_completo, sexo, edad, horas_reales,
    reparticion, departamento, jerarquia
  )

saveRDS(orcid_publicaciones, ruta_temp("orcid-publicaciones.rds"))


## ---------------------------------------------------------------------
## 9. VERIFICACIONES
## ---------------------------------------------------------------------

message("  Filas autor x publicacion (ORCID): ", nrow(orcid_publicaciones))
print(count(orcid_publicaciones, tipo_documento))