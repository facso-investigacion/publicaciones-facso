# =====================================================================
# 02-orcid.R  |  Etapa 2 de 6
#
# Recupera las publicaciones registradas por cada academico en ORCID y
# las enriquece con metadatos de revista (Crossref) e impacto/acceso
# abierto (OpenAlex). Sirve para capturar produccion que no fue declarada
# en SEPAVID, sobre todo libros y capitulos.
#
# Entradas : input/original/orcid.csv            (rut <-> id_orcid, unica
#                                                  fuente de ORCID; ver
#                                                  leer_orcid() en 00-funciones.R)
#            input/temp/acad.rds              (etapa 1)
#            input/temp/sepavid-publicaciones.rds (etapa 1)
#            catalogos de revistas en input/original/ (via
#            diccionario_revistas(); para articulos sin DOI)
# Salidas  : input/temp/orcid-crudo.rds          (respuesta cruda de las APIs)
#            input/temp/orcid-metadatos-obras.rds (revista e ids externos por obra)
#            input/temp/orcid-publicaciones.rds  (base depurada y cruzada)
#            output/orcid-sin-doi.csv            (decision sobre cada articulo
#                                                  sin DOI)
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


#' Metadatos de revista que ORCID declara para cada obra (todas las
#' versiones de cada grupo): nombre de revista, tipos de identificador
#' externo (eid = Scopus, wosuid = WoS, isbn, ...) e ISSN. Se usan para
#' decidir que articulos SIN DOI entran a la base (seccion 5), que no pasan
#' por Crossref y por eso no traen revista ni ISSN.
obtener_metadatos_orcid <- function(id_orcid) {
  vacio <- tibble(put_code = NA_character_, revista_orcid = NA_character_,
                  ext_tipos = NA_character_, issn_orcid = NA_character_)
  resp <- tryCatch(GET(paste0("https://pub.orcid.org/v3.0/", id_orcid, "/works"),
                       add_headers(Accept = "application/json")),
                   error = function(e) NULL)
  if (is.null(resp) || status_code(resp) != 200) return(vacio)
  grupos <- content(resp, as = "text", encoding = "UTF-8") |>
    fromJSON(simplifyVector = FALSE) |>
    pluck("group")
  if (length(grupos) == 0) return(vacio)

  map_dfr(grupos, \(g) map_dfr(g$`work-summary`, \(s) {
    ids <- s$`external-ids`$`external-id` %||% list()
    tipos <- map_chr(ids, \(e) e$`external-id-type` %||% "")
    tibble(
      put_code      = as.character(s$`put-code`),
      revista_orcid = s$`journal-title`$value %||% NA_character_,
      ext_tipos     = paste(unique(tipos), collapse = ";"),
      issn_orcid    = map_chr(ids[tipos == "issn"], \(e) e$`external-id-value` %||% "") |>
                        paste(collapse = "; ") |> na_if("")
    )
  }))
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
  
  encabezados <- if (nzchar(OPENALEX_API_KEY)) {
    add_headers(Authorization = paste("Bearer", OPENALEX_API_KEY))
  } else {
    NULL
  }
  resp <- tryCatch(
    GET(paste0("https://api.openalex.org/works/https://doi.org/", doi), encabezados),
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

orcid_acad <- leer_orcid()
ids_orcid  <- unique(orcid_acad$id_orcid)

orcid_crudo <- cache_incremental(
  ruta_temp("orcid-crudo.rds"), ruta_temp("orcid-parcial.rds"),
  \() pipeline_orcid_multi(ids_orcid),
  claves = ids_orcid, columna = "id_orcid"
)

# Metadatos de revista: cache incremental (solo se consultan los ORCID que
# aun no estan; un ORCID sin obras queda igual registrado, con put_code NA).
ruta_meta <- ruta_temp("orcid-metadatos-obras.rds")
orcid_meta <- if (file.exists(ruta_meta) && !FORZAR_API) readRDS(ruta_meta) else tibble(id_orcid = character())
pendientes_meta <- setdiff(ids_orcid, orcid_meta$id_orcid)
if (length(pendientes_meta) > 0) {
  message("  Metadatos de revista ORCID pendientes: ", length(pendientes_meta))
  orcid_meta <- bind_rows(orcid_meta, map_dfr(pendientes_meta, \(id) {
    Sys.sleep(0.2)
    obtener_metadatos_orcid(id) |> mutate(id_orcid = id, .before = 1)
  }))
  saveRDS(orcid_meta, ruta_meta)
}


## ---------------------------------------------------------------------
## 5. SELECCION DE OBRAS DENTRO DEL ALCANCE
## ---------------------------------------------------------------------
## De ORCID interesan tres cosas:
##   a) libros y capitulos (SEPAVID los subregistra);
##   b) articulos con DOI que NO fueron declarados en SEPAVID;
##   c) articulos SIN DOI identificables como tales (seccion 5b): sobre
##      todo revistas latinoamericanas sin DOI, clave para reconstruir la
##      trayectoria completa del periodo (no solo la estancia en la U.).

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

# El descarte contra SEPAVID se hace mas abajo, por par (rut, doi), una vez
# que cada obra ya tiene academico asignado (ver seccion 8).
orcid_articulos_doi <- orcid_obras |>
  filter(tipo_documento == "journal-article",
         !is.na(doi))


## ---------------------------------------------------------------------
## 5b. ARTICULOS SIN DOI: ¿SON ARTICULOS?
## ---------------------------------------------------------------------
## ORCID clasifica como "journal-article" cosas muy distintas. Un articulo
## sin DOI entra solo si se puede identificar como tal:
##   ACEPTA  - identificador de Scopus (eid) o WoS (wosuid); o
##           - revista conocida: su ISSN o su nombre (exacto o Jaro-Winkler
##             >= 0.95) esta en WoS, Scopus, SciELO, Latindex o ERIH; o
##           - el nombre del medio es de revista ("Revista", "Journal",
##             "Cuadernos", "Estudios", ...).
##   EXCLUYE - sin nombre de revista;
##           - titulo de no-articulo (presentacion, editorial, documento de
##             trabajo, entrevista, resena, ...);
##           - medio no academico (prensa, blogs, columnas, anuarios de
##             opinion, congresos, repositorios);
##           - trae ISBN y la revista no es conocida (capitulo mal tipeado).
## Ademas, en 03-id-revistas.R (seccion 1c) se descartan los que tienen la
## misma revista y anio que otro articulo del mismo academico: en el
## diagnostico, ~85% de esos casos eran la misma obra con el titulo
## traducido, algo que la comparacion de titulos no detecta.
## Todas las decisiones quedan en output/orcid-sin-doi.csv.

dic_revistas <- diccionario_revistas()

re_titulo_no_articulo <- regex(str_c(
  "documento de trabajo", "working paper", "^informe", "boletin", "^resena", "resena de",
  "book review", "review of", "^editorial", "^presentacion", "^introduccion", "^entrevista",
  "^interview", "prologo", "^columna", "^carta", "in memoriam", "obituario", "minuta",
  "policy brief", "documento n", "^dossier", "palabras de agradecimiento", sep = "|"),
  ignore_case = TRUE)
re_medio_no_academico <- regex(str_c(
  "ciper", "mostrador", "la tercera", "desconcierto", "le monde", "the conversation", "blog",
  "ssrn", "preprint", "congreso", "conference", "seminario", "jornadas", "actas", "proceedings",
  "diario", "periodico", "newspaper", "radio", "mercurio", "interferencia", "biobio",
  "cooperativa", "observatorio", "documento", "working paper", "repositorio", "zenodo",
  "researchgate", "academia edu", "medium", "palabra publica", "^mensaje", "analisis del ano",
  "^libro", "^series", sep = "|"), ignore_case = TRUE)
re_nombre_de_revista <- regex(str_c(
  "revista", "journal", "cuadernos", "anales", "estudios", "review", "revue", "rivista",
  "boletin", "bulletin", "papers", "psicolog", "sociolog", "antropolog", "educaci",
  "trabajo social", "quaderns", "cadernos", "praxis", "polis", "athenea", "perspectiva",
  "dialog", "investigaci", sep = "|"), ignore_case = TRUE)

orcid_sin_doi <- orcid_obras |>
  filter(tipo_documento == "journal-article", is.na(doi)) |>
  left_join(orcid_meta |> filter(!is.na(put_code)) |> distinct(id_orcid, put_code, .keep_all = TRUE),
            by = c("id_orcid", "put_code")) |>
  mutate(revista_norm = norm_nombre_revista(revista_orcid),
         # ISSN declarado como identificador o escrito dentro del nombre
         # ("Perfiles Latinoamericanos (WoS. ISSN: 0188-7653)")
         issn_conocido = map2_lgl(issn_orcid, revista_orcid, \(i, r) {
           v <- c(str_split_1(coalesce(i, ""), ";\\s*"),
                  str_extract_all(coalesce(r, ""), "[0-9]{4}-?[0-9]{3}[0-9Xx]")[[1]])
           any(norm_issn(v) %in% dic_revistas$issn)
         }))

#' Variantes del nombre de revista tal como lo escriben los autores en
#' ORCID: completo; sin volumen/numero ni parentesis ("Campos en Ciencias
#' Sociales, Vol. 9 N 2"); y antes del primer punto o dos puntos (sigla con
#' subtitulo: "CUHSO. Cultura-Hombre-Sociedad").
variantes_revista <- function(r) {
  sin_vol <- r |>
    str_remove_all("\\([^)]*\\)") |>
    str_remove(regex("[,.;]?\\s*(vol\\.?|volumen|volume|n[°ºo.]|num\\.?|numero|no\\.\\s|\\d{4}).*$", ignore_case = TRUE))
  antes_punto <- str_remove(r, "\\s*[.:].*$")
  unique(na.omit(norm_nombre_revista(c(r, sin_vol, antes_punto))))
}

# Nombre conocido: alguna variante calza exacto, o con Jaro-Winkler >= 0.95
# (variantes de 10+ caracteres) para tolerar diferencias de escritura.
nombres_orcid <- unique(na.omit(orcid_sin_doi$revista_orcid))
conocido <- map_lgl(nombres_orcid, \(r) {
  v <- variantes_revista(r)
  if (any(v %in% dic_revistas$nombres)) return(TRUE)
  v <- v[nchar(v) >= 10]
  length(v) > 0 && any(map_lgl(v, \(n) max(1 - stringdist(n, dic_revistas$nombres, method = "jw", p = 0.1)) >= 0.95))
})
nombre_conocido <- nombres_orcid[conocido]

orcid_sin_doi <- orcid_sin_doi |>
  mutate(
    revista_conocida = issn_conocido | revista_orcid %in% nombre_conocido,
    titulo_ascii = quitar_tildes(titulo),
    decision = case_when(
      is.na(revista_norm)                                         ~ "excluido: sin revista",
      str_detect(titulo_ascii, re_titulo_no_articulo)             ~ "excluido: titulo de no-articulo",
      str_detect(revista_norm, re_medio_no_academico)             ~ "excluido: medio no academico",
      str_detect(coalesce(ext_tipos, ""), "eid|wosuid")           ~ "aceptado: indexado (Scopus/WoS)",
      str_detect(coalesce(ext_tipos, ""), "isbn") & !revista_conocida ~ "excluido: ISBN (probable capitulo)",
      revista_conocida                                            ~ "aceptado: revista en catalogo",
      str_detect(revista_norm, re_nombre_de_revista)              ~ "aceptado: nombre de revista",
      TRUE                                                        ~ "excluido: medio no reconocible"
    )
  )

orcid_articulos <- bind_rows(
  orcid_articulos_doi,
  orcid_sin_doi |>
    filter(str_starts(decision, "aceptado")) |>
    mutate(revista = revista_orcid, issn = issn_orcid) |>
    select(any_of(names(orcid_articulos_doi)))
)


## ---------------------------------------------------------------------
## 6. VINCULAR ORCID CON LA PLANTA ACADEMICA
## ---------------------------------------------------------------------
## orcid.csv trae el rut, asi que el cruce es directo (sin pasar por
## apellidos). Se avisa si la planta trae academicos que aun no tienen fila
## en orcid.csv, para que se agreguen (con id_orcid vacio si no se conoce).

acad <- readRDS(ruta_temp("acad.rds"))

acad_orcid <- acad |>
  inner_join(orcid_acad, by = "rut")

ruts_en_archivo <- read_csv(ruta_input("orcid.csv"), col_types = cols(.default = "c")) |>
  pull(rut) |>
  norm_rut()
sin_fila <- acad |> filter(!rut %in% ruts_en_archivo)
if (nrow(sin_fila) > 0) {
  message("  ADVERTENCIA: ", nrow(sin_fila), " academicos de la planta no tienen fila en ",
          "input/original/orcid.csv: ", paste(sin_fila$nombre_completo, collapse = "; "))
}


## ---------------------------------------------------------------------
## 8. BASE ORCID DEPURADA (esta si exige jerarquia valida)
## ---------------------------------------------------------------------
## A diferencia de la lista de validacion (seccion 7), orcid_publicaciones es
## la base que se fusiona con SEPAVID en la etapa 03: aqui si corresponde
## exigir jerarquia, porque cada fila representa produccion que se le
## atribuye a un academico concreto.

adjuntar_academico <- function(obras) {
  obras |>
    inner_join(acad_orcid, by = "id_orcid") |>
    filter(jerarquia %in% JERARQUIAS_VALIDAS)
}

orcid_libros <- adjuntar_academico(orcid_libros_raw)

# Con DOI, por par (rut, doi): un DOI que SEPAVID atribuye a otro academico
# FACSO sigue contando para este rut. Sin DOI no hay con que cruzar contra
# SEPAVID aqui (NA calzaria con NA): se deduplican por titulo y el cruce
# con las otras fuentes lo hace 03-id-revistas.R (secciones 1b y 1c).
orcid_articulos_acad <- adjuntar_academico(orcid_articulos)
orcid_articulos_acad <- bind_rows(
  orcid_articulos_acad |>
    filter(!is.na(doi)) |>
    anti_join(sepavid |> filter(!is.na(doi)) |> select(rut, doi), by = c("rut", "doi")) |>   # ambos DOI ya normalizados
    distinct(rut, doi, .keep_all = TRUE),
  orcid_articulos_acad |>
    filter(is.na(doi)) |>
    distinct(rut, clave = clave_publicacion(doi, titulo), .keep_all = TRUE) |>
    select(-clave)
)

orcid_publicaciones <- bind_rows(orcid_libros, orcid_articulos_acad) |>
  transmute(
    titulo,
    revista = coalesce(revista, revista_openalex),
    anio,
    doi,
    tipo_documento,
    issn,                       # puede traer varios ISSN separados por ";"
    rut, nombre_completo, sexo, edad, horas_reales,
    reparticion, departamento, jerarquia,
    # marca para 03-id-revistas.R (seccion 1c)
    orcid_sin_doi = tipo_documento == "journal-article" & is.na(doi)
  )

saveRDS(orcid_publicaciones, ruta_temp("orcid-publicaciones.rds"))

# Registro de decisiones sobre articulos sin DOI (solo planta con jerarquia valida)
orcid_sin_doi |>
  inner_join(acad_orcid |> filter(jerarquia %in% JERARQUIAS_VALIDAS) |>
               select(id_orcid, rut, nombre_completo, departamento), by = "id_orcid") |>
  transmute(decision, nombre_completo, departamento, anio, revista_orcid, titulo,
            ext_tipos, issn_orcid, revista_conocida, id_orcid, put_code) |>
  arrange(decision, departamento, nombre_completo, anio) |>
  write_excel_csv(ruta_output("orcid-sin-doi.csv"), na = "")


## ---------------------------------------------------------------------
## 9. VERIFICACIONES
## ---------------------------------------------------------------------

message("  Filas autor x publicacion (ORCID): ", nrow(orcid_publicaciones))
print(count(orcid_publicaciones, tipo_documento))