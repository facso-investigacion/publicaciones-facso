# Identificacion inversa FACSO: RUT -> DOI -> authorships -> author.id.
# API: https://help.openalex.org/api/get-single-entities/
# Campos: https://help.openalex.org/data/authorships/

# 0. SET-UP ----
p_load(dplyr, 
       tidyr, 
       purrr, 
       tibble, 
       readr, 
       readxl, 
       janitor, 
       stringr, 
       stringi,
       stringdist, 
       httr2, 
       digest, 
       rlang)


archivo_publicaciones <- "input/data/raw/base-publicaciones-cindai.rdata"
archivo_academicos <- "input/data/raw/acad.xlsx"
carpeta_cache <- "input/data/raw/oa_profile_search"
carpeta_proc <- "input/data/proc"
carpeta_revision <- "output/oa_profile_search"

oa_api_key <- Sys.getenv("OPENALEX_API_KEY", unset = "")
actualizar_cache <- FALSE
reintentar_no_encontrados <- FALSE
pausa_segundos <- 0.2
umbral_similitud <- 0.90 # Jaro-Winkler

# Exclusiones confirmadas:
exclusiones_manuales <- tribble(
  ~rut, ~author_id, ~motivo_exclusion,
  "0088927222", "https://openalex.org/A5076046406", "Homonimo: este perfil no corresponde a Claudio Duarte"
)


# 1. FUNCIONES AUXILIARES ----

normalizar_rut <- function(x) {
  x |>
    as.character() |>
    str_to_upper() |>
    str_remove_all("[^0-9K]") |>
    na_if("") |>
    str_pad(width = 10, side = "left", pad = "0")
}

normalizar_doi <- function(x) {
  x |>
    as.character() |>
    str_trim() |>
    str_to_lower() |>
    str_remove("^doi\\s*:\\s*") |>
    str_remove("^https?://(?:dx\\.)?doi\\.org/") |>
    str_trim() |>
    na_if("")
}

normalizar_nombre <- function(x) {
  x |>
    as.character() |>
    stringi::stri_trans_general("Latin-ASCII") |>
    str_to_lower() |>
    str_replace_all("[^a-z ]", " ") |>
    str_remove_all("\\bna\\b") |>
    str_squish() |>
    na_if("")
}

firma_nombre <- function(x) {
  map_chr(x, \(nombre) {
    if (is.na(nombre)) return(NA_character_)
    str_split(nombre, " ")[[1]] |> sort() |> str_c(collapse = " ")
  })
}

colapsar_unicos <- function(x) {
  x <- x[!is.na(x) & x != ""] |> unique() |> sort()
  if (length(x) == 0L) return(NA_character_)
  str_c(x, collapse = "; ")
}

validar_columnas <- function(datos, columnas, origen) {
  faltantes <- setdiff(columnas, names(datos))
  if (length(faltantes) > 0L) {
    abort(str_c(origen, ": faltan columnas: ", str_c(faltantes, collapse = ", ")))
  }
}

compatible_iniciales <- function(nombre_local, nombre_oa) {
  if (is.na(nombre_local) || is.na(nombre_oa)) return(FALSE)
  locales <- str_split(nombre_local, " ")[[1]]
  oa <- str_split(nombre_oa, " ")[[1]]
  if (length(locales) != length(oa) || length(oa) < 2L) return(FALSE)
  if (!any(nchar(oa) == 1L)) return(FALSE)
  # Exigir una palabra completa compartida evita comparar solo iniciales.
  if (!any(oa[nchar(oa) >= 3L] %in% locales)) return(FALSE)
  # Consumir primero palabras completas; despues las iniciales restantes.
  for (token in oa[order(nchar(oa), decreasing = TRUE)]) {
    indices <- if (nchar(token) == 1L) {
      which(str_sub(locales, 1, 1) == token)
    } else {
      which(locales == token)
    }
    if (length(indices) == 0L) return(FALSE)
    locales <- locales[-indices[[1]]]
  }
  TRUE
}

plantilla_comparacion <- tibble(
  metodo = character(), prioridad = integer(), similitud = double()
)

comparar_nombre <- function(variante, nombre_oa) {
  if (is.na(variante) || is.na(nombre_oa)) {
    return(tibble(metodo = "sin_coincidencia", prioridad = 0L, similitud = 0))
  }
  firma_local <- firma_nombre(variante)
  firma_oa <- firma_nombre(nombre_oa)
  tokens_locales <- str_split(variante, " ")[[1]]
  tokens_oa <- str_split(nombre_oa, " ")[[1]]
  similitud <- 1 - stringdist::stringdist(
    firma_local, firma_oa, method = "jw", p = 0.1
  )
  exacta <- firma_local == firma_oa &&
    length(tokens_locales) >= 2L && all(nchar(tokens_locales) > 1L)
  iniciales <- compatible_iniciales(variante, nombre_oa) ||
    (firma_local == firma_oa && any(nchar(tokens_locales) == 1L))
  aproximada <- similitud >= umbral_similitud &&
    any(tokens_oa[nchar(tokens_oa) >= 3L] %in% tokens_locales)
  tibble(
    metodo = case_when(
      exacta ~ "exacta_normalizada",
      iniciales ~ "iniciales",
      aproximada ~ "aproximada",
      TRUE ~ "sin_coincidencia"
    ),
    prioridad = case_when(exacta ~ 3L, iniciales ~ 2L, aproximada ~ 1L, TRUE ~ 0L),
    similitud = similitud
  )
}

consultar_doi <- function(doi_consulta) {
  archivo_cache <- file.path(
    carpeta_cache,
    str_c(digest::digest(doi_consulta, algo = "sha256", serialize = FALSE), ".rds")
  )
  # Si una interrupcion dejo un archivo incompleto, se consulta de nuevo.
  guardado <- if (file.exists(archivo_cache)) {
    tryCatch(read_rds(archivo_cache), error = \(e) NULL)
  } else {
    NULL
  }
  reutilizables <- if (reintentar_no_encontrados) "encontrado" else {
    c("encontrado", "no_encontrado")
  }
  if (!actualizar_cache && is.list(guardado) &&
      identical(guardado$doi, doi_consulta) &&
      isTRUE(guardado$estado %in% reutilizables)) {
    return(guardado)
  }

  Sys.sleep(pausa_segundos)
  # Codificar el identificador protege DOI con ?, #, ; u otros signos.
  url <- str_c(
    "https://api.openalex.org/works/",
    utils::URLencode(str_c("https://doi.org/", doi_consulta), reserved = TRUE)
  )
  solicitud <- request(url) |>
    req_user_agent("facso-oa-profile-search/1.0") |>
    req_timeout(45) |>
    req_retry(
      max_tries = 4,
      max_seconds = 120,
      retry_on_failure = TRUE,
      is_transient = \(respuesta) resp_status(respuesta) %in%
        c(429L, 500L, 502L, 503L, 504L)
    ) |>
    req_error(is_error = \(respuesta) FALSE)
  if (nzchar(oa_api_key)) solicitud <- req_auth_bearer_token(solicitud, oa_api_key)

  registro <- list(
    doi = doi_consulta, consultado_en = Sys.time(), estado = "error_consulta",
    http_status = NA_integer_, detalle = NA_character_, work = NULL
  )
  respuesta <- tryCatch(req_perform(solicitud), error = \(e) NULL)
  if (is.null(respuesta)) {
    registro$detalle <- "Fallo de transporte o reintentos agotados; reejecutar."
  } else {
    registro$http_status <- resp_status(respuesta)
    if (registro$http_status == 404L) {
      registro$estado <- "no_encontrado"
    } else if (registro$http_status == 200L) {
      work <- tryCatch(
        resp_body_json(respuesta, simplifyVector = FALSE), error = \(e) NULL
      )
      if (is.list(work) &&
          isTRUE(str_detect(work$id %||% "", "^https://openalex\\.org/W[0-9]+$")) &&
          identical(normalizar_doi(work$doi %||% NA_character_), doi_consulta)) {
        registro$estado <- "encontrado"
        registro$work <- work
      } else {
        registro$detalle <- "Respuesta invalida o DOI devuelto distinto al solicitado."
      }
    } else {
      registro$detalle <- str_c("HTTP ", registro$http_status, "; reejecutar.")
    }
  }
  write_rds(registro, archivo_cache)
  registro
}

plantilla_autorias <- tibble(
  doi = character(), work_id = character(), anio_oa = integer(),
  posicion_autoria = integer(), author_id = character(), orcid = character(),
  nombre_oa = character(), nombre_publicado = character(),
  instituciones = character(), paises = character(), afiliaciones_raw = character()
)

extraer_autorias <- function(registro) {
  if (registro$estado != "encontrado") return(plantilla_autorias)
  work <- registro$work
  imap(work$authorships %||% list(), \(autoria, indice) {
    tibble(
      doi = registro$doi,
      work_id = work$id,
      anio_oa = as.integer(work$publication_year %||% NA_integer_),
      posicion_autoria = as.integer(indice),
      author_id = autoria$author$id %||% NA_character_,
      orcid = autoria$author$orcid %||% NA_character_,
      nombre_oa = autoria$author$display_name %||% NA_character_,
      nombre_publicado = autoria$raw_author_name %||% NA_character_,
      instituciones = map_chr(
        autoria$institutions %||% list(), \(x) x$display_name %||% NA_character_
      ) |> colapsar_unicos(),
      paises = unlist(autoria$countries, use.names = FALSE) |> colapsar_unicos(),
      afiliaciones_raw = unlist(
        autoria$raw_affiliation_strings, use.names = FALSE
      ) |> colapsar_unicos()
    )
  }) |>
    list_rbind(ptype = plantilla_autorias)
}

# 2. CARGA DATOS ----
academicos <- read_excel(archivo_academicos, col_types = "text") |>
  clean_names()

academicos <- academicos |>
  mutate(
    rut = normalizar_rut(rut),
    across(c(nombres, paterno, materno), \(x) str_squish(coalesce(x, ""))),
    reparticion_norm = normalizar_nombre(reparticion),
    departamento = case_when(
      str_detect(reparticion_norm, "psicologia") ~ "Psicología",
      str_detect(reparticion_norm, "sociologia") ~ "Sociología",
      str_detect(reparticion_norm, "antropologia") ~ "Antropología",
      str_detect(reparticion_norm, "trabajo social") ~ "Trabajo social",
      str_detect(reparticion_norm, "educacion") ~ "Educación",
      str_detect(reparticion_norm, "postgrado") ~ "Postgrado",
      TRUE ~ NA_character_
    ),
    nombre1 = word(nombres, 1),
    nombre2 = word(nombres, 2),
    nombre_completo = str_squish(str_c(nombres, paterno, materno, sep = " ")),
    nombre_np = str_squish(str_c(nombre1, paterno, sep = " ")),
    nombre_nnp = str_squish(str_c(nombre1, coalesce(nombre2, ""), paterno, sep = " ")),
    nombre_npm = str_squish(str_c(nombre1, paterno, materno, sep = " ")),
    nombre_n2p = if_else(
      !is.na(nombre2) & nombre2 != "", str_squish(str_c(nombre2, paterno, sep = " ")),
      NA_character_
    ),
    across(
      c(nombre_completo, nombre_np, nombre_nnp, nombre_npm, nombre_n2p),
      \(x) if_else(nombres == "" | paterno == "", NA_character_, x)
    )
  ) |>
  select(rut, departamento, nombre_completo, nombre_np, nombre_nnp, nombre_npm, nombre_n2p) |>
  distinct()

publicaciones <- base_final |>
  filter(tipo_documento == "journal-article") |>
  transmute(
    rut = normalizar_rut(rut), anio = as.integer(as.character(anio)),
    doi_original = as.character(doi), doi = normalizar_doi(doi)
  ) |>
  distinct()

articles_facso <- publicaciones |>
  left_join(academicos, by = "rut", relationship = "many-to-many", na_matches = "never") |>
  mutate(
    doi_valido = coalesce(str_detect(doi, "^10\\.[0-9]{4,9}/[^\\s]+$"), FALSE),
    rut_valido = coalesce(str_detect(rut, "^[0-9]{9}[0-9K]$"), FALSE)
  ) |>
  distinct()

write_rds(articles_facso, file.path(carpeta_proc, "articles_facso.rds"))
articles_facso |>
  filter(!doi_valido | !rut_valido | is.na(nombre_completo) | nombre_completo == "") |>
  write_excel_csv(file.path(carpeta_revision, "insumos_por_revisar.csv"), na = "")

variantes_nombres <- articles_facso |>
  filter(rut_valido) |>
  select(rut, starts_with("nombre")) |>
  pivot_longer(-rut, names_to = "tipo_variante", values_to = "variante_original") |>
  mutate(variante = normalizar_nombre(variante_original)) |>
  filter(!is.na(variante), str_count(variante, "\\S+") >= 2L) |>
  distinct(rut, variante, .keep_all = TRUE)

# 3. EXTRACCION REANUDABLE DE WORKS POR DOI ----

dois <- articles_facso |>
  filter(doi_valido) |>
  distinct(doi) |>
  arrange(doi) |>
  pull(doi)
if (length(dois) == 0L) {
  abort("No hay DOI validos para consultar; revisar insumos_por_revisar.csv.")
}

resultados_oa <- vector("list", length(dois))
for (i in seq_along(dois)) {
  resultados_oa[[i]] <- consultar_doi(dois[[i]])
  if (i == 1L || i %% 25L == 0L || i == length(dois)) {
    message(str_c("DOI procesados (incluye cache): ", i, "/", length(dois)))
  }
  if (resultados_oa[[i]]$http_status %in% c(401L, 403L, 429L)) {
    abort(str_c(
      "OpenAlex respondio HTTP ", resultados_oa[[i]]$http_status,
      ". Revisar clave/cuota y reejecutar. Los DOI previos quedaron en cache."
    ))
  }
}

estado_dois <- map(resultados_oa, \(registro) {
  tibble(
    doi = registro$doi, estado_consulta = registro$estado,
    http_status = registro$http_status, consultado_en = registro$consultado_en,
    detalle = registro$detalle,
    n_autorias = length(registro$work$authorships %||% list()),
    # La API limita authorships a 100: esta bandera indica posible truncamiento.
    posible_truncamiento = n_autorias >= 100L
  )
}) |>
  list_rbind()

autorias_oa <- map(resultados_oa, extraer_autorias) |>
  list_rbind(ptype = plantilla_autorias)
write_rds(autorias_oa, file.path(carpeta_proc, "oa_autorias_por_doi.rds"))
write_excel_csv(estado_dois, file.path(carpeta_revision, "estado_dois.csv"), na = "")

# 4. COMPARACION DENTRO DE CADA DOI ----

# Comparar solo con autores del articulo ya vinculado localmente al RUT.
# No filtrar por afiliacion actual: puede faltar o haber cambiado.
nombres_oa <- autorias_oa |>
  select(doi, work_id, posicion_autoria, author_id, nombre_oa, nombre_publicado) |>
  pivot_longer(
    c(nombre_oa, nombre_publicado),
    names_to = "fuente_nombre_oa", values_to = "nombre_observado"
  ) |>
  mutate(nombre_normalizado_oa = normalizar_nombre(nombre_observado)) |>
  filter(!is.na(nombre_normalizado_oa))

comparaciones <- articles_facso |>
  filter(doi_valido, rut_valido) |>
  distinct(rut, doi) |>
  inner_join(variantes_nombres, by = "rut", relationship = "many-to-many") |>
  inner_join(nombres_oa, by = "doi", relationship = "many-to-many")
puntajes <- map2(
  comparaciones$variante, comparaciones$nombre_normalizado_oa, comparar_nombre
) |>
  list_rbind(ptype = plantilla_comparacion)
comparaciones <- bind_cols(comparaciones, puntajes)

# Una evidencia por autoria: las variantes de nombre no cuentan como nuevos DOI.
evidencias <- comparaciones |>
  filter(prioridad > 0L) |>
  arrange(desc(prioridad), desc(similitud), tipo_variante, fuente_nombre_oa) |>
  group_by(rut, doi, work_id, posicion_autoria) |>
  slice_head(n = 1L) |>
  ungroup() |>
  left_join(
    autorias_oa, by = c("doi", "work_id", "posicion_autoria", "author_id"),
    relationship = "many-to-one"
  ) |>
  mutate(
    id_valido = coalesce(
      str_detect(author_id, "^https://openalex\\.org/A[0-9]+$"), FALSE
    )
  )

# Conservar la evidencia rechazada en una salida de auditoria, sin usarla
# para proponer perfiles, resolver empates ni contar candidatos.
evidencias <- evidencias |>
  left_join(
    exclusiones_manuales,
    by = c("rut", "author_id"),
    relationship = "many-to-one"
  )
evidencias_excluidas <- evidencias |>
  filter(!is.na(motivo_exclusion))
evidencias <- evidencias |>
  filter(is.na(motivo_exclusion)) |>
  select(-motivo_exclusion)

# Conservar empates y casos donde un mismo perfil apunta a varios RUT.
# Se consideran coincidencias exactas y de iniciales al buscar conflictos.
conflictos_id <- evidencias |>
  filter(id_valido, prioridad >= 2L) |>
  summarise(n_rut_candidatos = n_distinct(rut), .by = author_id)

evidencias <- evidencias |>
  group_by(rut, doi) |>
  mutate(n_autores_exactos = n_distinct(author_id[prioridad == 3L & id_valido])) |>
  ungroup() |>
  left_join(conflictos_id, by = "author_id") |>
  mutate(
    n_rut_candidatos = coalesce(n_rut_candidatos, 0L),
    estado_vinculo = case_when(
      !id_valido ~ "sin_author_id",
      n_rut_candidatos > 1L ~ "conflicto_entre_rut",
      n_autores_exactos > 1L ~ "ambiguo_en_articulo",
      prioridad == 3L ~ "propuesto_exacto",
      TRUE ~ "revisar_nombre"
    )
  )

# 5. SALIDAS: TODOS LOS PERFILES CANDIDATOS ----

investigadores <- articles_facso |>
  filter(rut_valido) |>
  summarise(
    nombre_completo = colapsar_unicos(nombre_completo),
    departamento = colapsar_unicos(departamento), .by = rut
  )

oa_perfiles_candidatos <- evidencias |>
  filter(id_valido) |>
  summarise(
    nombres_oa = colapsar_unicos(nombre_oa), orcid = colapsar_unicos(orcid),
    n_dois_evidencia = n_distinct(doi),
    n_dois_exactos = n_distinct(doi[estado_vinculo == "propuesto_exacto"]),
    doi_evidencia = colapsar_unicos(doi),
    metodos = colapsar_unicos(metodo),
    conflicto = any(estado_vinculo %in% c("conflicto_entre_rut", "ambiguo_en_articulo")),
    .by = c(rut, author_id)
  ) |>
  mutate(estado_perfil = case_when(
    conflicto ~ "revisar_conflicto",
    n_dois_exactos > 0L ~ "propuesto_exacto",
    TRUE ~ "revisar_nombre"
  )) |>
  left_join(investigadores, by = "rut") |>
  relocate(rut, nombre_completo, departamento, author_id, estado_perfil) |>
  arrange(rut, desc(n_dois_exactos), desc(n_dois_evidencia), author_id)

# Una fila por RUT-ID; nunca limitar a un unico perfil por persona.
# Revisar estas propuestas antes de usar los ID para la extraccion del script 02.
oa_author_ids_facso <- oa_perfiles_candidatos |>
  filter(estado_perfil == "propuesto_exacto")

resumen_evidencias <- evidencias |>
  left_join(
    oa_author_ids_facso |>
      select(rut, author_id) |>
      mutate(perfil_propuesto = TRUE),
    by = c("rut", "author_id")
  ) |>
  summarise(
    n_candidatos = n_distinct(author_id[id_valido]),
    n_propuestos = n_distinct(author_id[
      coalesce(perfil_propuesto, FALSE) & estado_vinculo == "propuesto_exacto"
    ]),
    .by = c(rut, doi)
  )

revision_articulos <- articles_facso |>
  left_join(estado_dois, by = "doi") |>
  left_join(resumen_evidencias, by = c("rut", "doi")) |>
  mutate(
    across(c(n_candidatos, n_propuestos), \(x) coalesce(x, 0L)),
    estado_revision = case_when(
      !rut_valido ~ "rut_invalido",
      !doi_valido ~ "doi_invalido",
      is.na(nombre_completo) | nombre_completo == "" ~ "sin_nombre_local",
      estado_consulta == "error_consulta" ~ "error_consulta",
      estado_consulta == "no_encontrado" ~ "doi_no_encontrado",
      n_autorias == 0L ~ "sin_autorias",
      n_propuestos > 0L ~ "con_propuesta_exacta",
      n_candidatos > 0L ~ "revisar_candidatos",
      TRUE ~ "sin_coincidencia"
    )
  )

resumen_investigadores <- investigadores |>
  left_join(
    revision_articulos |>
      summarise(
        n_dois_locales = n_distinct(doi[doi_valido]),
        n_dois_encontrados = n_distinct(doi[estado_consulta %in% "encontrado"]),
        .by = rut
      ),
    by = "rut"
  ) |>
  left_join(
    oa_perfiles_candidatos |>
      summarise(
        n_perfiles_candidatos = n_distinct(author_id),
        n_perfiles_propuestos = n_distinct(
          author_id[estado_perfil == "propuesto_exacto"]
        ),
        .by = rut
      ),
    by = "rut"
  ) |>
  mutate(across(starts_with("n_"), \(x) coalesce(x, 0L)))

write_rds(oa_author_ids_facso, file.path(carpeta_proc, "oa_author_ids_facso.rds"))
write_rds(oa_perfiles_candidatos, file.path(carpeta_proc, "oa_perfiles_candidatos.rds"))

# 
# n_errores_consulta <- sum(estado_dois$estado_consulta == "error_consulta")
# if (n_errores_consulta > 0L) {
#   warn(str_c(
#     n_errores_consulta, " DOI quedaron con errores de consulta. ",
#     "Ver estado_dois.csv y reejecutar para reintentarlos."
#   ))
# }



