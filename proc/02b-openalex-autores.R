# =====================================================================
# 02b-openalex-autores.R  |  Etapa 2b
#
# Cruce rut <-> author_id de OpenAlex. Dos senales, sin nunca buscar por
# nombre libre en el indice global de autores de OpenAlex:
#
#   1) Ancla por ORCID: para todo academico con id_orcid conocido
#      (input/original/orcid.csv, via leer_orcid()), se toman TODOS los
#      perfiles de OpenAlex que declaran ese ORCID (/authors?filter=orcid:).
#   2) Evidencia por DOI ("identificacion inversa"): para todos los rut, se
#      toman los DOI que SEPAVID/ORCID ya atribuyen con confianza a un rut,
#      se consulta cada work en OpenAlex, y se compara el nombre local del
#      academico (con variantes) contra cada autor de esa publicacion.
#
# Para un rut con ancla, la evidencia de DOI solo AGREGA perfiles que
# OpenAlex dejo partidos sin ORCID; un perfil que declara un ORCID distinto
# al conocido se descarta -- es asi como este script resuelve el caso
# "Catalina Arteaga" (dos author_id con evidencia de DOI igualmente
# "exacta", pero solo uno con el ORCID real de la academica) sin necesitar
# que alguien lo detecte a mano.
#
# Para el resto de los rut (sin ORCID conocido), si dos o mas candidatos
# "propuesto_exacto" declaran un ORCID propio distinto entre si, no hay
# forma de decidir con la evidencia disponible: quedan como
# "revisar_orcid_distinto" y no se usan para descubrir publicaciones
# (etapa 02c) hasta revision manual.
#
# Adaptado de proc/01-oa-profile-search.R (el metodo de evidencia por DOI,
# comparacion de nombres con variantes + Jaro-Winkler, y la lista de
# exclusiones manuales, son suyos; aqui se migran a las convenciones del
# pipeline y se agrega la ancla por ORCID + la desambiguacion entre
# candidatos del mismo rut).
#
# Entradas : input/original/acad.xlsx                 (nombres/paterno/materno
#                                                       por separado; acad.rds
#                                                       ya no los conserva)
#            input/original/orcid.csv               (rut <-> id_orcid)
#            input/temp/sepavid-publicaciones.rds      (etapa 1)
#            input/temp/orcid-publicaciones.rds        (etapa 2)
# Salidas  : input/temp/acad-openalex.rds        (rut, author_id, estado_perfil)
#            input/temp/oa-perfiles-candidatos.rds (todos los candidatos,
#                                                    incluidos los descartados)
#            output/openalex-insumos-por-revisar.csv
#            output/openalex-estado-dois.csv
#            output/orcid-sugeridos.csv  (ORCID que OpenAlex muestra para
#                                          academicos sin ORCID en orcid.csv;
#                                          se copian a mano si se confirman)
#
# API consultada (publica; OPENALEX_API_KEY y OPENALEX_MAILTO opcionales):
#   OpenAlex  https://api.openalex.org/authors/, /works/
# =====================================================================


## ---------------------------------------------------------------------
## 0. CONFIGURACION
## ---------------------------------------------------------------------

carpeta_cache_doi <- ruta_temp("openalex-autores-cache")
dir.create(carpeta_cache_doi, showWarnings = FALSE, recursive = TRUE)

# OPENALEX_API_KEY y OPENALEX_MAILTO viven en 00-funciones.R.
actualizar_cache    <- FALSE
reintentar_no_encontrados <- FALSE
pausa_segundos      <- 0.2
umbral_similitud    <- 0.90 # Jaro-Winkler

# Exclusiones confirmadas (homonimos detectados y descartados a mano).
# El caso Catalina Arteaga (rut 0099800682) NO necesita estar aqui: su
# ORCID es conocido, asi que la seccion 7a descarta el perfil homonimo por
# declarar un ORCID distinto. Se deja documentado igual, como
# respaldo/prueba de regresion.
exclusiones_manuales <- tribble(
  ~rut,          ~author_id,                          ~motivo_exclusion,
  "0088927222",  "https://openalex.org/A5076046406",  "Homonimo: este perfil no corresponde a Claudio Duarte",
  "0099800682",  "https://openalex.org/A5030462844",  "Homonimo: 'Catalina Arteaga' sin materno, ORCID distinto al de Catalina Arteaga Aguirre (respaldo; la ancla ORCID ya la excluye)",
  "0091297612",  "https://openalex.org/A5103109176",  "Homonimo: 'Hector Morales' de genetica de plantas y micotoxinas, no el antropologo",
  "0136711946",  "https://openalex.org/A5056573302",  "Homonimo: 'Jose Luis Rossi' de nutricion animal, no el psicologo"
)

# Inclusiones confirmadas a mano: perfiles de OpenAlex que son del
# academico pero que ni la ancla ORCID ni la evidencia de DOI encuentran
# (sin ORCID en OpenAlex, con un ORCID distinto al de orcid.csv, o sin
# publicaciones en SEPAVID/ORCID que sirvan de evidencia). Prevalecen sobre
# los descartes automaticos.
inclusiones_manuales <- tribble(
  ~rut,          ~author_id,                          ~motivo_inclusion,
  "0098430806",  "https://openalex.org/A5001930147",  "Omar Aguilar: perfil declara ORCID 0000-0002-0290-0155 (vacio, tambien suyo) y orcid.csv tiene 0009-0000-3080-0621 (vacio)",
  "0104499252",  "https://openalex.org/A5115429812",  "Juan Enrique Opazo Marmentini: perfil sin ORCID, afiliacion U. de Chile",
  "0139064119",  "https://openalex.org/A5086230531",  "Camila Marchant Fernandez: el mas completo de sus 3 perfiles; sin ORCID (orcid.csv tiene 0000-0001-8724-8700, vacio)"
)


## ---------------------------------------------------------------------
## 1. FUNCIONES AUXILIARES: COMPARACION DE NOMBRES
## ---------------------------------------------------------------------

normalizar_nombre <- function(x) {
  x |>
    as.character() |>
    quitar_tildes() |>
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


## ---------------------------------------------------------------------
## 2. FUNCIONES AUXILIARES: DESCARGA CON CACHE Y REINTENTOS
## ---------------------------------------------------------------------

consultar_doi <- function(doi_consulta) {
  archivo_cache <- file.path(
    carpeta_cache_doi,
    str_c(digest::digest(doi_consulta, algo = "sha256", serialize = FALSE), ".rds")
  )
  # Si una interrupcion dejo un archivo incompleto, se consulta de nuevo.
  guardado <- if (file.exists(archivo_cache)) {
    tryCatch(readRDS(archivo_cache), error = \(e) NULL)
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
  # `after` tope la espera a 20s: httr2 por defecto respeta el header
  # Retry-After tal cual venga, y OpenAlex puede pedir horas de espera
  # cuando el presupuesto de creditos del tier gratuito esta agotado (no es
  # un limite por segundo que valga la pena reintentar). Sin este tope,
  # max_seconds NO protege: solo se chequea antes de decidir un reintento,
  # no limita el sleep de un reintento ya en curso.
  solicitud <- request(url) |>
    req_user_agent("facso-openalex-autores/1.0") |>
    req_timeout(45) |>
    req_retry(
      max_tries = 4,
      max_seconds = 120,
      retry_on_failure = TRUE,
      is_transient = \(respuesta) resp_status(respuesta) %in%
        c(429L, 500L, 502L, 503L, 504L),
      after = \(respuesta) {
        espera <- suppressWarnings(as.numeric(resp_header(respuesta, "retry-after")))
        min(if (is.na(espera)) 5 else espera, 20)
      }
    ) |>
    req_error(is_error = \(respuesta) FALSE)
  if (nzchar(OPENALEX_API_KEY)) solicitud <- req_auth_bearer_token(solicitud, OPENALEX_API_KEY)
  if (nzchar(OPENALEX_MAILTO)) solicitud <- req_url_query(solicitud, mailto = OPENALEX_MAILTO)

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
          identical(norm_doi(work$doi %||% NA_character_), doi_consulta)) {
        registro$estado <- "encontrado"
        registro$work <- work
      } else {
        registro$detalle <- "Respuesta invalida o DOI devuelto distinto al solicitado."
      }
    } else {
      registro$detalle <- str_c("HTTP ", registro$http_status, "; reejecutar.")
    }
  }
  saveRDS(registro, archivo_cache)
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

#' Ancla por ORCID: TODOS los author_id de OpenAlex que declaran ese ORCID.
#' OpenAlex a veces parte a una persona en varios perfiles con el mismo
#' ORCID (p. ej. Maria Paulina Castro); /authors/orcid/{id} devuelve solo
#' uno, por eso se usa el filtro de la lista de autores.
obtener_author_id_por_orcid <- function(orcid, mailto = OPENALEX_MAILTO) {
  vacio <- tibble(author_id = NA_character_, orcid_openalex = NA_character_,
                  works_count = NA_integer_)
  if (is.na(orcid) || orcid == "") return(vacio)

  url <- paste0("https://api.openalex.org/authors?per-page=50&filter=orcid:", orcid)

  solicitud <- request(url) |>
    req_user_agent("facso-openalex-autores/1.0") |>
    req_timeout(30) |>
    req_retry(
      max_tries = 3, max_seconds = 60, retry_on_failure = TRUE,
      is_transient = \(r) resp_status(r) %in% c(429L, 500L, 502L, 503L, 504L),
      after = \(r) {
        espera <- suppressWarnings(as.numeric(resp_header(r, "retry-after")))
        min(if (is.na(espera)) 5 else espera, 20)
      }
    ) |>
    req_error(is_error = \(r) FALSE)
  if (nzchar(OPENALEX_API_KEY)) solicitud <- req_auth_bearer_token(solicitud, OPENALEX_API_KEY)
  if (nzchar(mailto)) solicitud <- req_url_query(solicitud, mailto = mailto)

  resp <- tryCatch(req_perform(solicitud), error = function(e) NULL)
  if (is.null(resp) || resp_status(resp) != 200) return(vacio)

  datos <- tryCatch(
    resp_body_string(resp) |> fromJSON(flatten = TRUE),
    error = function(e) NULL
  )
  perfiles <- datos$results
  if (is.null(perfiles) || !is.data.frame(perfiles) || nrow(perfiles) == 0) return(vacio)

  tibble(
    author_id      = as.character(perfiles$id),
    orcid_openalex = str_remove(as.character(perfiles$orcid), "^https?://orcid\\.org/"),
    works_count    = as.integer(perfiles$works_count)
  ) |>
    # el filtro es exacto, pero se verifica igual
    filter(toupper(orcid_openalex) == toupper(orcid))
}

#' Recorre ORCID con guardado incremental y reanudacion (mismo patron que
#' pipeline_orcid_multi() en 02-orcid.R).
resolver_ancla_orcid <- function(orcids, pausa_seg = 0.3,
                                 archivo_parcial = ruta_temp("openalex-ancla-orcid-v2-parcial.rds")) {
  acumulado <- if (file.exists(archivo_parcial)) readRDS(archivo_parcial) else tibble()
  ya_hechos <- if (nrow(acumulado) > 0) unique(acumulado$id_orcid) else character()
  pendientes <- setdiff(orcids, ya_hechos)

  if (length(pendientes) == 0) return(acumulado)
  message("  ORCID pendientes (ancla OpenAlex): ", length(pendientes), " de ", length(orcids))

  for (i in seq_along(pendientes)) {
    orcid <- pendientes[i]
    message(sprintf("  [%d/%d] ORCID %s", i, length(pendientes), orcid))

    fila <- tryCatch(
      obtener_author_id_por_orcid(orcid) |> mutate(id_orcid = orcid, .before = 1),
      error = function(e) tibble(id_orcid = orcid)
    )
    acumulado <- bind_rows(acumulado, fila)
    saveRDS(acumulado, archivo_parcial)
    Sys.sleep(pausa_seg)
  }
  acumulado
}


## ---------------------------------------------------------------------
## 3. ANCLA POR ORCID
## ---------------------------------------------------------------------

consolidado <- leer_orcid()

ancla_crudo <- cache_incremental(
  ruta_temp("openalex-ancla-orcid.rds"), ruta_temp("openalex-ancla-orcid-v2-parcial.rds"),
  \() resolver_ancla_orcid(consolidado$id_orcid),
  claves = consolidado$id_orcid, columna = "id_orcid"
)

candidatos_ancla <- consolidado |>
  inner_join(ancla_crudo, by = "id_orcid") |>
  filter(!is.na(author_id)) |>
  distinct(rut, author_id, .keep_all = TRUE) |>
  transmute(rut, author_id, orcid_openalex,
            n_dois_evidencia = NA_integer_, n_dois_exactos = NA_integer_,
            metodo = "orcid", estado_perfil = "propuesto_orcid")

ruts_con_ancla <- unique(candidatos_ancla$rut)

message("  Ancla ORCID resuelta: ", length(ruts_con_ancla), " de ", nrow(consolidado),
        " academicos con ORCID conocido (", nrow(candidatos_ancla), " perfiles)")


## ---------------------------------------------------------------------
## 4. EVIDENCIA POR DOI (identificacion inversa), PARA TODOS LOS rut
## ---------------------------------------------------------------------
## Tambien para los rut con ancla: OpenAlex puede tener parte de la obra de
## una persona en perfiles sin ORCID (p. ej. Hector Morales, Francisca
## Concha). La seccion 7 decide como combinar ambas senales.

academicos <- read_xlsx(ruta_input("acad.xlsx"), col_types = "text") |>
  clean_names() |>
  mutate(
    rut = norm_rut(rut),
    across(c(nombres, paterno, materno), \(x) str_squish(coalesce(x, ""))),
    departamento = recodificar_departamento(reparticion),
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

publicaciones <- bind_rows(
  readRDS(ruta_temp("sepavid-publicaciones.rds")),
  readRDS(ruta_temp("orcid-publicaciones.rds"))
) |>
  filter(tipo_documento == "journal-article") |>
  transmute(rut = norm_rut(rut), anio = as.integer(as.character(anio)),
            doi_original = as.character(doi), doi = norm_doi(doi)) |>
  distinct()

articles_facso <- publicaciones |>
  inner_join(academicos, by = "rut", relationship = "many-to-many") |>
  mutate(
    doi_valido = coalesce(str_detect(doi, "^10\\.[0-9]{4,9}/[^\\s]+$"), FALSE),
    rut_valido = coalesce(str_detect(rut, "^[0-9]{9}[0-9K]$"), FALSE)
  ) |>
  distinct()

saveRDS(articles_facso, ruta_temp("openalex-articles-facso.rds"))
articles_facso |>
  filter(!doi_valido | !rut_valido | is.na(nombre_completo) | nombre_completo == "") |>
  write_excel_csv(ruta_output("openalex-insumos-por-revisar.csv"), na = "")

variantes_nombres <- articles_facso |>
  filter(rut_valido) |>
  select(rut, starts_with("nombre")) |>
  pivot_longer(-rut, names_to = "tipo_variante", values_to = "variante_original") |>
  mutate(variante = normalizar_nombre(variante_original)) |>
  filter(!is.na(variante), str_count(variante, "\\S+") >= 2L) |>
  distinct(rut, variante, .keep_all = TRUE)

dois <- articles_facso |>
  filter(doi_valido) |>
  distinct(doi) |>
  arrange(doi) |>
  pull(doi)

if (length(dois) == 0L) {
  message("  Sin DOI pendientes de evidencia (todos los rut sin ORCID ya cubiertos o sin articulos).")
  oa_perfiles_candidatos <- tibble(
    rut = character(), nombre_completo = character(), departamento = character(),
    author_id = character(), estado_perfil = character(), nombres_oa = character(),
    orcid = character(), n_dois_evidencia = integer(), n_dois_exactos = integer(),
    doi_evidencia = character(), metodos = character(), conflicto = logical()
  )
} else {

resultados_oa <- vector("list", length(dois))
n_completados <- length(dois)
for (i in seq_along(dois)) {
  resultados_oa[[i]] <- consultar_doi(dois[[i]])
  if (i == 1L || i %% 25L == 0L || i == length(dois)) {
    message(str_c("  DOI procesados (incluye cache): ", i, "/", length(dois)))
  }
  if (resultados_oa[[i]]$http_status %in% c(401L, 403L, 429L)) {
    # Cuota agotada: se DETIENE la corrida. Seguir con evidencia parcial
    # dejaria perfiles sin descubrir, y 02c cachearia las obras de ese
    # conjunto incompleto como si fuera definitivo. Lo consultado hasta aqui
    # queda cacheado por DOI (consultar_doi() cachea archivo por archivo),
    # asi que la proxima corrida retoma justo donde quedo, no desde cero.
    stop(str_c(
      "OpenAlex respondio HTTP ", resultados_oa[[i]]$http_status,
      " (cuota agotada). Quedan ", length(dois) - i + 1, " DOI pendientes. ",
      "Vuelve a correr proc-final.R mas tarde (o define OPENALEX_API_KEY en ",
      "~/.Renviron); lo ya consultado queda en cache."
    ), call. = FALSE)
  }
}
resultados_oa <- resultados_oa[seq_len(n_completados)]

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
write_excel_csv(estado_dois, ruta_output("openalex-estado-dois.csv"), na = "")


## ---------------------------------------------------------------------
## 5. COMPARACION DENTRO DE CADA DOI
## ---------------------------------------------------------------------

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


## ---------------------------------------------------------------------
## 6. CANDIDATOS POR rut x author_id
## ---------------------------------------------------------------------

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

} # fin del bloque `if (length(dois) == 0L)`


## ---------------------------------------------------------------------
## 7. DESAMBIGUACION ENTRE CANDIDATOS "propuesto_exacto" DEL MISMO rut
## ---------------------------------------------------------------------
## 7a. rut CON ancla ORCID: la ancla tiene prioridad, pero se aceptan
##     ademas los perfiles por evidencia de DOI que complementan la obra
##     (perfiles partidos de la misma persona, sin ORCID en OpenAlex). Un
##     perfil que declara un ORCID DISTINTO al conocido es otra persona
##     (caso Catalina Arteaga) y se descarta. Los perfiles que ya trae la
##     ancla no se repiten.
## 7b. rut SIN ancla: si 2+ candidatos "propuesto_exacto" declaran un ORCID
##     propio distinto entre si, no hay forma de decidir cual es la persona
##     real -> quedan para revision manual, no se usan en la etapa 02c.

orcid_conocido <- consolidado |> select(rut, orcid_conocido = id_orcid)

oa_perfiles_candidatos <- oa_perfiles_candidatos |>
  filter(!paste(rut, author_id) %in% paste(candidatos_ancla$rut, candidatos_ancla$author_id)) |>
  left_join(orcid_conocido, by = "rut") |>
  mutate(
    orcid_declarado = str_remove_all(orcid, "https?://orcid\\.org/"),
    orcid_distinto  = rut %in% ruts_con_ancla &
                      map2_lgl(orcid_declarado, orcid_conocido, \(d, k) {
                        !is.na(d) && !is.na(k) && any(str_split_1(d, ";\\s*") != k)
                      }),
    estado_perfil = if_else(orcid_distinto, "descartado_orcid_distinto", estado_perfil)
  ) |>
  select(-orcid_conocido, -orcid_declarado, -orcid_distinto)

message("  Perfiles por evidencia DOI que complementan la ancla: ",
        sum(oa_perfiles_candidatos$rut %in% ruts_con_ancla &
              oa_perfiles_candidatos$estado_perfil == "propuesto_exacto"),
        " (descartados por ORCID distinto: ",
        sum(oa_perfiles_candidatos$estado_perfil == "descartado_orcid_distinto"), ")")

propuestos_exactos <- oa_perfiles_candidatos |>
  filter(estado_perfil == "propuesto_exacto", !rut %in% ruts_con_ancla)

orcid_distintos_por_rut <- propuestos_exactos |>
  filter(!is.na(orcid)) |>
  distinct(rut, orcid) |>
  count(rut, name = "n_orcid_distintos") |>
  filter(n_orcid_distintos > 1)

oa_perfiles_candidatos <- oa_perfiles_candidatos |>
  mutate(estado_perfil = if_else(
    estado_perfil == "propuesto_exacto" & rut %in% orcid_distintos_por_rut$rut,
    "revisar_orcid_distinto",
    estado_perfil
  ))

if (nrow(orcid_distintos_por_rut) > 0) {
  message("  ADVERTENCIA: ", nrow(orcid_distintos_por_rut), " rut con candidatos 'exactos' ",
          "que declaran ORCID propio distinto entre si (sin ORCID conocido para desempatar). ",
          "Quedan como revisar_orcid_distinto, ver input/temp/oa-perfiles-candidatos.rds.")
}

saveRDS(oa_perfiles_candidatos, ruta_temp("oa-perfiles-candidatos.rds"))


## ---------------------------------------------------------------------
## 8. SALIDA FINAL: acad-openalex.rds
## ---------------------------------------------------------------------
## Ancla ORCID + inclusiones manuales + evidencia de DOI ya desambiguada
## (seccion 7). Un rut puede tener mas de un author_id "aceptado" (perfil
## partido de la misma persona): eso es intencional, la etapa 02c consulta
## todos los author_id de un rut y deduplica por DOI. Si un mismo perfil
## llega por mas de una via, se conserva la primera (ancla > manual >
## evidencia), asi una inclusion manual prevalece sobre un descarte
## automatico (p. ej. "descartado_orcid_distinto").

acad_openalex <- bind_rows(
  candidatos_ancla,
  inclusiones_manuales |>
    transmute(rut, author_id, orcid_openalex = NA_character_,
              n_dois_evidencia = NA_integer_, n_dois_exactos = NA_integer_,
              metodo = "manual", estado_perfil = "propuesto_manual"),
  oa_perfiles_candidatos |>
    filter(estado_perfil %in% c("propuesto_exacto", "revisar_conflicto",
                                "revisar_nombre", "revisar_orcid_distinto")) |>
    transmute(rut, author_id,
              # orcid trae el formato URL completo (viene de autoria$author$orcid
              # sin procesar); se normaliza para que calce con el formato de
              # orcid.csv y con las filas resueltas por ancla.
              orcid_openalex = str_remove(orcid, "^https?://orcid\\.org/"),
              n_dois_evidencia, n_dois_exactos,
              metodo = metodos, estado_perfil)
) |>
  distinct(rut, author_id, .keep_all = TRUE)

saveRDS(acad_openalex, ruta_temp("acad-openalex.rds"))


## ---------------------------------------------------------------------
## 8b. ORCID SUGERIDOS (para completar orcid.csv a mano)
## ---------------------------------------------------------------------
## El pipeline nunca agrega ORCID por su cuenta: orcid.csv es la unica
## fuente. Si un perfil aceptado por evidencia de DOI declara un ORCID y el
## academico aun no tiene uno en orcid.csv, se deja como sugerencia. Al
## copiarlo a orcid.csv, la proxima corrida lo usa como ancla (y 02-orcid.R
## descarga sus libros y capitulos).

orcid_sugeridos <- acad_openalex |>
  filter(estado_perfil == "propuesto_exacto", !is.na(orcid_openalex),
         !rut %in% consolidado$rut,
         !orcid_openalex %in% consolidado$id_orcid) |>
  left_join(readRDS(ruta_temp("acad.rds")) |> select(rut, nombre_completo, departamento),
            by = "rut") |>
  transmute(rut, nombre_completo, departamento, id_orcid_sugerido = orcid_openalex,
            author_id, n_dois_exactos) |>
  arrange(departamento, nombre_completo)

write_excel_csv(orcid_sugeridos, ruta_output("orcid-sugeridos.csv"), na = "")


## ---------------------------------------------------------------------
## 9. VERIFICACIONES
## ---------------------------------------------------------------------

aceptados <- acad_openalex |> filter(estado_perfil %in% ESTADOS_PERFIL_ACEPTADOS)

message("\n  --- Resumen 02b-openalex-autores.R ---")
message("  Academicos con author_id aceptado : ", n_distinct(aceptados$rut), " de 206")
message("    - via ancla ORCID   : ", n_distinct(aceptados$rut[aceptados$estado_perfil == "propuesto_orcid"]))
message("    - via evidencia DOI : ", n_distinct(aceptados$rut[aceptados$estado_perfil == "propuesto_exacto"]))
message("    - via inclusion manual: ", n_distinct(aceptados$rut[aceptados$estado_perfil == "propuesto_manual"]))
message("  Rut con >1 author_id aceptado (perfiles partidos)      : ",
        sum(table(aceptados$rut) > 1))
message("  Rut para revision manual (conflicto/ambiguo/sin ORCID) : ",
        n_distinct(acad_openalex$rut[!acad_openalex$estado_perfil %in% ESTADOS_PERFIL_ACEPTADOS]))
print(count(acad_openalex, estado_perfil))
