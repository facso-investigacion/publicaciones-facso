# =====================================================================
# 07-coautores.R  |  Etapa 7 (posterior a 01-06; depende de 02a, 02b y 03)
#
# Recupera, via OpenAlex, el listado COMPLETO de autores de cada publicacion
# (FACSO y externos) con su afiliacion institucional y pais. Es el insumo
# para redes de coautoria y de colaboracion internacional: base_final y
# consolidado_wide solo traen los coautores que son planta FACSO (via RUT),
# porque SEPAVID no registra a nadie mas.
#
# Entradas : input/temp/base-consolidada.rds     (etapa 3; doi -> clave_pub,
#                                                  y el rut/nombre_completo
#                                                  FACSO ya conocido de cada
#                                                  pub)
#            input/temp/acad-openalex.rds        (etapa 02b; rut <-> author_id)
#            input/temp/acad-orcid-consolidado.rds (etapa 02a; rut <-> id_orcid)
# Salidas  : input/temp/coautores.rds  (una fila por autor x publicacion,
#                                       FACSO y externos)
#
# API consultada (publica, sin credenciales):
#   OpenAlex  https://api.openalex.org/works/
#
# Cobertura: solo se puede consultar por DOI. Los articulos casi siempre lo
# tienen; libros y capitulos, no siempre (ver mensaje final). Las
# publicaciones sin DOI quedan sin fila en coautores.rds.
# =====================================================================


## ---------------------------------------------------------------------
## 1. CONSULTA A OPENALEX: AUTORES DE UNA OBRA
## ---------------------------------------------------------------------

# OPENALEX_MAILTO vive en 00-funciones.R (la usa tambien la etapa 02b).

obtener_openalex_autores <- function(doi, mailto = OPENALEX_MAILTO) {
  vacio <- tibble(orden_autor = integer(), nombre_autor = character(),
                  orcid_autor = character(), author_id_autor = character(),
                  institucion = character(),
                  pais_iso = character(), es_corresponding = logical())
  if (is.na(doi)) return(vacio)

  url <- paste0("https://api.openalex.org/works/https://doi.org/", doi)

  # Reintentos con backoff ante 429/5xx (mismo patron que consultar_doi() en
  # 02b-openalex-autores.R): sin esto, una racha de rate-limit en una corrida
  # larga se traduce en cientos de filas vacias silenciosas, no en un error
  # visible.
  #
  # `after` tope la espera a 20s: httr2 por defecto respeta el header
  # Retry-After tal cual venga, y OpenAlex puede pedir horas de espera
  # cuando el presupuesto de creditos del tier gratuito esta agotado (no es
  # un limite por segundo que valga la pena reintentar). Sin este tope,
  # max_seconds NO protege: solo se chequea antes de decidir un reintento,
  # no limita el sleep de un reintento ya en curso.
  solicitud <- request(url) |>
    req_user_agent("facso-coautores/1.0") |>
    req_timeout(45) |>
    req_retry(
      max_tries = 5,
      max_seconds = 180,
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
  if (!identical(mailto, "")) solicitud <- req_url_query(solicitud, mailto = mailto)

  respuesta <- tryCatch(req_perform(solicitud), error = function(e) NULL)
  if (is.null(respuesta) || resp_status(respuesta) != 200L) {
    # Estado agotado (401/403/429, incluso tras los reintentos con tope de
    # 20s): se marca en un atributo para que consultar_openalex_autores()
    # corte el lote entero en vez de seguir intentando DOI por DOI durante
    # horas sin avanzar -- son miles de DOI y cada intento fallido igual
    # cuesta ~20-60s de reintentos.
    if (!is.null(respuesta) && resp_status(respuesta) %in% c(401L, 403L, 429L)) {
      attr(vacio, "agotado") <- TRUE
    }
    return(vacio)
  }

  # fromJSON() con su simplifyVector = TRUE por defecto (no resp_body_json():
  # el resto de esta funcion espera `authorships` como data.frame, no como
  # lista anidada).
  datos <- tryCatch(
    resp_body_string(respuesta) |> fromJSON(flatten = FALSE),
    error = function(e) NULL
  )
  aut <- datos$authorships
  if (is.null(aut) || !is.data.frame(aut) || nrow(aut) == 0) return(vacio)

  map_dfr(seq_len(nrow(aut)), function(i) {
    nombre <- safe_chr(aut$raw_author_name[i])
    if (is.na(nombre)) nombre <- safe_chr(aut$author$display_name[i])

    orcid_raw <- aut$author$orcid[i]
    orcid <- if (is.na(orcid_raw)) NA_character_
             else str_remove(orcid_raw, "^https?://orcid\\.org/")

    # institucion y pais_iso se guardan alineados (misma cantidad de
    # elementos, mismo orden: institucion[k] va con pais_iso[k]) tomando
    # ambos del mismo data.frame de instituciones -- exportar-bibliometrix.R
    # arma un fragmento de C1 por cada par institucion-pais, no uno solo por
    # autor, porque bibliometrix::AU_UN()/AU_CO() esperan un tag "[AUTOR]"
    # repetido por cada afiliacion cuando un autor tiene mas de una.
    inst <- aut$institutions[[i]]
    if (is.data.frame(inst) && nrow(inst) > 0) {
      institucion <- safe_paste(inst$display_name)
      pais_iso    <- safe_paste(inst$country_code)
    } else {
      institucion <- NA_character_
      pais_iso    <- NA_character_
    }

    tibble(
      orden_autor      = i,
      nombre_autor     = nombre,
      orcid_autor      = orcid,
      author_id_autor  = safe_chr(aut$author$id[i]),
      institucion      = institucion,
      pais_iso         = pais_iso,
      es_corresponding = isTRUE(aut$is_corresponding[i])
    )
  })
}

#' Consulta una lista de DOI con guardado incremental y reanudacion (mismo
#' patron que consultar_scopus() en 06-scopus.R).
consultar_openalex_autores <- function(dois, pausa_seg = 1,
                                       archivo_parcial = ruta_temp("openalex-coautores-parcial.rds")) {

  acumulado <- if (file.exists(archivo_parcial)) readRDS(archivo_parcial) else tibble()
  ya_hechos <- if (nrow(acumulado) > 0) unique(acumulado$doi) else character()
  pendientes <- setdiff(dois, ya_hechos)

  if (length(pendientes) == 0) return(acumulado)
  message("  DOI pendientes en OpenAlex (autores): ", length(pendientes), " de ", length(dois))

  for (i in seq_along(pendientes)) {
    doi <- pendientes[i]
    message(sprintf("  [%d/%d] DOI %s", i, length(pendientes), doi))

    resultado <- tryCatch(
      obtener_openalex_autores(doi),
      error = function(e) {
        message("    -> error: ", conditionMessage(e))
        tibble()
      }
    )

    # Cuota agotada (401/403/429 persistente, incluso tras reintentos): se
    # corta el lote entero en vez de seguir DOI por DOI durante horas sin
    # avanzar. Este DOI NO se marca como hecho (no se guarda fila centinela
    # para el), asi que se reintenta de verdad en la proxima corrida en vez
    # de quedar erroneamente como "ya revisado, sin autores".
    if (isTRUE(attr(resultado, "agotado"))) {
      message("  ADVERTENCIA: OpenAlex devolvio 401/403/429 persistente (cuota ",
              "probablemente agotada). Se corta el lote: quedan ",
              length(pendientes) - i + 1, " DOI pendientes para la proxima ",
              "corrida (lo ya avanzado no se pierde, queda en ", archivo_parcial, ").")
      break
    }

    # Fila centinela si la obra no trajo autores (o fallo la consulta): sin
    # ella, el DOI se reintentaria en cada corrida futura.
    fila <- resultado |> mutate(doi = doi, .before = 1)
    if (nrow(fila) == 0) fila <- tibble(doi = doi)

    acumulado <- bind_rows(acumulado, fila)
    saveRDS(acumulado, archivo_parcial)   # checkpoint
    Sys.sleep(pausa_seg)
  }

  acumulado
}


## ---------------------------------------------------------------------
## 2. DESCARGA (con cache)
## ---------------------------------------------------------------------

base_consolidada <- readRDS(ruta_temp("base-consolidada.rds"))

dois <- base_consolidada$doi |> na.omit() |> unique()

autores_openalex_crudo <- usar_cache(
  ruta_temp("openalex-coautores-crudo.rds"),
  consultar_openalex_autores(dois)
)


## ---------------------------------------------------------------------
## 3. clave_pub Y CANDIDATOS FACSO POR PUBLICACION
## ---------------------------------------------------------------------
## clave_pub se deriva del doi cuando existe, asi que doi -> clave_pub es
## una relacion uno a uno: alcanza con un distinct().

doi_a_clave <- base_consolidada |>
  filter(!is.na(doi)) |>
  distinct(doi, clave_pub)

# quitar_tildes() vive en 00-funciones.R (la usan tambien las etapas 02a y 02b).

# Nombres FACSO ya conocidos con certeza (via RUT) para cada clave_pub: el
# candidato al que se compara cada autor de OpenAlex, acotado a su propia
# publicacion (nunca una busqueda global). Se guardan los 2 apellidos por
# separado (no concatenados) porque las publicaciones internacionales suelen
# mostrar solo el paterno (p. ej. "Rodrigo A. Asún" en vez de "Rodrigo Asún
# Inostroza"): exigir que calcen los 2 juntos dejaba fuera la mayoria de los
# casos reales.
facso_por_pub <- base_consolidada |>
  distinct(clave_pub, rut, nombre_completo) |>
  filter(!is.na(nombre_completo)) |>
  mutate(
    apellidos = map(nombre_completo, ~str_split(partir_nombre(.x)$apellidos, " ")[[1]]),
    apellido1 = quitar_tildes(str_to_upper(map_chr(apellidos, 1))),
    apellido2 = quitar_tildes(str_to_upper(map_chr(apellidos, ~if (length(.x) > 1) .x[2] else NA_character_)))
  ) |>
  select(-apellidos)


## ---------------------------------------------------------------------
## 4. RESOLUCION DE es_facso / rut POR AUTOR
## ---------------------------------------------------------------------
## Metodo principal: apellidos, acotado a los 1-4 candidatos FACSO de esa
## misma publicacion (RUT es la llave consistente en todo el pipeline). Se
## considera match si CUALQUIERA de los 2 apellidos oficiales aparece como
## palabra completa en el nombre que reporta OpenAlex (sin tildes, por si
## una fuente las trae y la otra no).
## Complementos, en orden de fuerza: author_id (etapa 02b: si el author.id
## que trae la autoria calza con el cruce rut<->author_id ya resuelto, es
## una senal mas fuerte que el apellido porque no depende de texto) y ORCID
## (solo cuando el academico tiene id_orcid conocido Y OpenAlex trajo orcid
## para esa fila -- la cobertura de ORCID en la planta es baja, por eso no
## es el mecanismo principal).

autores_openalex <- autores_openalex_crudo |>
  filter(!is.na(nombre_autor)) |>
  left_join(doi_a_clave, by = "doi") |>
  filter(!is.na(clave_pub)) |>
  mutate(nombre_autor_norm = quitar_tildes(str_to_upper(nombre_autor)),
         id_autor = row_number())

match_nombre <- autores_openalex |>
  inner_join(facso_por_pub, by = "clave_pub", relationship = "many-to-many") |>
  filter(
    str_detect(nombre_autor_norm, paste0("\\b", apellido1, "\\b")) |
    (!is.na(apellido2) & str_detect(nombre_autor_norm, paste0("\\b", apellido2, "\\b")))
  ) |>
  distinct(id_autor, .keep_all = TRUE) |>          # un autor de OpenAlex, un solo FACSO asignado
  distinct(clave_pub, rut, .keep_all = TRUE) |>     # un mismo FACSO no se asigna 2 veces en la misma pub
  transmute(id_autor, rut_nombre = rut)

acad_openalex <- readRDS(ruta_temp("acad-openalex.rds")) |>
  filter(estado_perfil %in% c("propuesto_orcid", "propuesto_exacto")) |>
  distinct(rut, author_id)

autores_openalex <- autores_openalex |>
  mutate(author_id_norm = str_extract(author_id_autor, "A[0-9]+$"))

match_author_id <- autores_openalex |>
  filter(!is.na(author_id_norm)) |>
  inner_join(
    acad_openalex |> mutate(author_id_norm = str_extract(author_id, "A[0-9]+$")),
    by = "author_id_norm", relationship = "many-to-many"
  ) |>
  distinct(id_autor, .keep_all = TRUE) |>
  transmute(id_autor, rut_author_id = rut)

acad_orcid <- readRDS(ruta_temp("acad-orcid-consolidado.rds")) |>
  filter(!is.na(id_orcid)) |>
  distinct(rut, id_orcid)

autores_sin_match <- autores_openalex |>
  left_join(match_nombre,     by = "id_autor") |>
  left_join(match_author_id,  by = "id_autor") |>
  filter(is.na(rut_nombre), is.na(rut_author_id), !is.na(orcid_autor))

match_orcid <- autores_sin_match |>
  inner_join(acad_orcid, by = c("orcid_autor" = "id_orcid")) |>
  distinct(id_autor, .keep_all = TRUE) |>
  transmute(id_autor, rut_orcid = rut)

coautores <- autores_openalex |>
  left_join(match_nombre,    by = "id_autor") |>
  left_join(match_author_id, by = "id_autor") |>
  left_join(match_orcid,     by = "id_autor") |>
  mutate(
    # author_id es la senal mas fuerte (no depende de texto); nombre y
    # orcid quedan como respaldo cuando no hay cruce de author_id.
    rut          = coalesce(rut_author_id, rut_nombre, rut_orcid),
    metodo_match = case_when(
      !is.na(rut_author_id) ~ "author_id",
      !is.na(rut_nombre) & !is.na(rut_orcid) ~ "nombre+orcid",
      !is.na(rut_nombre)                     ~ "nombre",
      !is.na(rut_orcid)                      ~ "orcid",
      .default = NA_character_
    ),
    es_facso = !is.na(rut)
  ) |>
  transmute(clave_pub, doi, orden_autor, nombre_autor, orcid_autor,
            es_facso, rut, institucion, pais_iso, es_corresponding, metodo_match)

saveRDS(coautores, ruta_temp("coautores.rds"))


## ---------------------------------------------------------------------
## 5. VERIFICACIONES
## ---------------------------------------------------------------------

message("  DOI con autores recuperados de OpenAlex : ",
        n_distinct(coautores$clave_pub), " de ", length(dois), " DOI consultados")
message("  Filas autor x publicacion (coautores)    : ", nrow(coautores))
message("  Autores marcados como FACSO               : ", sum(coautores$es_facso))
print(count(coautores, metodo_match))

# Contraste de calidad: cuantos rut FACSO conocidos por base_consolidada no
# aparecieron en el listado OpenAlex de su propia publicacion (o no calzaron
# por apellido/orcid). Ayuda a detectar problemas de matching, no solo de
# cobertura por DOI.
ruts_openalex <- coautores |> filter(es_facso) |> distinct(clave_pub, rut)
ruts_esperados <- facso_por_pub |> filter(clave_pub %in% coautores$clave_pub) |>
  distinct(clave_pub, rut)
faltantes <- anti_join(ruts_esperados, ruts_openalex, by = c("clave_pub", "rut"))
message("  RUT FACSO esperados pero no calzados en su publicacion: ",
        nrow(faltantes), " de ", nrow(ruts_esperados))
