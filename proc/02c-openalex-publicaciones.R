# =====================================================================
# 02c-openalex-publicaciones.R  |  Etapa 2c
#
# Descubre articulos de revista de cada academico via OpenAlex, usando el
# cruce rut -> author_id de la etapa 02b. Complementa a 02-orcid.R: mientras
# ORCID cubre libros/capitulos autorreportados (ver 02-orcid.R), esta etapa
# cubre articulos para los academicos que 02-orcid.R no alcanza porque no
# tienen ORCID conocido -- y para el resto, articulos que OpenAlex indexa y
# el registro ORCID de esa persona no.
#
# Alcance acotado a journal-article, no a libros/capitulos: OpenAlex los
# indexa peor que ORCID (dependen de DOI/registro en Crossref, el
# autorreporte en ORCID no), y proc-final.R solo valida libros contra
# orcid-libros.rds -- un libro nuevo encontrado solo aqui quedaria
# igualmente filtrado al final. Esa funcion se queda exclusivamente en
# 02-orcid.R.
#
# Entradas : input/temp/acad.rds             (etapa 1)
#            input/temp/acad-openalex.rds    (etapa 02b; rut -> author_id)
#            input/temp/sepavid-publicaciones.rds (etapa 1)
#            input/temp/orcid-publicaciones.rds   (etapa 2)
# Salidas  : input/temp/openalex-publicaciones-crudo.rds
#            input/temp/openalex-publicaciones.rds
#
# API consultada (publica, sin credenciales; OPENALEX_MAILTO opcional):
#   OpenAlex  https://api.openalex.org/works
# =====================================================================


## ---------------------------------------------------------------------
## 1. CONSULTA A OPENALEX: ARTICULOS DE UN CONJUNTO DE author_id
## ---------------------------------------------------------------------
## Un rut puede tener mas de un author_id (perfiles legitimamente partidos
## por OpenAlex): OpenAlex permite combinarlos con "|" en un mismo filtro,
## asi que se cubren todos con una sola serie de consultas paginadas.

obtener_obras_openalex <- function(author_ids, mailto = OPENALEX_MAILTO) {
  vacio <- tibble(titulo = character(), tipo = character(),
                  anio = integer(), doi = character(),
                  revista = character(), issn = character())
  ids_limpios <- author_ids |> str_extract("A[0-9]+$") |> unique() |> na.omit()
  if (length(ids_limpios) == 0) return(vacio)

  filtro_autor <- paste(ids_limpios, collapse = "|")
  paginas <- list()
  cursor <- "*"

  repeat {
    url <- paste0("https://api.openalex.org/works?filter=author.id:", filtro_autor,
                  ",type:article&per_page=200&cursor=", utils::URLencode(cursor, reserved = TRUE))
    if (!identical(mailto, "")) url <- paste0(url, "&mailto=", mailto)

    encabezados <- if (nzchar(OPENALEX_API_KEY)) {
      add_headers(Authorization = paste("Bearer", OPENALEX_API_KEY))
    } else {
      NULL
    }
    resp <- tryCatch(GET(url, encabezados), error = function(e) NULL)
    if (!is.null(resp) && status_code(resp) %in% c(401L, 403L, 429L)) {
      # Cuota agotada: se marca para que consultar_openalex_publicaciones()
      # corte el lote entero, en vez de guardar este rut como "sin obras"
      # (falso negativo que lo dejaria mal marcado como ya revisado).
      attr(vacio, "agotado") <- TRUE
      return(vacio)
    }
    if (is.null(resp) || status_code(resp) != 200) break

    datos <- tryCatch(
      content(resp, as = "text", encoding = "UTF-8") |> fromJSON(flatten = TRUE),
      error = function(e) NULL
    )
    if (is.null(datos) || is.null(datos$results) || length(datos$results) == 0 ||
        !is.data.frame(datos$results)) break

    resultados <- datos$results
    n_res <- nrow(resultados)

    # issn viene como lista (una revista puede declarar ISSN impreso y
    # electronico); se colapsa igual que en norm_issn()/03-id-revistas.R,
    # que espera un string separado por ";". No siempre viene primary_location
    # (obras sin fuente identificada), asi que se resuelve con valor por
    # defecto si la columna no existe en esta pagina.
    col_issn <- "primary_location.source.issn"
    issn_col <- if (col_issn %in% names(resultados)) {
      map_chr(resultados[[col_issn]], safe_paste)
    } else {
      rep(NA_character_, n_res)
    }
    col_revista <- "primary_location.source.display_name"
    revista_col <- if (col_revista %in% names(resultados)) {
      safe_chr_vec(resultados[[col_revista]])
    } else {
      rep(NA_character_, n_res)
    }

    paginas[[length(paginas) + 1]] <- tibble(
      titulo  = safe_chr_vec(resultados$title),
      tipo    = safe_chr_vec(resultados$type),
      anio    = suppressWarnings(as.integer(resultados$publication_year)),
      doi     = norm_doi(resultados$doi),
      revista = revista_col,
      issn    = issn_col
    )

    cursor_siguiente <- datos$meta$next_cursor
    if (is.null(cursor_siguiente) || is.na(cursor_siguiente) || nrow(datos$results) < 200) break
    cursor <- cursor_siguiente
    Sys.sleep(0.2)
  }

  if (length(paginas) == 0) return(vacio)
  bind_rows(paginas) |> distinct(doi, .keep_all = TRUE)
}

#' Version vectorizada de safe_chr() (00-funciones.R), para columnas ya
#' aplanadas por fromJSON(flatten = TRUE) en vez de listas por fila.
safe_chr_vec <- function(x) {
  if (is.null(x)) return(NA_character_)
  as.character(x)
}

#' Recorre los rut con guardado incremental y reanudacion (mismo patron que
#' consultar_openalex_autores() en 07-coautores.R).
consultar_openalex_publicaciones <- function(autores_por_rut, pausa_seg = 0.5,
                                             archivo_parcial = ruta_temp("openalex-publicaciones-parcial.rds")) {
  acumulado <- if (file.exists(archivo_parcial)) readRDS(archivo_parcial) else tibble()
  ya_hechos <- if (nrow(acumulado) > 0) unique(acumulado$rut) else character()
  pendientes <- setdiff(names(autores_por_rut), ya_hechos)

  if (length(pendientes) == 0) return(acumulado)
  message("  RUT pendientes (obras OpenAlex): ", length(pendientes), " de ", length(autores_por_rut))

  for (i in seq_along(pendientes)) {
    rut <- pendientes[i]
    message(sprintf("  [%d/%d] RUT %s", i, length(pendientes), rut))

    resultado <- tryCatch(
      obtener_obras_openalex(autores_por_rut[[rut]]),
      error = function(e) {
        message("    -> error: ", conditionMessage(e))
        tibble()
      }
    )

    # Cuota agotada: se corta el lote entero (este rut NO se marca como
    # hecho, para que se reintente de verdad en la proxima corrida).
    if (isTRUE(attr(resultado, "agotado"))) {
      message("  ADVERTENCIA: OpenAlex devolvio 401/403/429 (cuota probablemente ",
              "agotada). Se corta el lote: quedan ", length(pendientes) - i + 1,
              " rut pendientes para la proxima corrida.")
      break
    }

    fila <- resultado |> mutate(rut = rut, .before = 1)
    if (nrow(fila) == 0) fila <- tibble(rut = rut)  # centinela: no reintentar en corridas futuras

    acumulado <- bind_rows(acumulado, fila)
    saveRDS(acumulado, archivo_parcial)
    Sys.sleep(pausa_seg)
  }
  acumulado
}


## ---------------------------------------------------------------------
## 2. DESCARGA (con cache)
## ---------------------------------------------------------------------

acad_openalex <- readRDS(ruta_temp("acad-openalex.rds")) |>
  filter(estado_perfil %in% c("propuesto_orcid", "propuesto_exacto"))

autores_por_rut <- split(acad_openalex$author_id, acad_openalex$rut)

openalex_crudo <- usar_cache(
  ruta_temp("openalex-publicaciones-crudo.rds"),
  consultar_openalex_publicaciones(autores_por_rut)
)


## ---------------------------------------------------------------------
## 3. FILTRO Y DEDUPLICACION CONTRA SEPAVID + ORCID
## ---------------------------------------------------------------------
## Traslape esperado con 02-orcid.R para academicos con ORCID conocido
## (OpenAlex ingiere ORCID como una de sus fuentes): se descartan DOI ya
## presentes en cualquiera de las dos, no solo en SEPAVID, para no
## reintroducir como "nuevo" algo que 02-orcid.R ya capturo.

acad <- readRDS(ruta_temp("acad.rds"))

dois_conocidos <- c(
  readRDS(ruta_temp("sepavid-publicaciones.rds"))$doi,
  readRDS(ruta_temp("orcid-publicaciones.rds"))$doi
) |> norm_doi() |> na.omit() |> unique()

openalex_publicaciones <- openalex_crudo |>
  filter(!is.na(titulo), !is.na(doi), !doi %in% dois_conocidos) |>
  mutate(tipo_documento = "journal-article") |>
  distinct(doi, .keep_all = TRUE) |>
  inner_join(acad, by = "rut") |>
  filter(jerarquia %in% JERARQUIAS_VALIDAS,
         between(anio, ANIO_INICIO, ANIO_FIN)) |>
  transmute(
    titulo, revista, anio, doi, tipo_documento, issn,
    rut, nombre_completo, sexo, edad, horas_reales,
    reparticion, departamento, jerarquia
  )

saveRDS(openalex_publicaciones, ruta_temp("openalex-publicaciones.rds"))


## ---------------------------------------------------------------------
## 4. VERIFICACIONES
## ---------------------------------------------------------------------

message("  RUT con obras recuperadas de OpenAlex (bruto) : ", n_distinct(openalex_crudo$rut))
message("  Articulos nuevos (no en SEPAVID ni ORCID)      : ", nrow(openalex_publicaciones))
message("  Academicos que aportan al menos un articulo nuevo : ",
        n_distinct(openalex_publicaciones$rut))
