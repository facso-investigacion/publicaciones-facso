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
# autorreporte en ORCID no) y su tipo documental es menos confiable. La
# recuperacion de libros/capitulos se queda en SEPAVID y 02-orcid.R.
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
  # Cada rut se consulta con una "firma" (sus author_id ordenados): si en
  # una corrida posterior cambian sus perfiles aceptados (02b), la firma
  # cambia y el rut se vuelve a consultar, en vez de quedar con las obras de
  # los perfiles viejos.
  firmas <- map_chr(autores_por_rut, \(ids) paste(sort(unique(ids)), collapse = "|"))
  acumulado <- if (file.exists(archivo_parcial)) readRDS(archivo_parcial) else tibble()
  if (nrow(acumulado) > 0 && !"firma" %in% names(acumulado)) {
    # checkpoint anterior a las firmas: se asume que corresponde a los
    # perfiles vigentes (migracion de una sola vez)
    acumulado$firma <- unname(firmas[acumulado$rut])
  }
  ya_hechos <- if (nrow(acumulado) > 0) unique(paste(acumulado$rut, acumulado$firma)) else character()
  pendientes <- names(autores_por_rut)[!paste(names(autores_por_rut), firmas) %in% ya_hechos]
  if (nrow(acumulado) > 0) acumulado <- acumulado |> filter(!rut %in% pendientes)   # descarta obras de firmas viejas

  if (length(pendientes) == 0) return(acumulado |> filter(paste(rut, firma) %in% paste(names(firmas), firmas)))
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

    # Cuota agotada: se DETIENE la corrida (este rut NO se marca como hecho).
    # Devolver el lote incompleto haria que se guardara como
    # openalex-publicaciones-crudo.rds definitivo; el avance ya quedo en
    # archivo_parcial, asi que la proxima corrida retoma desde aqui.
    if (isTRUE(attr(resultado, "agotado"))) {
      stop("OpenAlex devolvio 401/403/429 (cuota agotada). Quedan ",
           length(pendientes) - i + 1, " rut pendientes. Vuelve a correr ",
           "proc-final.R mas tarde (o define OPENALEX_API_KEY en ~/.Renviron); ",
           "el avance queda en ", archivo_parcial, ".", call. = FALSE)
    }

    firma_rut <- firmas[[rut]]   # fuera de mutate(): ahi `rut` seria la columna
    fila <- resultado |> mutate(rut = rut, firma = firma_rut, .before = 1)
    if (nrow(fila) == 0) fila <- tibble(rut = rut, firma = firma_rut)  # centinela: no reintentar en corridas futuras

    acumulado <- bind_rows(acumulado, fila)
    saveRDS(acumulado, archivo_parcial)
    Sys.sleep(pausa_seg)
  }
  acumulado |> filter(paste(rut, firma) %in% paste(names(firmas), firmas))
}


## ---------------------------------------------------------------------
## 2. DESCARGA (con cache)
## ---------------------------------------------------------------------

acad_openalex <- readRDS(ruta_temp("acad-openalex.rds")) |>
  filter(estado_perfil %in% ESTADOS_PERFIL_ACEPTADOS)

autores_por_rut <- split(acad_openalex$author_id, acad_openalex$rut)

openalex_crudo <- cache_incremental(
  ruta_temp("openalex-publicaciones-crudo.rds"), ruta_temp("openalex-publicaciones-parcial.rds"),
  \() consultar_openalex_publicaciones(autores_por_rut),
  claves = names(autores_por_rut), columna = "rut"
)


## ---------------------------------------------------------------------
## 3. FILTRO Y DEDUPLICACION CONTRA SEPAVID + ORCID
## ---------------------------------------------------------------------
## Traslape esperado con 02-orcid.R para academicos con ORCID conocido
## (OpenAlex ingiere ORCID como una de sus fuentes): se descartan DOI ya
## presentes en cualquiera de las dos, no solo en SEPAVID, para no
## reintroducir como "nuevo" algo que 02-orcid.R ya capturo.
## El descarte es por par (rut, doi), no por DOI solo: un articulo que
## SEPAVID atribuye a un coautor FACSO pero no a este academico (p. ej.
## Marambio en 10.22380/2539472x.2930) debe seguir sumando para este rut.

acad <- readRDS(ruta_temp("acad.rds"))

pares_conocidos <- bind_rows(
  readRDS(ruta_temp("sepavid-publicaciones.rds")) |> select(rut, doi),
  readRDS(ruta_temp("orcid-publicaciones.rds"))   |> select(rut, doi)
) |>
  mutate(doi = norm_doi(doi)) |>
  filter(!is.na(doi)) |>
  distinct()

# Obras de homonimos dentro de un perfil MIXTO de OpenAlex (que tambien
# tiene obras reales del academico, por lo que no se puede excluir entero
# en exclusiones_manuales de 02b). Se excluyen obra por obra.
exclusiones_obras <- tribble(
  ~rut,          ~doi,                             ~motivo,
  "0091297612",  "10.3389/fpls.2021.679059",       "Hector Morales: genetica de ciruelos (homonimo en perfil mixto A5064024428)",
  "0091297612",  "10.3389/fpls.2022.805744",       "Hector Morales: genetica de ciruelos (homonimo en perfil mixto A5064024428)",
  "0091297612",  "10.1016/j.scienta.2024.113798",  "Hector Morales: genetica de ciruelos (homonimo en perfil mixto A5064024428)",
  "0091297612",  "10.1108/jefas-07-2021-0113",     "Hector Morales: ley de Benford en auditoria (homonimo en perfil mixto A5064024428)"
)

openalex_publicaciones <- openalex_crudo |>
  filter(!is.na(titulo), !is.na(doi)) |>
  anti_join(exclusiones_obras, by = c("rut", "doi")) |>
  anti_join(pares_conocidos, by = c("rut", "doi")) |>
  mutate(tipo_documento = "journal-article") |>
  distinct(rut, doi, .keep_all = TRUE) |>
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
