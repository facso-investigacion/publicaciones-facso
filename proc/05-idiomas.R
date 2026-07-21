# =====================================================================
# 05-idiomas.R  |  Etapa 5 de 6
#
# Determina el idioma de publicacion y la condicion de acceso abierto de
# cada revista, a partir de los catalogos descargados en la etapa 04.
#
# Entradas : input/temp/catalogos.rds      (etapa 4: wos, scopus, scielo)
#            input/temp/revistas-issn.rds  (etapa 3)
# Salidas  : input/temp/idioma-revista.rds (revista_id -> idioma, fuente_idioma, oa)
#
# Cada catalogo codifica los idiomas de forma distinta:
#   WoS     nombres en ingles ("English", "Spanish", ...)
#   Scopus  codigos ISO de tres letras ("eng", "spa", ...)
#   SciELO  codigos de dos letras ("en", "es", ...)
# Se traducen todos a un codigo interno de dos letras y recien despues se
# comparan. Cuando una revista aparece en mas de un catalogo, se prioriza
# WoS > Scopus > SciELO (de mayor a menor curatoria editorial).
# =====================================================================


catalogos     <- readRDS(ruta_temp("catalogos.rds"))
revistas_issn <- readRDS(ruta_temp("revistas-issn.rds"))


## ---------------------------------------------------------------------
## 1. DICCIONARIOS DE IDIOMA
## ---------------------------------------------------------------------
## Se reportan siete idiomas; el resto se agrupa en "Otro" (xx) y las
## revistas multilingues en "Multi-idioma" (ml).

nombre_es <- c(
  es = "Español",   en = "Inglés",       de = "Alemán",
  pt = "Portugués", nl = "Neerlandés",   fr = "Francés",
  it = "Italiano",  ml = "Multi-idioma", xx = "Otro"
)

# WoS: nombre en ingles -> codigo de dos letras
wos_a_2l <- c(
  "English" = "en", "Spanish" = "es", "German" = "de",
  "Portuguese" = "pt", "Dutch" = "nl", "French" = "fr",
  "Italian" = "it", "Multi-Language" = "ml"
)

# Scopus: ISO 639-2 (tres letras, con variantes B/T) -> codigo de dos letras
iso3_a_2l <- c(
  "eng" = "en", "spa" = "es", "ger" = "de", "deu" = "de",
  "por" = "pt", "dut" = "nl", "nld" = "nl",
  "fre" = "fr", "fra" = "fr", "ita" = "it", "mul" = "ml"
)

# SciELO ya entrega codigos de dos letras: se usa un diccionario identidad
# para reutilizar la misma funcion de traduccion y homogeneizar
# separadores. Los codigos fuera de los siete permitidos caen en "xx".
scielo_a_2l <- setNames(names(nombre_es), names(nombre_es))
scielo_a_2l <- scielo_a_2l[!scielo_a_2l %in% c("xx", "ml")]


## ---------------------------------------------------------------------
## 2. FUNCIONES DE TRADUCCION
## ---------------------------------------------------------------------

#' Convierte una celda de idiomas (posiblemente multiple) al conjunto de
#' codigos de dos letras, separados por ";". Idiomas presentes pero no
#' reconocidos pasan al residual "xx"; celdas vacias devuelven NA.
a_dos_letras <- function(x, diccionario) {
  if (is.na(x) || x == "") return(NA_character_)
  partes <- x |> str_split("[;,/]") |> unlist() |> str_trim()
  partes <- partes[partes != ""]
  if (length(partes) == 0) return(NA_character_)

  traducidos <- diccionario[partes]
  traducidos <- if_else(is.na(traducidos), "xx", traducidos)
  traducidos |> unique() |> paste(collapse = ";")
}

#' Une varios conjuntos de codigos en un unico set sin repeticiones.
unir_codigos <- function(v) {
  v <- v[!is.na(v)]
  if (length(v) == 0) return(NA_character_)
  v |> str_split(";") |> unlist() |> str_trim() |>
    discard(~ .x == "") |> unique() |> paste(collapse = ";")
}

#' Traduce codigos de dos letras a nombres en espanol.
a_nombre_es <- function(codigos) {
  if (is.na(codigos)) return(NA_character_)
  partes <- codigos |> str_split(";") |> unlist() |> str_trim()
  nombre_es[partes] |> discard(is.na) |> unique() |> paste(collapse = "; ")
}


## ---------------------------------------------------------------------
## 3. IDIOMA POR ISSN EN CADA CATALOGO
## ---------------------------------------------------------------------

idioma_wos <- catalogos$wos |>
  mutate(idioma_2l = map_chr(languages, a_dos_letras, diccionario = wos_a_2l)) |>
  catalogo_a_largo("idioma_2l")

idioma_scopus <- catalogos$scopus |>
  mutate(idioma_2l = map_chr(
    str_to_lower(article_language_in_source_three_letter_iso_language_codes),
    a_dos_letras, diccionario = iso3_a_2l)) |>
  catalogo_a_largo("idioma_2l")

idioma_scielo <- catalogos$scielo |>
  mutate(idioma_2l = map_chr(idiomas_pub, a_dos_letras,
                             diccionario = scielo_a_2l)) |>
  filter(!is.na(issn), !is.na(idioma_2l)) |>
  distinct(issn, idioma_2l)


## ---------------------------------------------------------------------
## 4. IDIOMA POR REVISTA
## ---------------------------------------------------------------------
## Se resuelve al nivel de revista_id (no de ISSN) para que las variantes
## impresa y electronica de una misma revista no den resultados distintos.

idioma_revista <- revistas_issn |>
  left_join(rename(idioma_wos,    idi_wos    = idioma_2l), by = "issn") |>
  left_join(rename(idioma_scopus, idi_scopus = idioma_2l), by = "issn") |>
  left_join(rename(idioma_scielo, idi_scielo = idioma_2l), by = "issn") |>
  summarise(
    idi_wos    = unir_codigos(idi_wos),
    idi_scopus = unir_codigos(idi_scopus),
    idi_scielo = unir_codigos(idi_scielo),
    .by = revista_id
  ) |>
  mutate(
    idioma_2l = coalesce(idi_wos, idi_scopus, idi_scielo),
    fuente_idioma = case_when(
      !is.na(idi_wos)    ~ "WoS",
      !is.na(idi_scopus) ~ "Scopus",
      !is.na(idi_scielo) ~ "SciELO",
      .default = NA_character_
    ),
    idioma = map_chr(idioma_2l, a_nombre_es),
    # Si la revista publica en mas de un idioma se reporta como multilingue.
    idioma = if_else(str_detect(idioma, ";"), "Multi-idioma", idioma)
  ) |>
  select(revista_id, idioma, fuente_idioma)


## ---------------------------------------------------------------------
## 5. ACCESO ABIERTO (fuente: Scopus)
## ---------------------------------------------------------------------
## NA cuando la revista no esta en Scopus: no es lo mismo "no es de acceso
## abierto" que "no hay informacion".

oa_scopus <- catalogos$scopus |>
  mutate(oa = if_else(!is.na(open_access_status) &
                        str_detect(open_access_status, "Open Access"),
                      "Sí", "No")) |>
  catalogo_a_largo("oa")

oa_revista <- revistas_issn |>
  left_join(oa_scopus, by = "issn") |>
  summarise(
    oa = case_when(
      any(oa == "Sí", na.rm = TRUE) ~ "Sí",
      all(is.na(oa))                ~ NA_character_,
      .default                      = "No"
    ),
    .by = revista_id
  )

idioma_revista <- left_join(idioma_revista, oa_revista, by = "revista_id")

saveRDS(idioma_revista, ruta_temp("idioma-revista.rds"))


## ---------------------------------------------------------------------
## 6. VERIFICACIONES
## ---------------------------------------------------------------------

print(count(idioma_revista, idioma))
print(count(idioma_revista, fuente_idioma))
print(count(idioma_revista, oa))
