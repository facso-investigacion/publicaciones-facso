# =====================================================================
# buscar-vejez-envejecimiento.R
#
# Filtra output/consolidado-wide.rdata para identificar publicaciones
# relacionadas con vejez o envejecimiento (y sinonimos habituales del
# campo: adulto mayor, tercera edad, gerontologia), incluyendo sus
# equivalentes en ingles (el consolidado incluye publicaciones en
# distintos idiomas, ver columna `idioma`).
#
# La busqueda combina titulo + abstract + keywords, sin tildes y en
# minuscula (quitar_tildes(), de 00-funciones.R). abstract/keywords
# solo existen para publicaciones con match en Scopus, por lo que el
# titulo es el unico campo de texto confiablemente poblado: la columna
# campo_match indica en cual(es) de los tres se encontro coincidencia,
# para priorizar la revision manual (titulo > abstract/keywords).
#
# ENTRADA   output/consolidado-wide.rdata (consolidado_wide)
# SALIDA    output/publicaciones-vejez-envejecimiento.xlsx
#
# USO
#   source("proc/buscar-vejez-envejecimiento.R")
# =====================================================================

source("proc/00-funciones.R", encoding = "UTF-8")

pacman::p_load(writexl)

consolidado_wide <- leer_rdata(ruta_output("consolidado-wide.rdata"), "consolidado_wide")


## ---------------------------------------------------------------------
## 1. Terminos de busqueda
## ---------------------------------------------------------------------
## Un solo regex, aplicado sobre texto sin tildes y en minuscula. Los
## terminos completos llevan limites de palabra (\\b) en ambos extremos
## para no matchear como substring dentro de otras palabras: sin \\b,
## "aging" hace match dentro de "engAGING", "manAGING", "stAGING", etc.
## Los prefijos (envejec, gerontolog) solo llevan \\b al inicio, porque
## se usan a proposito para cubrir sus derivaciones (envejecimiento,
## gerontologia/gerontology).
##   - vejez
##   - envejec            -> envejecimiento, envejecer, envejecido/a
##   - adultos? mayores?  -> adulto mayor, adultos mayores
##   - tercera edad
##   - gerontolog         -> gerontologia/gerontologico/a (ES), gerontology (EN)
##   - aging|ageing       -> aging, ageing (variantes AmEn/BrEn)
##   - elderly
##   - older adults?      -> older adult, older adults
##   - old age

PATRON_VEJEZ <- paste(
  "\\bvejez\\b", "\\benvejec", "\\badultos? mayores?\\b",
  "\\btercera edad\\b", "\\bgerontolog", "\\baging\\b", "\\bageing\\b",
  "\\belderly\\b", "\\bolder adults?\\b", "\\bold age\\b",
  sep = "|"
)


## ---------------------------------------------------------------------
## 2. Texto de busqueda por publicacion + deteccion por campo
## ---------------------------------------------------------------------

normalizar_busqueda <- function(x) {
  quitar_tildes(str_to_lower(coalesce(x, "")))
}

resultado <- consolidado_wide |>
  mutate(
    match_titulo   = str_detect(normalizar_busqueda(titulo), PATRON_VEJEZ),
    match_resumen  = str_detect(normalizar_busqueda(abstract), PATRON_VEJEZ) |
                      str_detect(normalizar_busqueda(keywords), PATRON_VEJEZ),
    campo_match    = case_when(
      match_titulo & match_resumen ~ "titulo + abstract/keywords",
      match_titulo                 ~ "titulo",
      match_resumen                 ~ "abstract/keywords",
      .default = NA_character_
    )
  ) |>
  filter(match_titulo | match_resumen) |>
  select(-match_titulo, -match_resumen)


## ---------------------------------------------------------------------
## 3. Columnas de salida
## ---------------------------------------------------------------------

cols_autor <- names(resultado)[str_detect(names(resultado), "^autor_\\d+$")]

resultado <- resultado |>
  select(
    clave_pub, anio, titulo, revista, doi, tipo_documento, indexacion,
    quartil, abstract, keywords, campo_match,
    n_autores_total, colaboracion_internacional,
    any_of(cols_autor)
  ) |>
  arrange(desc(anio))


## ---------------------------------------------------------------------
## 4. Exportar
## ---------------------------------------------------------------------

write_xlsx(resultado, ruta_output("publicaciones-vejez-envejecimiento.xlsx"))

message(
  "Publicaciones encontradas: ", nrow(resultado),
  " (titulo: ", sum(resultado$campo_match == "titulo"),
  ", abstract/keywords: ", sum(resultado$campo_match == "abstract/keywords"),
  ", ambos: ", sum(resultado$campo_match == "titulo + abstract/keywords"), ")"
)
message("Guardado en ", ruta_output("publicaciones-vejez-envejecimiento.xlsx"))
