rm(list = ls())

library(tidyverse)
library(readr)

load("output/base_consolidada.rdata")   # trae base_long_consolidada con id_pub
wos_journals    <- read_csv("input/wos_journals.csv")
scopus_journals <- read_csv("input/scopus_journals.csv")
scielo_journals <- read_csv("input/scielo_issn.csv")

# =====================================================================
# 0. FUNCION DE NORMALIZACION DE ISSN
# =====================================================================
norm_issn <- function(x) {
  x %>%
    str_to_upper() %>%
    str_remove_all("[^0-9X]") %>%
    str_pad(width = 8, side = "left", pad = "0") %>%
    na_if("00000000")
}

# =====================================================================
# 1. DICCIONARIOS DE IDIOMA (siete permitidos + multi + residual)
# =====================================================================

# codigo de 2 letras -> nombre en espanol
nombre_es <- c(
  es = "Español",    en = "Inglés",     de = "Alemán",
  pt = "Portugués",  nl = "Neerlandés", fr = "Francés",
  it = "Italiano",   ml = "Multi-idioma", xx = "Otro"
)

# nombre en ingles (WoS) -> codigo de 2 letras
wos_a_2l <- c(
  "English" = "en", "Spanish" = "es", "German" = "de",
  "Portuguese" = "pt", "Dutch" = "nl", "French" = "fr",
  "Italian" = "it", "Multi-Language" = "ml"
)

# ISO 3 letras (Scopus) -> codigo de 2 letras
iso3_a_2l <- c(
  "eng" = "en", "spa" = "es", "ger" = "de", "deu" = "de",
  "por" = "pt", "dut" = "nl", "nld" = "nl",
  "fre" = "fr", "fra" = "fr", "ita" = "it", "mul" = "ml"
)

# =====================================================================
# 2. FUNCIONES DE TRADUCCION DE IDIOMA
# =====================================================================

# Convierte una celda de idiomas al conjunto de codigos de 2 letras.
# Idiomas no reconocidos (pero presentes) pasan a residual "xx".
# Celdas vacias o NA devuelven NA (sin dato).
a_dos_letras <- function(x, diccionario) {
  if (is.na(x) || x == "") return(NA_character_)
  partes <- x %>% str_split("[;,/]") %>% unlist() %>% str_trim()
  partes <- partes[partes != ""]
  if (length(partes) == 0) return(NA_character_)

  traducidos <- diccionario[partes]
  traducidos <- ifelse(is.na(traducidos), "xx", traducidos)  # residual
  traducidos %>% unique() %>% paste(collapse = ";")
}

# Junta varios conjuntos de codigos de 2 letras en un set unico.
junta_2l <- function(v) {
  v <- v[!is.na(v)]
  if (length(v) == 0) return(NA_character_)
  v %>% str_split(";") %>% unlist() %>% str_trim() %>%
    discard(~ .x == "") %>% unique() %>% paste(collapse = ";")
}

# Traduce codigos de 2 letras a nombre completo en espanol.
a_nombre_es <- function(codigos) {
  if (is.na(codigos)) return(NA_character_)
  codigos %>% str_split(";") %>% unlist() %>% str_trim() %>%
    (\(v) nombre_es[v])() %>%
    discard(is.na) %>% unique() %>%
    paste(collapse = "; ")
}

# =====================================================================
# 3. HOMOGENEIZAR CADA FUENTE -> (issn, idioma_2l) en formato largo
# =====================================================================

# --- WoS ---
wos_idioma <- wos_journals %>%
  mutate(idioma_2l = map_chr(Languages, ~ a_dos_letras(.x, wos_a_2l))) %>%
  pivot_longer(c(ISSN, eISSN), values_to = "issn") %>%
  mutate(issn = norm_issn(issn)) %>%
  filter(!is.na(issn), !is.na(idioma_2l)) %>%
  distinct(issn, idioma_2l)

# --- Scopus ---
scopus_idioma <- scopus_journals %>%
  mutate(idioma_2l = map_chr(
    str_to_lower(article_language_in_source_three_letter_iso_language_codes),
    ~ a_dos_letras(.x, iso3_a_2l))) %>%
  pivot_longer(c(issn, eissn), values_to = "issn") %>%
  mutate(issn = norm_issn(issn)) %>%
  filter(!is.na(issn), !is.na(idioma_2l)) %>%
  distinct(issn, idioma_2l)

# --- SciELO (idiomas_pub ya viene en codigos de 2 letras) ---
# Diccionario identidad para reutilizar la funcion y homogeneizar
# separadores; los codigos fuera de los 7 permitidos caeran en "xx".
scielo_dic <- setNames(names(nombre_es), names(nombre_es))
scielo_dic <- scielo_dic[!scielo_dic %in% c("xx", "ml")]

scielo_idioma <- scielo_journals %>%
  mutate(idioma_2l = map_chr(idiomas_pub, ~ a_dos_letras(.x, scielo_dic))) %>%
  mutate(issn = norm_issn(issn)) %>%
  filter(!is.na(issn), !is.na(idioma_2l)) %>%
  distinct(issn, idioma_2l)

# =====================================================================
# 4. TABLA DE ISSN DE LA BASE EN FORMATO LARGO (id_pub + issn)
# =====================================================================
## id_pub ya viene definido desde proc-indexaciones.R. El ISSN es el
## identificador de revista; id_pub identifica cada publicacion.

cols_issn_base <- intersect(c("issn", "issn_p", "issn_e"),
                            names(base_long_consolidada))

base_issn_largo <- base_long_consolidada %>%
  select(id_pub, all_of(cols_issn_base)) %>%
  pivot_longer(all_of(cols_issn_base), values_to = "issn") %>%
  separate_rows(issn, sep = ";") %>%
  mutate(issn = norm_issn(issn)) %>%
  filter(!is.na(issn)) %>%
  distinct(id_pub, issn)

# =====================================================================
# 5. RESOLVER IDIOMA POR ISSN (prioridad WoS > Scopus > SciELO)
# =====================================================================
idioma_por_issn <- base_issn_largo %>%
  distinct(issn) %>%
  left_join(wos_idioma    %>% rename(idi_wos    = idioma_2l), by = "issn") %>%
  left_join(scopus_idioma %>% rename(idi_scopus = idioma_2l), by = "issn") %>%
  left_join(scielo_idioma %>% rename(idi_scielo = idioma_2l), by = "issn") %>%
  group_by(issn) %>%
  summarise(
    idi_wos    = junta_2l(idi_wos),
    idi_scopus = junta_2l(idi_scopus),
    idi_scielo = junta_2l(idi_scielo),
    .groups = "drop"
  ) %>%
  mutate(
    idioma_2l = case_when(
      !is.na(idi_wos)    ~ idi_wos,
      !is.na(idi_scopus) ~ idi_scopus,
      !is.na(idi_scielo) ~ idi_scielo,
      TRUE               ~ NA_character_
    ),
    fuente_idioma = case_when(
      !is.na(idi_wos)    ~ "WoS",
      !is.na(idi_scopus) ~ "Scopus",
      !is.na(idi_scielo) ~ "SciELO",
      TRUE               ~ NA_character_
    )
  )

# =====================================================================
# 6. PROPAGAR A CADA PUBLICACION (id_pub)
# =====================================================================
idioma_por_pub <- base_issn_largo %>%
  left_join(idioma_por_issn %>% select(issn, idioma_2l, fuente_idioma),
            by = "issn") %>%
  group_by(id_pub) %>%
  summarise(
    idioma_2l = junta_2l(idioma_2l),
    fuente_idioma = case_when(
      any(fuente_idioma == "WoS",    na.rm = TRUE) ~ "WoS",
      any(fuente_idioma == "Scopus", na.rm = TRUE) ~ "Scopus",
      any(fuente_idioma == "SciELO", na.rm = TRUE) ~ "SciELO",
      TRUE                                         ~ NA_character_
    ),
    .groups = "drop"
  ) %>%
  mutate(
    idioma = map_chr(idioma_2l, a_nombre_es),
    idioma = if_else(str_detect(idioma, ";"), "Multi-idioma", idioma)
  )

# =====================================================================
# 7. ACCESO ABIERTO DESDE SCOPUS -> (issn, oa)
# =====================================================================
scopus_oa <- scopus_journals %>%
  mutate(
    oa = if_else(
      !is.na(open_access_status) &
        str_detect(open_access_status, "Open Access"),
      "Sí", "No"
    )
  ) %>%
  pivot_longer(c(issn, eissn), values_to = "issn") %>%
  mutate(issn = norm_issn(issn)) %>%
  filter(!is.na(issn)) %>%
  distinct(issn, oa)

# Resolver por ISSN, conservando NA cuando la revista no esta en Scopus
oa_por_issn <- base_issn_largo %>%
  distinct(issn) %>%
  left_join(scopus_oa, by = "issn") %>%
  group_by(issn) %>%
  summarise(
    oa = case_when(
      any(oa == "Sí", na.rm = TRUE) ~ "Sí",
      all(is.na(oa))                ~ NA_character_,
      TRUE                          ~ "No"
    ),
    .groups = "drop"
  )

# Propagar a cada publicacion (NA si ninguno de sus ISSN esta en Scopus)
oa_por_pub <- base_issn_largo %>%
  left_join(oa_por_issn, by = "issn") %>%
  group_by(id_pub) %>%
  summarise(
    oa = case_when(
      any(oa == "Sí", na.rm = TRUE) ~ "Sí",
      all(is.na(oa))                ~ NA_character_,
      TRUE                          ~ "No"
    ),
    .groups = "drop"
  )

# =====================================================================
# 8. PEGAR COLUMNAS A LA BASE Y GUARDAR
# =====================================================================
base_final <- base_long_consolidada %>%
  left_join(idioma_por_pub %>% select(id_pub, idioma, fuente_idioma),
            by = "id_pub") %>%
  left_join(oa_por_pub, by = "id_pub")

save(base_final, file = "output/base_final.rdata")
write_csv(base_final, "output/base_final.csv")

# =====================================================================
# 9. VERIFICACIONES DE CALIDAD
# =====================================================================
count(base_final, idioma)
count(base_final, fuente_idioma)
count(base_final, oa)
