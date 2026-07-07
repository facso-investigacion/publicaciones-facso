rm(list = ls())

library(tidyverse)
library(readxl)
library(readr)
library(httr)
library(jsonlite)
library(janitor)   # necesario para clean_names()


load("output/base_orcid.rdata")
load("output/publicaciones_academico.rdata")


## ---------------------------------------------------------------------
## 1. CARGAR LISTAS MAESTRAS
## ---------------------------------------------------------------------

scie_wos <- read_csv("input/scie-wos.csv")
ssci_wos <- read_csv("input/ssci-wos.csv")
ahci_wos <- read_csv("input/ahci-wos.csv")
scopus_journals <- read_xlsx("input/scopus-journals.xlsx")
scielo_journals <- read_csv("input/scielo-journals.csv")

# WoS: solo SCIE, SSCI y AHCI (ESCI queda deliberadamente fuera -> "Otra")
wos_journals <- bind_rows(ahci_wos, scie_wos, ssci_wos)
write_csv(wos_journals, "input/wos_journals.csv")

scopus_journals <- scopus_journals |> clean_names() |>
  filter(active_or_inactive == "Active")
write_csv(scopus_journals, "input/scopus_journals.csv")


## ---------------------------------------------------------------------
## 2. BUSQUEDA DE REVISTAS SciELO (API ArticleMeta)
## ---------------------------------------------------------------------

base_url <- "http://articlemeta.scielo.org/api/v1"

# Obtener los codigos de todas las colecciones (esto faltaba en el original)
colecciones <- fromJSON(
  content(GET(paste0(base_url, "/collection/identifiers/")),
          "text", encoding = "UTF-8")
)
codigos <- colecciones$code

extraer_revistas <- function(codigo) {
  resp <- GET(paste0(base_url, "/journal/"),
              query = list(collection = codigo))
  if (status_code(resp) != 200) return(NULL)

  datos <- fromJSON(content(resp, "text", encoding = "UTF-8"),
                    simplifyVector = FALSE)

  map_dfr(datos, function(j) {
    titulo <- tryCatch(j$v100[[1]][["_"]], error = function(e) NA_character_)

    idioma_principal <- tryCatch(j$v350[[1]][["_"]],
                                 error = function(e) NA_character_)

    idiomas_pub <- tryCatch(
      map_chr(j$v360, ~ .x[["_"]] %||% NA_character_) %>%
        discard(is.na) %>% paste(collapse = "; "),
      error = function(e) NA_character_
    )

    issns <- unlist(j$issns)
    if (is.null(issns) || length(issns) == 0) issns <- j$code

    tibble(
      coleccion        = codigo,
      titulo           = titulo %||% NA_character_,
      idioma_principal = idioma_principal %||% NA_character_,
      idiomas_pub      = idiomas_pub %||% NA_character_,
      issn             = issns
    )
  })
}

scielo_raw <- map_dfr(codigos, extraer_revistas)


## ---------------------------------------------------------------------
## 3. NORMALIZAR ISSN
## ---------------------------------------------------------------------

norm_issn <- function(x) {
  x %>%
    str_to_upper() %>%
    str_remove_all("[^0-9X]") %>%
    str_pad(width = 8, side = "left", pad = "0") %>%
    na_if("00000000")
}

# Guardar el catalogo SciELO conservando idiomas_pub (lo usa proc-idiomas.R)
scielo_journals <- scielo_raw %>%
  mutate(issn = norm_issn(issn)) %>%
  filter(!is.na(issn))
write_csv(scielo_journals, "input/scielo_issn.csv")

# Lista de ISSN de SciELO para el marcado de indexacion
scielo <- scielo_journals %>% distinct(issn)


## ---------------------------------------------------------------------
## 4. CARGAR LISTAS MAESTRAS COMO CONJUNTOS DE ISSN
## ---------------------------------------------------------------------

cargar_lista <- function(df, cols_issn) {
  df %>%
    select(all_of(cols_issn)) %>%
    pivot_longer(everything(), values_to = "issn") %>%
    mutate(issn = norm_issn(issn)) %>%
    filter(!is.na(issn)) %>%
    distinct(issn)
}

wos    <- cargar_lista(read_csv("input/wos_journals.csv"),
                       c("ISSN", "eISSN"))
scopus <- cargar_lista(read_csv("input/scopus_journals.csv"),
                       c("issn", "eissn"))


## ---------------------------------------------------------------------
## 5. CONSTRUIR LA BASE CONSOLIDADA CON UN ID ESTABLE POR PUBLICACION
## ---------------------------------------------------------------------
## El ISSN es el identificador de revista; id_pub identifica cada
## publicacion (fila) para poder repropagar los resultados tras el
## paso por formato largo. Se define DESPUES de unir ambas bases.

# base_orcid: se procesa la indexacion mas abajo, por ISSN
resultado <- base_orcid %>%
  mutate(indexacion = NA_character_) %>%   # placeholder, se calcula por ISSN
  rename(revista = nombre_revista)

base_long_consolidada <- base_long %>%
  rename(titulo  = titulo_de_documento,
         revista = titulo_revista,
         anio    = ano) %>%
  bind_rows(resultado) %>%
  mutate(tipo_documento = case_when(
    tipo_documento %in% c("LIBRO", "Libro", "book")            ~ "book",
    tipo_documento %in% c("Capítulo de libro", "book-chapter") ~ "book-chapter",
    tipo_documento %in% c("Artículo", "journal-article")       ~ "journal-article",
    TRUE ~ tipo_documento)) %>%
  mutate(id_pub = row_number())            # identificador estable de fila


## ---------------------------------------------------------------------
## 6. TABLA DE ISSN EN FORMATO LARGO (una fila por id_pub + issn)
## ---------------------------------------------------------------------
## Reune los ISSN de ambos origenes: issn (base_orcid, con ';'),
## issn_p e issn_e (base_long). Las columnas ausentes en una parte
## quedan como NA y se descartan al normalizar.

cols_issn_base <- intersect(c("issn", "issn_p", "issn_e"),
                            names(base_long_consolidada))

base_issn_largo <- base_long_consolidada %>%
  select(id_pub, all_of(cols_issn_base)) %>%
  pivot_longer(all_of(cols_issn_base), values_to = "issn") %>%
  separate_rows(issn, sep = ";") %>%
  mutate(issn = norm_issn(issn)) %>%
  filter(!is.na(issn)) %>%
  distinct(id_pub, issn)


## ---------------------------------------------------------------------
## 7. MARCAR PERTENENCIA POR ISSN Y APLICAR JERARQUIA
## ---------------------------------------------------------------------
## WoS (SCIE/SSCI/AHCI) > Scopus > SciELO > Otra

indexacion_por_pub <- base_issn_largo %>%
  mutate(
    en_wos    = issn %in% wos$issn,
    en_scopus = issn %in% scopus$issn,
    en_scielo = issn %in% scielo$issn
  ) %>%
  group_by(id_pub) %>%
  summarise(
    en_wos    = any(en_wos),
    en_scopus = any(en_scopus),
    en_scielo = any(en_scielo),
    .groups = "drop"
  ) %>%
  mutate(
    indexacion = case_when(
      en_wos    ~ "WoS",
      en_scopus ~ "Scopus",
      en_scielo ~ "Scielo",
      TRUE      ~ "Otra"
    )
  )


## ---------------------------------------------------------------------
## 8. PROPAGAR A CADA PUBLICACION
## ---------------------------------------------------------------------
## La indexacion solo aplica a articulos de revista; el resto queda NA.

base_long_consolidada <- base_long_consolidada %>%
  select(-indexacion) %>%                       # descartar el placeholder vacio
  left_join(indexacion_por_pub %>% select(id_pub, indexacion),
            by = "id_pub") %>%
  mutate(
    indexacion = if_else(tipo_documento != "journal-article",
                         NA_character_, indexacion)
  )

save(base_long_consolidada, file = "output/base_consolidada.rdata")
