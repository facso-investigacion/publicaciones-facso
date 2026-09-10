# =====================================================================
# 04-indexaciones.R  |  Etapa 4 de 6
#
# Construye los catalogos maestros de revistas (WoS, Scopus, SciELO,
# Latindex, ERIH PLUS) y clasifica la indexacion de cada revista del
# proyecto.
#
# Entradas : input/original/scie-wos.csv, input/original/ssci-wos.csv, input/original/ahci-wos.csv
#            input/original/scopus-journals.xlsx
#            input/original/latindex_catalogo.csv (export manual, ver mas abajo)
#            input/original/erih_plus.xlsx        (export manual, ver mas abajo)
#            API SciELO ArticleMeta (catalogo de revistas por coleccion)
#            input/temp/revistas-issn.rds  (etapa 3)
# Salidas  : input/temp/catalogos.rds            (lista: wos, scopus, scielo,
#                                                  latindex, erih)
#            input/temp/indexacion-revista.rds   (revista_id -> indexacion)
#
# Criterios:
#   - WoS considera SCIE, SSCI y AHCI. ESCI queda deliberadamente fuera y
#     cae en la categoria "Otra".
#   - De Scopus solo se usan revistas activas.
#   - De ERIH PLUS se excluyen las revistas dadas de baja (`nedlagt`).
#   - Jerarquia excluyente: WoS > Scopus > SciELO > Latindex/ERIH PLUS > Otra.
#     Latindex y ERIH PLUS se colapsan en una sola categoria "Latindex/ERIH
#     PLUS": basta estar en cualquiera de los dos.
#
# Ni Latindex (Catalogo 2.0) ni ERIH PLUS ofrecen API o descarga masiva
# publica sencilla, a diferencia de WoS/Scopus/SciELO, asi que sus
# catalogos se exportan a mano y se dejan en input/original/:
#   - latindex_catalogo.csv : export del Catalogo 2.0 (`;`-separado), con
#     columnas issn_e/issn_l/issn_imp (electronico, "linking" e impreso;
#     issn_l a veces difiere de los otros dos, asi que se usan los tres).
#   - erih_plus.xlsx        : export del listado de ERIH PLUS, con columnas
#     tidsskrift_issne/tidsskrift_issnp (electronico/impreso).
#
# Los catalogos se guardan en input/temp/ porque las etapas 05 (idiomas) y 06
# (SJR) los reutilizan; input/ se mantiene de solo lectura.
# =====================================================================


## ---------------------------------------------------------------------
## 1. CATALOGOS WOS, SCOPUS, LATINDEX Y ERIH PLUS
## ---------------------------------------------------------------------

archivos_wos <- ruta_input(c("ahci-wos.csv", "scie-wos.csv", "ssci-wos.csv"))
verificar_archivos(c(archivos_wos, ruta_input("scopus-journals.xlsx"),
                     ruta_input("latindex_catalogo.csv"),
                     ruta_input("erih_plus.xlsx")))

wos_journals <- archivos_wos |>
  map(\(archivo) read_csv(archivo, show_col_types = FALSE) |>
        mutate(across(everything(), as.character))) |>
  bind_rows() |>
  clean_names() |>
  distinct()

verificar_columnas(wos_journals, "languages", "catalogos WoS")

scopus_journals <- read_xlsx(ruta_input("scopus-journals.xlsx")) |>
  clean_names() |>
  filter(active_or_inactive == "Active")

verificar_columnas(
  scopus_journals,
  c("open_access_status",
    "article_language_in_source_three_letter_iso_language_codes"),
  "input/original/scopus-journals.xlsx"
)

latindex_journals <- read_delim(ruta_input("latindex_catalogo.csv"), delim = ";",
                                show_col_types = FALSE) |>
  mutate(across(everything(), as.character)) |>
  clean_names()

verificar_columnas(latindex_journals, c("issn_e", "issn_l", "issn_imp"),
                   "input/original/latindex_catalogo.csv")

erih_journals <- read_xlsx(ruta_input("erih_plus.xlsx")) |>
  clean_names() |>
  filter(is.na(nedlagt))

verificar_columnas(erih_journals, c("tidsskrift_issne", "tidsskrift_issnp"),
                   "input/original/erih_plus.xlsx")


## ---------------------------------------------------------------------
## 2. CATALOGO SCIELO (API ArticleMeta)
## ---------------------------------------------------------------------
## Se recorren todas las colecciones nacionales de SciELO. Ademas de los
## ISSN, se conserva `idiomas_pub` (codigos de dos letras), que usa la
## etapa 05.

descargar_scielo <- function() {
  base_url <- "http://articlemeta.scielo.org/api/v1"

  colecciones <- fromJSON(
    content(GET(paste0(base_url, "/collection/identifiers/")),
            "text", encoding = "UTF-8")
  )

  extraer_revistas <- function(codigo) {
    message("    coleccion SciELO: ", codigo)
    resp <- GET(paste0(base_url, "/journal/"),
                query = list(collection = codigo))
    if (status_code(resp) != 200) return(NULL)

    datos <- fromJSON(content(resp, "text", encoding = "UTF-8"),
                      simplifyVector = FALSE)

    map_dfr(datos, function(j) {
      # v100 = titulo, v350 = idioma principal, v360 = idiomas de publicacion
      titulo <- tryCatch(j$v100[[1]][["_"]], error = function(e) NA_character_)
      idioma_principal <- tryCatch(j$v350[[1]][["_"]],
                                   error = function(e) NA_character_)
      idiomas_pub <- tryCatch(
        map_chr(j$v360, ~ .x[["_"]] %||% NA_character_) |>
          discard(is.na) |> paste(collapse = "; "),
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

  map_dfr(colecciones$code, extraer_revistas)
}

scielo_journals <- usar_cache(ruta_temp("scielo-catalogo.rds"),
                              descargar_scielo()) |>
  mutate(issn = norm_issn(issn)) |>
  filter(!is.na(issn))

saveRDS(list(wos = wos_journals, scopus = scopus_journals,
             scielo = scielo_journals, latindex = latindex_journals,
             erih = erih_journals),
        ruta_temp("catalogos.rds"))


## ---------------------------------------------------------------------
## 3. CONJUNTOS DE ISSN POR BASE DE INDEXACION
## ---------------------------------------------------------------------
## Latindex y ERIH PLUS traen ISSN electronico/impreso (y Latindex ademas
## un ISSN "linking" que a veces no coincide con ninguno de los otros dos)
## en columnas separadas; se juntan en un solo vector normalizado.

issn_wos    <- issn_de_catalogo(wos_journals)
issn_scopus <- issn_de_catalogo(scopus_journals)
issn_scielo <- unique(scielo_journals$issn)

issn_latindex <- latindex_journals |>
  select(issn_e, issn_l, issn_imp) |>
  pivot_longer(everything(), values_to = "issn") |>
  mutate(issn = norm_issn(issn)) |>
  filter(!is.na(issn)) |>
  distinct(issn) |>
  pull(issn)

issn_erih <- erih_journals |>
  select(tidsskrift_issne, tidsskrift_issnp) |>
  pivot_longer(everything(), values_to = "issn") |>
  mutate(issn = norm_issn(issn)) |>
  filter(!is.na(issn)) |>
  distinct(issn) |>
  pull(issn)

message("  ISSN en catalogos -> WoS: ", length(issn_wos),
        " | Scopus: ", length(issn_scopus),
        " | SciELO: ", length(issn_scielo),
        " | Latindex: ", length(issn_latindex),
        " | ERIH PLUS: ", length(issn_erih))


## ---------------------------------------------------------------------
## 4. INDEXACION POR REVISTA
## ---------------------------------------------------------------------
## Una revista puede tener varios ISSN; basta que uno figure en un
## catalogo para considerarla indexada alli.

revistas_issn <- readRDS(ruta_temp("revistas-issn.rds"))

indexacion_revista <- revistas_issn |>
  mutate(
    en_wos      = issn %in% issn_wos,
    en_scopus   = issn %in% issn_scopus,
    en_scielo   = issn %in% issn_scielo,
    en_latindex = issn %in% issn_latindex,
    en_erih     = issn %in% issn_erih
  ) |>
  summarise(across(c(en_wos, en_scopus, en_scielo, en_latindex, en_erih), any),
            .by = revista_id) |>
  mutate(
    indexacion = case_when(
      en_wos                ~ "WoS",
      en_scopus              ~ "Scopus",
      en_scielo              ~ "Scielo",
      en_latindex | en_erih  ~ "Latindex/ERIH PLUS",
      .default               = "Otra"
    )
  )

saveRDS(indexacion_revista, ruta_temp("indexacion-revista.rds"))


## ---------------------------------------------------------------------
## 5. VERIFICACIONES
## ---------------------------------------------------------------------

print(count(indexacion_revista, indexacion))
