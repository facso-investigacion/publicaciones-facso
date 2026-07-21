# =====================================================================
# 04-indexaciones.R  |  Etapa 4 de 6
#
# Construye los catalogos maestros de revistas (WoS, Scopus, SciELO) y
# clasifica la indexacion de cada revista del proyecto.
#
# Entradas : input/original/scie-wos.csv, input/original/ssci-wos.csv, input/original/ahci-wos.csv
#            input/original/scopus-journals.xlsx
#            API SciELO ArticleMeta (catalogo de revistas por coleccion)
#            input/temp/revistas-issn.rds  (etapa 3)
# Salidas  : input/temp/catalogos.rds            (lista: wos, scopus, scielo)
#            input/temp/indexacion-revista.rds   (revista_id -> indexacion)
#
# Criterios:
#   - WoS considera SCIE, SSCI y AHCI. ESCI queda deliberadamente fuera y
#     cae en la categoria "Otra".
#   - De Scopus solo se usan revistas activas.
#   - Jerarquia excluyente: WoS > Scopus > SciELO > Otra.
#
# Los catalogos se guardan en input/temp/ porque las etapas 05 (idiomas) y 06
# (SJR) los reutilizan; input/ se mantiene de solo lectura.
# =====================================================================


## ---------------------------------------------------------------------
## 1. CATALOGOS WOS Y SCOPUS
## ---------------------------------------------------------------------

archivos_wos <- ruta_input(c("ahci-wos.csv", "scie-wos.csv", "ssci-wos.csv"))
verificar_archivos(c(archivos_wos, ruta_input("scopus-journals.xlsx")))

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
             scielo = scielo_journals),
        ruta_temp("catalogos.rds"))


## ---------------------------------------------------------------------
## 3. CONJUNTOS DE ISSN POR BASE DE INDEXACION
## ---------------------------------------------------------------------

issn_wos    <- issn_de_catalogo(wos_journals)
issn_scopus <- issn_de_catalogo(scopus_journals)
issn_scielo <- unique(scielo_journals$issn)

message("  ISSN en catalogos -> WoS: ", length(issn_wos),
        " | Scopus: ", length(issn_scopus),
        " | SciELO: ", length(issn_scielo))


## ---------------------------------------------------------------------
## 4. INDEXACION POR REVISTA
## ---------------------------------------------------------------------
## Una revista puede tener varios ISSN; basta que uno figure en un
## catalogo para considerarla indexada alli.

revistas_issn <- readRDS(ruta_temp("revistas-issn.rds"))

indexacion_revista <- revistas_issn |>
  mutate(
    en_wos    = issn %in% issn_wos,
    en_scopus = issn %in% issn_scopus,
    en_scielo = issn %in% issn_scielo
  ) |>
  summarise(across(c(en_wos, en_scopus, en_scielo), any),
            .by = revista_id) |>
  mutate(
    indexacion = case_when(
      en_wos    ~ "WoS",
      en_scopus ~ "Scopus",
      en_scielo ~ "Scielo",
      .default  = "Otra"
    )
  )

saveRDS(indexacion_revista, ruta_temp("indexacion-revista.rds"))


## ---------------------------------------------------------------------
## 5. VERIFICACIONES
## ---------------------------------------------------------------------

print(count(indexacion_revista, indexacion))
