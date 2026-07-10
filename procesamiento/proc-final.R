rm(list=ls())

library(tidyverse)
library(readr)


load("output/base_final.rdata")
load("output/base_book_lu.rdata")


base_final <- base_final %>%
  filter(
    tipo_documento == "journal-article" |
      (tipo_documento %in% c("book-chapter", "book") & titulo %in% base_books$titulo) |
      !is.na(indexacion)
  )


save(base_final, file="output/base_final.rdata")

# Base publicaciones-departamento

consolidado_depto <- base_final |> 
  distinct(doi, departamento, .keep_all=TRUE) |> 
  dplyr::select(doi, anio, departamento, tipo_documento, indexacion, titulo, 
                revista, issn_p, issn_e, issn, n_citas=scopus_citas,
                quartil=cuartil_sjr, idioma, oa, revista_id)

save(consolidado_depto, file="output/base-completa-depto.rdata")


# base wide

consolidado_wide <- base_final |>
  dplyr::group_by(doi) |>
  dplyr::mutate(autor_n = paste0("autor_", dplyr::row_number())) |>
  dplyr::ungroup() |>
  tidyr::pivot_wider(
    id_cols = c(doi, anio, indexacion, tipo_documento, titulo,
                revista, issn, issn_e, issn_p, cuartil_sjr, scopus_citas, 
                scopus_abstract, scopus_keywords, idioma, oa, revista_id),
    names_from = autor_n,
    values_from = rut
  ) |> 
  rename(quartil=cuartil_sjr, n_citas=scopus_citas, 
         abstract=scopus_abstract, keywords=scopus_keywords)

save(consolidado_wide, file="output/base-completa-facso.rdata")


