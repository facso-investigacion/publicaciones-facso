rm(list=ls())

library(tidyverse)

load("output/publicaciones_academico.rdata")
load("output/base_orcid.rdata")
load("output/base_scopus.rdata")

## Agregar base de scopus

base_long_scopus <- base_long |> 
  left_join(base_scopus, by="doi") |> 
  select(titulo = titulo_de_documento,
         titulo_revista,
         anio = ano,
         doi,
         tipo_documento,
         rut, 
         num_autor,
         sexo,
         edad,
         nombre_completo,
         horas_reales,
         departamento,
         jerarquia,
         indexacion,
         n_citas = scopus_citas,
         keywords = scopus_keywords,
         abstract = scopus_abstract,
         quartil = sjr_best_quartile
         ) 

base_orcid <- base_orcid |>
  mutate(indexacion = case_when(editorial == "Universidad de Chile" ~ "Universidad de Chile")) |> 
  select(titulo,
         nombre_revista=revista,
         anio,
         doi,
         tipo_documento = tipo,
         rut,
         sexo,
         edad,
         nombre_completo,
         horas_reales)

base_consolidada <- bind_rows(base_long_scopus, base_books) |> 
  distinct(rut, doi, .keep_all = TRUE) |> 
  mutate(tipo_documento = case_when(
    tipo_documento %in% c("LIBRO", "Libro", "book") ~ "Libro",
    tipo_documento %in% c("Capítulo de libro", "book-chapter") ~ "Capítulo de Libro",
    TRUE ~ tipo_documento
  ))

save(base_consolidada, file="output/base-completa-academicos.rdata")

# Base publicaciones-departamento

consolidado_depto <- base_consolidada |> 
  distinct(doi, departamento, .keep_all=TRUE) |> 
  dplyr::select(doi, anio, departamento, tipo_documento, indexacion, titulo, titulo_revista, n_citas,
                quartil)

save(consolidado_depto, file="output/base-completa-depto.rdata")


# base wide

consolidado_wide <- base_consolidada |>
  dplyr::group_by(doi) |>
  dplyr::mutate(autor_n = paste0("autor_", dplyr::row_number())) |>
  dplyr::ungroup() |>
  tidyr::pivot_wider(
    id_cols = c(doi, anio, indexacion, tipo_documento, titulo,
                titulo_revista, quartil, n_citas, abstract,
                keywords),
    names_from = autor_n,
    values_from = rut
  )

save(consolidado_wide, file="output/base-completa-facso.rdata")


