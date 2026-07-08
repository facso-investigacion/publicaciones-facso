library(pacman)
p_load(tidyverse,
       readxl,
       janitor)


### Cargar datos

publicaciones_2020 <- read_xlsx("input/publicaciones-2020.xlsx")
publicaciones_2021 <- read_xlsx("input/publicaciones-2021.xlsx")
publicaciones_2022 <- read_xlsx("input/publicaciones-2022.xlsx")
publicaciones_2023 <- read_xlsx("input/publicaciones-2023.xlsx")
publicaciones_2024 <- read_xlsx("input/publicaciones-2024.xlsx")
publicaciones_2025 <- read_xlsx("input/publicaciones-2025.xlsx")
acad <- read_xlsx("input/acad.xlsx")
load("input/acads-historico.rdata")


## Función para transformar todo a chracter en todas las bases, para facilitar el bind

transformar_publicaciones_a_character <- function(envir = .GlobalEnv) {
  # Buscar todos los objetos que empiecen con "publicaciones"
  nombres <- ls(envir = envir, pattern = "^publicaciones")
  
  if (length(nombres) == 0) {
    message("No se encontraron objetos que empiecen con 'publicaciones'.")
    return(invisible(NULL))
  }
  
  for (nombre in nombres) {
    obj <- get(nombre, envir = envir)
    
    # Solo procesar si es un data.frame (o tibble, que también es data.frame)
    if (is.data.frame(obj)) {
      obj[] <- lapply(obj, as.character)
      assign(nombre, obj, envir = envir)
      message("Convertido: ", nombre)
    } else {
      message("Omitido (no es data.frame): ", nombre)
    }
  }
  
  invisible(NULL)
}

transformar_publicaciones_a_character()

consolidado <- bind_rows(publicaciones_2020,
                         publicaciones_2021,
                         publicaciones_2022,
                         publicaciones_2023,
                         publicaciones_2024,
                         publicaciones_2025)

consolidado_long <- consolidado |> clean_names() |> 
  mutate(across(
    matches("^rut_autor([1-9]|1[0-9]|2[0-5])$"),
    ~ .x %>%
      str_remove_all("[-.]") %>%          # elimina guion y puntos (por si acaso)
      str_trim() %>%                       # elimina espacios en blanco
      str_pad(width = 10, side = "left", pad = "0")  # rellena con 0 a la izquierda
  )) |> 
  pivot_longer(
    cols = matches("^rut_autor([1-9]|1[0-9]|2[0-5])$"),
    names_to = "num_autor",
    values_to = "rut_autor",
    names_prefix = "rut_autor"
  ) %>%
  filter(!is.na(rut_autor) & rut_autor != "0000000000") |> 
  select("indexado_en",
         "titulo_de_documento",
         "titulo_revista",
         "ano",
         "doi",
         "tipo_documento",
         rut = "rut_autor",
         num_autor,
         issn_p,
         issn_e)


save(consolidado_long, file="input/consolidado-publicaciones.rdata")

# procesar las bases de ademicos

acad <- acad |> clean_names() |> 
  mutate(nombre_completo = paste0(nombres, " ", paterno, " ", materno)) |> 
  select(reparticion,
         cargo,
         sexo,
         edad,
         rut,
         nombre_completo,
         horas_reales) |> 
  group_by(rut) %>%
  mutate(suma_horas = sum(horas_reales, na.rm = TRUE)) %>%
  {
    # Casos donde la suma es <= 44: colapsar sumando horas
    casos_suma <- filter(., suma_horas <= 44) %>%
      slice(1) %>%                          # se queda con un registro representativo
      mutate(horas_reales = suma_horas)      # pero con la suma de horas
    
    # Casos donde la suma es > 44: quedarse con el de mayor horas_reales
    casos_max <- filter(., suma_horas > 44) %>%
      slice_max(horas_reales, n = 1, with_ties = FALSE)
    
    bind_rows(casos_suma, casos_max)
  } %>%
  ungroup() %>%
  select(-suma_horas) |> 
  mutate(departamento = case_when(
    str_detect(str_to_lower(reparticion), "psicología|psicologia") ~ "Psicología",
    str_detect(str_to_lower(reparticion), "sociología|sociologia") ~ "Sociología",
    str_detect(str_to_lower(reparticion), "antropología|antropologia") ~ "Antropología",
    str_detect(str_to_lower(reparticion), "trabajo social") ~ "Trabajo social",
    str_detect(str_to_lower(reparticion), "educación|educacion") ~ "Educación",
    str_detect(str_to_lower(reparticion), "postgrado") ~ "Postgrado",
    TRUE ~ NA),
    jerarquia = case_when(
      str_detect(str_to_lower(cargo), "titular")      ~ "Titular",
      str_detect(str_to_lower(cargo), "asociado")     ~ "Asociado",
      str_detect(str_to_lower(cargo), "asistente")    ~ "Asistente",
      str_detect(str_to_lower(cargo), "postdoctoral") ~ "Investigador Postdoctoral",
      str_detect(str_to_lower(cargo), "adjunto")      ~ "Adjunto",
      str_detect(str_to_lower(cargo), "instructor")   ~ "Instructor",
      str_detect(str_to_lower(cargo), "ayudante")     ~ NA_character_,
      str_detect(str_to_lower(cargo), "evaluado")     ~ NA_character_))

save(acad, file="input/acad-proc.rdata")

# retirados_periodo <- academicos_historico |> 
#   filter(retiro>2019 & retiro<=2025) |> 
#   select(rut=rut_investigador,
#          reparticion,
#          cargo=jerarquia,
#          edad,
#          nombre_completo,
#          horas_reales)
# 
# acad_bind <- bind_rows(acad, retirados_periodo)


base_long <- inner_join(consolidado_long, acad, by="rut") |> 
  filter(tipo_documento %in% c("Artículo", "Capítulo de libro", "Libro", "LIBRO")) |> 
  mutate(,
    indexacion = case_when(
      str_detect(indexado_en, regex("wos", ignore_case = TRUE)) ~ "WoS",
      str_detect(indexado_en, regex("scopus", ignore_case = TRUE)) ~ "Scopus",
      str_detect(indexado_en, regex("scielo", ignore_case = TRUE)) ~ "Scielo",
      str_detect(indexado_en, regex("esci|otro registro", ignore_case = TRUE)) ~ "Otra",
      TRUE ~ "Sin clasificar"
    )) |> 
  filter(jerarquia %in% c("Titular", "Asociado", "Asistente", "Adjunto", "Instructor"))
      

save(base_long, file="output/publicaciones_academico.rdata")


# Base publicaciones-departamento

publicaciones_depto <- base_long |> 
  distinct(doi, departamento, .keep_all=TRUE) |> 
  dplyr::select(doi, ano, departamento, tipo_documento, indexacion, titulo_de_documento, titulo_revista)

save(publicaciones_depto, file="output/publicaciones_depto.rdata")

# Base wide publicaciones FACSO

base_wide <- base_long |> 
  select(indexacion,
         titulo_de_documento,
         titulo_revista,
         tipo_documento,
         ano,
         doi,
         num_autor,
         rut) |> 
    pivot_wider(
  id_cols = c(ano, doi, indexacion, tipo_documento, titulo_de_documento, titulo_revista),
  names_from = num_autor,
  values_from = rut,
  names_glue = "rut_autor_{num_autor}"
)

save(base_wide, file="output/publicaciones_facso_proc.rdata")

