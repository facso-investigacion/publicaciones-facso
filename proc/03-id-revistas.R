# =====================================================================
# 03-id-revistas.R  |  Etapa 3 de 6
#
# Une las dos fuentes (SEPAVID y ORCID) en una sola base y asigna un
# identificador unico de revista (`revista_id`).
#
# Por que esta etapa va aqui: es el primer punto del pipeline que
# necesita ambas bases juntas, y `revista_id` es la llave con la que las
# etapas 04, 05 y 06 resuelven indexacion, idioma y SJR. Antes, cada una
# de esas etapas reconstruia por su cuenta la tabla larga de ISSN; ahora
# se construye una sola vez, aqui.
#
# Entradas : input/temp/sepavid-publicaciones.rds (etapa 1)
#            input/temp/orcid-publicaciones.rds   (etapa 2)
# Salidas  : input/temp/base-consolidada.rds  (base larga con id_fila, clave_pub,
#                                        revista_id)
#            input/temp/revistas-issn.rds     (revista_id <-> issn, formato largo)
#            input/temp/dic-revistas.rds      (una fila por revista)
#            output/catalogo-revistas.csv
#
# Logica de identificacion de revistas:
#   1) De cada fila se extraen todos los ISSN disponibles (issn, issn_p,
#      issn_e; el primero puede traer varios separados por ";").
#   2) Dos ISSN que aparecen juntos en una misma publicacion pertenecen a
#      la misma revista. Con Union-Find se agrupan en componentes conexas,
#      lo que une los casos en que una revista aparece unas veces solo con
#      el ISSN impreso y otras solo con el electronico.
#   3) Las filas sin ningun ISSN se agrupan por nombre de revista.
#   4) Cada revista recibe un `issn_canonico` legible con todos sus ISSN.
# =====================================================================


## ---------------------------------------------------------------------
## 1. CONSOLIDACION DE LAS DOS FUENTES
## ---------------------------------------------------------------------
## Ambas bases estan en formato largo (una fila por autor x publicacion) y
## ya comparten vocabulario de tipo documental, jerarquia y departamento,
## porque se recodificaron con las mismas funciones en las etapas previas.

sepavid <- readRDS(ruta_temp("sepavid-publicaciones.rds")) |>
  transmute(titulo, revista, anio, doi, tipo_documento,
            issn = NA_character_, issn_p, issn_e,
            rut, nombre_completo, sexo, edad, horas_reales,
            reparticion, departamento, jerarquia,
            num_autor)

orcid <- readRDS(ruta_temp("orcid-publicaciones.rds")) |>
  transmute(titulo, revista, anio, doi, tipo_documento,
            issn, issn_p = NA_character_, issn_e = NA_character_,
            rut, nombre_completo, sexo, edad, horas_reales,
            reparticion, departamento, jerarquia,
            num_autor = NA_integer_)

base_consolidada <- bind_rows(SEPAVID = sepavid, ORCID = orcid, .id = "fuente") |>
  mutate(clave_pub = clave_publicacion(doi, titulo))

# Una misma publicacion puede venir por las dos vias. Se conserva la
# version de SEPAVID (aparece primero en el bind) para cada par
# publicacion-autor.
n_antes <- nrow(base_consolidada)
base_consolidada <- base_consolidada |>
  distinct(clave_pub, rut, .keep_all = TRUE) |>
  # id_fila identifica la FILA (autor x publicacion); clave_pub identifica
  # la PUBLICACION. Se usan para propagar atributos sin ambiguedad.
  mutate(id_fila = row_number(), .before = 1)

message("  Filas consolidadas: ", nrow(base_consolidada),
        " (se eliminaron ", n_antes - nrow(base_consolidada),
        " duplicados entre fuentes)")


## ---------------------------------------------------------------------
## 2. TABLA LARGA DE ISSN POR FILA
## ---------------------------------------------------------------------

cols_issn <- c("issn", "issn_p", "issn_e")

issn_por_fila <- base_consolidada |>
  select(id_fila, all_of(cols_issn)) |>
  pivot_longer(all_of(cols_issn), values_to = "issn") |>
  separate_longer_delim(issn, delim = ";") |>
  mutate(issn = norm_issn(issn)) |>
  filter(!is.na(issn)) |>
  distinct(id_fila, issn)


## ---------------------------------------------------------------------
## 3. UNION-FIND SOBRE EL CONJUNTO DE ISSN
## ---------------------------------------------------------------------

#' Agrupa elementos en componentes conexas.
#'
#' @param grupos lista de vectores; los elementos de un mismo vector se
#'   consideran conectados entre si.
#' @return tibble con una fila por elemento y su numero de componente.
componentes_conexas <- function(grupos) {
  elementos <- sort(unique(unlist(grupos, use.names = FALSE)))
  padre <- seq_along(elementos)

  # Busqueda con compresion de camino
  buscar <- function(i) {
    while (padre[i] != i) {
      padre[i] <<- padre[padre[i]]
      i <- padre[i]
    }
    i
  }
  unir <- function(a, b) {
    ra <- buscar(a); rb <- buscar(b)
    if (ra != rb) padre[ra] <<- rb
  }

  for (g in grupos) {
    if (length(g) < 2) next
    idx <- match(g, elementos)
    for (j in idx[-1]) unir(idx[1], j)
  }

  raices <- vapply(seq_along(elementos), buscar, integer(1))
  tibble(elemento   = elementos,
         componente = match(raices, unique(raices)))
}

revistas_con_issn <- componentes_conexas(split(issn_por_fila$issn,
                                               issn_por_fila$id_fila)) |>
  transmute(issn = elemento,
            revista_id = sprintf("REV-%04d", componente))


## ---------------------------------------------------------------------
## 4. ASIGNAR revista_id A CADA FILA
## ---------------------------------------------------------------------

# 4.1 Filas con ISSN: por construccion, todos los ISSN de una fila caen en
#     la misma componente, de modo que el resultado es unico por fila.
id_por_fila <- issn_por_fila |>
  left_join(revistas_con_issn, by = "issn") |>
  distinct(id_fila, revista_id)

stopifnot(!anyDuplicated(id_por_fila$id_fila))

# 4.2 Filas sin ISSN: se agrupan por nombre de revista normalizado.
sin_issn <- base_consolidada |>
  anti_join(id_por_fila, by = "id_fila") |>
  transmute(id_fila, revista_norm = norm_texto(revista)) |>
  filter(!is.na(revista_norm))

nombres_revista <- sort(unique(sin_issn$revista_norm))

id_por_fila <- sin_issn |>
  transmute(id_fila,
            revista_id = sprintf("REVNOM-%04d",
                                 match(revista_norm, nombres_revista))) |>
  bind_rows(id_por_fila)

# Las filas sin ISSN y sin nombre de revista (tipico de libros) quedan con
# revista_id = NA y, por lo tanto, sin indexacion ni metricas de revista.
base_consolidada <- base_consolidada |>
  left_join(id_por_fila, by = "id_fila")


## ---------------------------------------------------------------------
## 5. DICCIONARIO DE REVISTAS
## ---------------------------------------------------------------------

# revista_id <-> issn: llave que usan las etapas 04, 05 y 06.
revistas_issn <- issn_por_fila |>
  left_join(id_por_fila, by = "id_fila") |>
  filter(!is.na(revista_id)) |>
  distinct(revista_id, issn)

saveRDS(revistas_issn, ruta_temp("revistas-issn.rds"))

# ISSN canonico: todos los ISSN de la revista, ordenados y legibles.
issn_canonico <- revistas_issn |>
  arrange(revista_id, issn) |>
  summarise(issn_canonico = paste(issn, collapse = "; "), .by = revista_id)

# Nombre de referencia: el mas frecuente entre las variantes observadas.
nombre_revista <- base_consolidada |>
  filter(!is.na(revista_id), !is.na(revista)) |>
  count(revista_id, revista, sort = TRUE) |>
  slice(1, .by = revista_id) |>
  select(revista_id, revista)

dic_revistas <- id_por_fila |>
  distinct(revista_id) |>
  left_join(nombre_revista, by = "revista_id") |>
  left_join(issn_canonico,  by = "revista_id") |>
  arrange(revista_id)

saveRDS(dic_revistas, ruta_temp("dic-revistas.rds"))
write_csv(dic_revistas, ruta_output("catalogo-revistas.csv"))

base_consolidada <- base_consolidada |>
  left_join(issn_canonico, by = "revista_id")

saveRDS(base_consolidada, ruta_temp("base-consolidada.rds"))


## ---------------------------------------------------------------------
## 6. VERIFICACIONES
## ---------------------------------------------------------------------

message("  Revistas identificadas por ISSN   : ",
        sum(str_starts(dic_revistas$revista_id, "REV-")))
message("  Revistas identificadas por nombre : ",
        sum(str_starts(dic_revistas$revista_id, "REVNOM-")))
message("  Filas sin revista_id (sin ISSN ni nombre): ",
        sum(is.na(base_consolidada$revista_id)))
