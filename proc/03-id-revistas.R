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
# Entradas : input/temp/sepavid-publicaciones.rds   (etapa 1)
#            input/temp/orcid-publicaciones.rds     (etapa 2)
#            input/temp/openalex-publicaciones.rds  (etapa 2c)
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
## 1. CONSOLIDACION DE LAS TRES FUENTES
## ---------------------------------------------------------------------
## Las tres bases estan en formato largo (una fila por autor x publicacion)
## y ya comparten vocabulario de tipo documental, jerarquia y departamento,
## porque se recodificaron con las mismas funciones en las etapas previas.

sepavid <- readRDS(ruta_temp("sepavid-publicaciones.rds")) |>
  transmute(titulo, revista, anio, doi, tipo_documento,
            issn = NA_character_, issn_p, issn_e,
            rut, nombre_completo, sexo, edad, horas_reales,
            reparticion, departamento, jerarquia,
            num_autor)

orcid <- readRDS(ruta_temp("orcid-publicaciones.rds"))
# orcid-publicaciones.rds anterior a la seccion 5b de 02-orcid.R no trae la marca
if (!"orcid_sin_doi" %in% names(orcid)) orcid$orcid_sin_doi <- FALSE
orcid <- orcid |>
  transmute(titulo, revista, anio, doi, tipo_documento,
            issn, issn_p = NA_character_, issn_e = NA_character_,
            rut, nombre_completo, sexo, edad, horas_reales,
            reparticion, departamento, jerarquia,
            num_autor = NA_integer_,
            orcid_sin_doi = coalesce(orcid_sin_doi, FALSE))

openalex <- readRDS(ruta_temp("openalex-publicaciones.rds")) |>
  transmute(titulo, revista, anio, doi, tipo_documento,
            issn, issn_p = NA_character_, issn_e = NA_character_,
            rut, nombre_completo, sexo, edad, horas_reales,
            reparticion, departamento, jerarquia,
            num_autor = NA_integer_)

base_consolidada <- bind_rows(SEPAVID = sepavid, ORCID = orcid, OpenAlex = openalex,
                              .id = "fuente") |>
  mutate(doi = norm_doi(doi),
         clave_pub = clave_publicacion(doi, titulo),
         orcid_sin_doi = coalesce(orcid_sin_doi, FALSE),
         # antes de que la seccion 1b complete DOI desde otros registros
         doi_propio = !is.na(doi))


## ---------------------------------------------------------------------
## 1b. DUPLICADOS POR TITULO SIMILAR
## ---------------------------------------------------------------------
## Una misma obra puede llegar con claves distintas: con DOI en SEPAVID y
## sin DOI en ORCID, con dos DOI distintos (reediciones de la editorial),
## o con el titulo levemente cambiado ("Portfolio" / "Portafolio"). Se
## comparan los titulos de un MISMO academico entre si (todas las fuentes y
## tipos): si son casi iguales y los anios difieren a lo sumo en 1, se
## consideran la misma obra. Salvaguardas contra falsos positivos:
##   - solo dentro de un academico (no fusiona titulos genericos de autores
##     distintos) y con largo minimo (no fusiona "Introduccion");
##   - similitud por distancia de edicion (Levenshtein normalizada), no
##     Jaro-Winkler: JW premia el prefijo comun y fusionaba series del mismo
##     autor ("...guidelines for suicide" / "...for psychosis");
##   - si los titulos traen numeros distintos no se fusionan ("Testigos de
##     una epoca 1" / "3", "Dossier N 7" / "N 8");
##   - si un titulo REEMPLAZA palabras de contenido del otro por palabras
##     distintas, no se fusionan ("...guidelines for suicide risk" /
##     "...for psychosis", "(FIRST PART)" / "(SECOND PART)"). Si se toleran
##     palabras agregadas u omitidas ("Future prospects" / "Prospects"),
##     variantes ortograficas ("comentadores" / "comentarios") y cambios de
##     palabras vacias ("between" / "from");
##   - se quitan prefijos de capitulo ("Chapter Six -", "Capitulo 4.")
##     antes de comparar.
## Traducciones del mismo trabajo (titulo en ingles vs espanol) NO se
## detectan: se prefiere perder esa fusion a fusionar obras distintas.
##
## Las claves equivalentes se agrupan con Union-Find (asi una obra que dos
## academicos FACSO declararon con claves distintas queda con una sola
## clave para todos) y cada grupo toma la clave de su fila prioritaria:
## SEPAVID primero; despues, la que trae DOI (de ahi salen ISSN e indexacion);
## a igual condicion, ORCID antes que OpenAlex.
## Como SEPAVID queda primero, tambien prevalece su tipo documental.

umbral_titulo <- 0.90
largo_minimo_titulo <- 20

norm_titulo <- function(x) {
  x |>
    quitar_tildes() |>
    str_to_lower() |>
    str_replace_all("[^a-z0-9 ]", " ") |>
    str_squish() |>
    str_remove("^(chapter|capitulo|cap) [a-z0-9]+ ") |>
    na_if("")
}

palabras_vacias <- c("a", "al", "and", "between", "con", "da", "de", "del", "do",
                     "e", "el", "en", "entre", "for", "from", "in", "la", "las",
                     "los", "o", "of", "on", "para", "por", "the", "to", "with", "y")

#' TRUE si entre dos titulos hay una sustitucion de palabras de contenido:
#' ambos tienen palabras que el otro no tiene y alguna no se parece a
#' ninguna del otro lado (similitud Levenshtein < 0.6).
sustituye_palabras <- function(a, b) {
  pa <- setdiff(str_split_1(a, " "), palabras_vacias)
  pb <- setdiff(str_split_1(b, " "), palabras_vacias)
  solo_a <- setdiff(pa, pb)
  solo_b <- setdiff(pb, pa)
  if (length(solo_a) == 0 || length(solo_b) == 0) return(FALSE)
  sin_pareja <- function(x, y) any(vapply(x, \(w) max(stringsim(w, y, method = "lv")) < 0.6, logical(1)))
  sin_pareja(solo_a, solo_b) || sin_pareja(solo_b, solo_a)
}

filas_titulo <- base_consolidada |>
  transmute(rut, clave_pub, anio, titulo_norm = norm_titulo(titulo)) |>
  filter(!is.na(rut), !is.na(anio), nchar(titulo_norm) >= largo_minimo_titulo) |>
  distinct() |>
  mutate(numeros = map_chr(str_extract_all(titulo_norm, "[0-9]+"), str_c, collapse = " "))

pares_titulo <- filas_titulo |>
  inner_join(filas_titulo, by = "rut", suffix = c("", "_b"),
             relationship = "many-to-many") |>
  filter(clave_pub < clave_pub_b, abs(anio - anio_b) <= 1, numeros == numeros_b) |>
  mutate(similitud = 1 - stringdist(titulo_norm, titulo_norm_b, method = "lv") /
                         pmax(nchar(titulo_norm), nchar(titulo_norm_b))) |>
  filter(similitud >= umbral_titulo,
         !map2_lgl(titulo_norm, titulo_norm_b, sustituye_palabras))

prioridad_fuente <- c(SEPAVID = 1L, ORCID = 2L, OpenAlex = 3L)

clave_canonica <- componentes_conexas(map2(pares_titulo$clave_pub, pares_titulo$clave_pub_b, c)) |>
  inner_join(base_consolidada |> distinct(clave_pub, fuente, doi),
             by = c("elemento" = "clave_pub"), relationship = "one-to-many") |>
  arrange(componente, fuente != "SEPAVID", is.na(doi), prioridad_fuente[fuente]) |>
  mutate(clave_nueva = first(elemento),
         doi_grupo   = first(na.omit(doi)),
         .by = componente) |>
  distinct(clave_pub = elemento, clave_nueva, doi_grupo)

# Registro para revision: que obras se fusionaron y con que similitud.
duplicados_titulo <- pares_titulo |>
  left_join(base_consolidada |> distinct(rut, clave_pub, .keep_all = TRUE) |>
              select(rut, clave_pub, nombre_completo, fuente, tipo_documento, titulo),
            by = c("rut", "clave_pub")) |>
  left_join(base_consolidada |> distinct(rut, clave_pub, .keep_all = TRUE) |>
              select(rut, clave_pub_b = clave_pub, fuente_b = fuente,
                     tipo_documento_b = tipo_documento, titulo_b = titulo),
            by = c("rut", "clave_pub_b")) |>
  transmute(nombre_completo, similitud = round(similitud, 3), anio, anio_b,
            fuente, tipo_documento, clave_pub, titulo,
            fuente_b, tipo_documento_b, clave_pub_b, titulo_b) |>
  arrange(nombre_completo, desc(similitud))

write_excel_csv(duplicados_titulo, ruta_output("duplicados-titulo.csv"), na = "")

base_consolidada <- base_consolidada |>
  left_join(clave_canonica, by = "clave_pub") |>
  mutate(clave_pub = coalesce(clave_nueva, clave_pub),
         doi       = coalesce(doi, doi_grupo)) |>
  select(-clave_nueva, -doi_grupo)

message("  Duplicados por titulo similar: ", nrow(pares_titulo), " pares, ",
        n_distinct(clave_canonica$clave_pub) - n_distinct(clave_canonica$clave_nueva),
        " claves fusionadas (ver output/duplicados-titulo.csv)")


## ---------------------------------------------------------------------
## 1c. ARTICULOS ORCID SIN DOI CON TITULO TRADUCIDO
## ---------------------------------------------------------------------
## ORCID suele importar desde Scopus/WoS el titulo en ingles (o los dos
## idiomas pegados) de un articulo que SEPAVID/OpenAlex tienen en espanol;
## la seccion 1b no los detecta. Un articulo ORCID sin DOI se descarta si el
## mismo academico ya tiene OTRO articulo (de otra via) en la misma revista
## y con anio +-1. En el diagnostico, ~85% de esos casos eran la misma obra;
## el resto (articulos distintos en la misma revista y anio) se pierde: se
## prefiere eso a duplicar.

if (any(base_consolidada$orcid_sin_doi)) {

nombre_revista_corto <- function(x) str_remove(norm_nombre_revista(x), "^(revista|journal) ")

otros_articulos <- base_consolidada |>
  filter(tipo_documento == "journal-article", !orcid_sin_doi) |>
  transmute(rut, clave_otro = clave_pub, anio_otro = anio, revista_otro = nombre_revista_corto(revista)) |>
  filter(!is.na(revista_otro), nchar(revista_otro) >= 4) |>
  distinct()

sin_doi_misma_revista <- base_consolidada |>
  filter(orcid_sin_doi) |>
  transmute(rut, clave_pub, anio, revista_sd = nombre_revista_corto(revista)) |>
  filter(!is.na(revista_sd), nchar(revista_sd) >= 4) |>
  inner_join(otros_articulos, by = "rut", relationship = "many-to-many") |>
  filter(clave_pub != clave_otro, abs(anio - anio_otro) <= 1) |>
  filter(1 - stringdist(revista_sd, revista_otro, method = "jw", p = 0.1) >= 0.9 |
           str_detect(revista_otro, fixed(revista_sd)) |
           str_detect(revista_sd, fixed(revista_otro))) |>
  distinct(rut, clave_pub)

base_consolidada |>
  semi_join(sin_doi_misma_revista, by = c("rut", "clave_pub")) |>
  filter(orcid_sin_doi) |>
  select(nombre_completo, anio, revista, titulo) |>
  arrange(nombre_completo, anio) |>
  write_excel_csv(ruta_output("orcid-sin-doi-misma-revista.csv"), na = "")

base_consolidada <- base_consolidada |>
  filter(!(orcid_sin_doi & paste(rut, clave_pub) %in%
             paste(sin_doi_misma_revista$rut, sin_doi_misma_revista$clave_pub)))

message("  Articulos ORCID sin DOI descartados por misma revista y anio: ",
        nrow(sin_doi_misma_revista), " (ver output/orcid-sin-doi-misma-revista.csv)")

} # fin del bloque `if (any(base_consolidada$orcid_sin_doi))`

base_consolidada <- base_consolidada |> select(-orcid_sin_doi)

# Una misma publicacion puede venir por varias vias. Para cada par
# publicacion-autor se conserva la version de SEPAVID; si no hay, la que
# trae DOI propio (no el completado en 1b): es la que tiene ISSN y, por lo
# tanto, revista indexable. A igual condicion, ORCID antes que OpenAlex.
n_antes <- nrow(base_consolidada)
base_consolidada <- base_consolidada |>
  arrange(fuente != "SEPAVID", !doi_propio, prioridad_fuente[fuente]) |>
  distinct(clave_pub, rut, .keep_all = TRUE) |>
  select(-doi_propio) |>
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

# componentes_conexas() vive en 00-funciones.R (tambien la usa la
# seccion 1b para agrupar duplicados por titulo).
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
