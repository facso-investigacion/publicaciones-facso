# =====================================================================
# exportar-bibliometrix.R
#
# Convierte output/consolidado-wide.rdata en un data frame con la
# estructura estandar de bibliometrix (clase "bibliometrixDB"), listo
# para usarse con biblioAnalysis(), biblioshiny() y el resto de
# funciones del paquete sin pasar por convert2df(). Incluye afiliacion
# (C1) y pais por autor, para redes de coautoria y de colaboracion
# institucional/internacional (biblioNetwork(..., network = "countries")).
#
# LIMITACIONES DE LOS DATOS DE ORIGEN
#   - AU y C1 traen el listado COMPLETO de autores (FACSO y externos) solo
#     para las publicaciones que output/coautores.rdata pudo resolver via
#     OpenAlex (requiere DOI resoluble; cubre ~85% del corpus, ver mensaje
#     final). Para el resto, AU cae de vuelta a los coautores FACSO de
#     autor_1..n (como en la version anterior de este script) y C1 queda
#     NA: esas publicaciones no aportan a las redes de colaboracion.
#   - No hay lista de referencias citadas (CR), por lo que el analisis de
#     co-citacion no esta disponible.
#   - AU1_CO (pais del primer autor) queda casi siempre NA: es una
#     limitacion de metaTagExtraction(M, "AU1_CO") en si (su regex espera un
#     espacio inicial antes del pais que trim() elimina), no de estos datos.
#     AU_CO (pais de TODOS los autores, la que alimenta
#     biblioNetwork(..., network = "countries")) funciona bien.
#
# ENTRADA   output/consolidado-wide.rdata (consolidado_wide)
#           output/base-final.rdata       (para resolver rut -> nombre FACSO)
#           output/coautores.rdata        (listado completo de autores,
#                                          etapa 07 del pipeline)
# SALIDA    output/consolidado-bibliometrix.rdata  (objeto M)
#
# USO
#   source("proc/exportar-bibliometrix.R")
  library(bibliometrix)
  load("output/consolidado-bibliometrix.rdata")
  resultados <- biblioAnalysis(M)
  summary(resultados)
  biblioNetwork(M, analysis = "collaboration", network = "countries")
  # o bien, dentro de biblioshiny(): Load Data > File (.rdata) > M
# =====================================================================

source("proc/00-funciones.R", encoding = "UTF-8")

if (!requireNamespace("bibliometrix", quietly = TRUE)) {
  install.packages("bibliometrix")
}
library(bibliometrix)

load(ruta_output("consolidado-wide.rdata"))
load(ruta_output("base-final.rdata"))
load(ruta_output("coautores.rdata"))


## ---------------------------------------------------------------------
## 1. Nombre de autor -> formato "APELLIDOS INICIALES"
## ---------------------------------------------------------------------
## Para autores FACSO se usa el nombre_completo oficial (base_final), mas
## confiable que lo que reporte OpenAlex. partir_nombre() (00-funciones.R)
## toma las 2 ultimas palabras como apellidos: vale para nombres chilenos
## (nombre(s) + paterno + materno) y degrada razonablemente para nombres
## de 2 palabras (convencion "Nombre Apellido").

formatear_autor <- function(nombre) {
  p <- partir_nombre(nombre)
  str_to_upper(str_squish(paste(p$apellidos, paste(p$iniciales, collapse = ""))))
}

rut_a_nombre <- base_final |>
  distinct(rut, nombre_completo) |>
  filter(!is.na(nombre_completo)) |>
  mutate(autor_fmt = map_chr(nombre_completo, formatear_autor)) |>
  distinct(rut, .keep_all = TRUE) |>
  select(rut, autor_fmt)


## ---------------------------------------------------------------------
## 2. Pais ISO-2 -> nombre tal como lo espera bibliometrix
## ---------------------------------------------------------------------
## AU_CO()/AU_UN() (funciones internas de bibliometrix) buscan, dentro de
## C1, el nombre de pais tal como aparece en su propio catalogo interno
## data(countries) -- se usa esa misma tabla como puente desde el
## country_code (ISO-2) que entrega OpenAlex, para no arriesgar un
## desajuste de ortografia/alias.

data(countries, package = "bibliometrix")
paises_bibliometrix <- countries |> distinct(iso2, countries) |> deframe()

#' Un fragmento de C1 por cada afiliacion del autor (no uno solo por autor):
#' cuando un autor tiene mas de una institucion, bibliometrix espera el tag
#' "[APELLIDOS INICIALES]" repetido una vez por afiliacion, cada una con su
#' propio pais -- juntar todas las instituciones en un solo fragmento
#' (separadas por ";", el mismo separador que bibliometrix usa entre
#' autores) rompe AU_UN()/AU1_CO() porque ya no hay un fragmento por autor.
#' institucion y pais_iso vienen alineados desde coautores.rdata (etapa 07):
#' institucion[k] es la afiliacion pais_iso[k].
construir_c1_autor <- function(nombre, institucion, pais_iso) {
  if (is.na(institucion) && is.na(pais_iso)) return(paste0("[", nombre, "] "))
  insts   <- if (is.na(institucion)) character(0) else str_split(institucion, "; ")[[1]]
  codigos <- if (is.na(pais_iso))    character(0) else str_split(pais_iso,    "; ")[[1]]
  n <- max(length(insts), length(codigos))
  length(insts)   <- n
  length(codigos) <- n
  paises_nombre <- unname(paises_bibliometrix[codigos])
  fragmentos <- map2_chr(insts, paises_nombre, function(i, p) {
    partes <- c(i, p)
    partes <- partes[!is.na(partes) & partes != ""]
    paste(partes, collapse = ", ")
  })
  paste0("[", nombre, "] ", fragmentos, collapse = ";")
}


## ---------------------------------------------------------------------
## 3. AU y C1: listado completo de autores (FACSO y externos)
## ---------------------------------------------------------------------
## coautores.rdata trae, por autor x publicacion, su afiliacion y pais
## (etapa 07, via OpenAlex). Los autores FACSO usan el nombre oficial
## (rut_a_nombre); el resto, el mismo heuristico aplicado a nombre_autor.

autores_completos <- coautores |>
  left_join(rut_a_nombre, by = "rut") |>
  mutate(
    nombre_fmt = if_else(es_facso & !is.na(autor_fmt), autor_fmt,
                         map_chr(nombre_autor, formatear_autor)),
    # bibliometrix::AU_UN() busca palabras clave (UNIV, INST, CTR, ...)
    # dentro de C1 con comparacion sensible a mayusculas: si la institucion
    # queda en minusculas (tal como la entrega OpenAlex) nunca calza.
    c1_entry = str_to_upper(pmap_chr(list(nombre_fmt, institucion, pais_iso),
                                     construir_c1_autor))
  ) |>
  arrange(clave_pub, orden_autor)

AU_completo <- autores_completos |>
  summarise(
    AU = paste(nombre_fmt, collapse = ";"),
    C1 = paste(c1_entry, collapse = ";"),
    .by = clave_pub
  )

## Publicaciones sin fila en coautores.rdata (sin DOI, o OpenAlex no la
## encontro): mismo comportamiento que la version anterior del script --
## AU solo con los coautores FACSO de autor_1..n, sin C1.

cols_autor <- names(consolidado_wide)[str_starts(names(consolidado_wide), "autor_")]

AU_facso_solo <- consolidado_wide |>
  filter(!clave_pub %in% AU_completo$clave_pub) |>
  select(clave_pub, all_of(cols_autor)) |>
  pivot_longer(all_of(cols_autor), values_to = "rut") |>
  filter(!is.na(rut)) |>
  left_join(rut_a_nombre, by = "rut") |>
  summarise(AU = paste(autor_fmt, collapse = ";"), .by = clave_pub) |>
  mutate(C1 = NA_character_)

autores_pub <- bind_rows(AU_completo, AU_facso_solo)


## ---------------------------------------------------------------------
## 4. Tipo de documento e idioma -> vocabulario bibliometrix
## ---------------------------------------------------------------------

dt_map <- c(
  "journal-article" = "ARTICLE",
  "book"            = "BOOK",
  "book-chapter"    = "BOOK CHAPTER"
)

la_map <- c(
  "Inglés" = "ENGLISH", "Español" = "SPANISH", "Francés" = "FRENCH",
  "Alemán" = "GERMAN", "Italiano" = "ITALIAN", "Portugués" = "PORTUGUESE",
  "Multi-idioma" = "MULTILINGUAL", "Otro" = "OTHER"
)


## ---------------------------------------------------------------------
## 5. Ensamblar el data frame en formato bibliometrix
## ---------------------------------------------------------------------
## Convencion del paquete: campos de texto en mayusculas, DOI se deja
## tal cual (bibliometrix tampoco lo convierte a mayusculas al importar
## desde Scopus).

M <- consolidado_wide |>
  left_join(autores_pub, by = "clave_pub") |>
  transmute(
    AU = AU,
    C1 = C1,
    TI = str_to_upper(titulo),
    SO = str_to_upper(revista),
    JI = str_to_upper(revista),
    DT = unname(dt_map[tipo_documento]),
    DE = str_to_upper(keywords),
    AB = str_to_upper(abstract),
    PY = anio,
    TC = n_citas,
    DI = doi,
    LA = unname(la_map[idioma]),
    UT = str_to_upper(clave_pub),
    DB = "FACSO",
    # Atributos propios del proyecto, fuera del estandar bibliometrix:
    FACSO_INDEXACION = indexacion,
    FACSO_QUARTIL     = quartil,
    FACSO_SJR         = sjr,
    FACSO_OA          = oa,
    FACSO_N_PAISES    = n_paises,
    FACSO_COLAB_INTL  = colaboracion_internacional,
    FACSO_CLAVE_PUB   = clave_pub
  ) |>
  filter(!is.na(AU)) |>
  as.data.frame()

M <- metaTagExtraction(M, "SR")
M <- metaTagExtraction(M, "AU_UN")   # institucion por autor, desde C1
M <- metaTagExtraction(M, "AU_CO")   # pais por autor, desde C1
M <- metaTagExtraction(M, "AU1_CO")  # pais del primer autor
row.names(M) <- M$SR
class(M) <- c("bibliometrixDB", "data.frame")
attr(M, "db") <- "FACSO"

save(M, file = ruta_output("consolidado-bibliometrix.rdata"))

message("Publicaciones exportadas a bibliometrix : ", nrow(M),
        " (de ", nrow(consolidado_wide), " en consolidado_wide).")
message("Con coautoria completa (AU + C1)        : ", sum(!is.na(M$C1)),
        " (", sum(is.na(M$C1)), " solo con coautores FACSO, sin C1).")
message("Con pais identificado (AU_CO)           : ", sum(!is.na(M$AU_CO) & M$AU_CO != ""))
message("Guardado en: ", ruta_output("consolidado-bibliometrix.rdata"))
