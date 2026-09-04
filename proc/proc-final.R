# =====================================================================
# proc-final.R  |  MASTERSCRIPT
#
# Ejecuta el pipeline completo de produccion academica de la facultad y
# genera los tres productos finales.
#
# FLUJO
#   01-sepavid      publicaciones declaradas en SEPAVID + planta academica
#   02-orcid        publicaciones recuperadas desde ORCID/Crossref/OpenAlex
#   03-id-revistas  consolidacion de ambas fuentes + identificador de revista
#   04-indexaciones catalogos WoS/Scopus/SciELO -> indexacion por revista
#   05-idiomas      idioma de publicacion y acceso abierto por revista
#   06-scopus       citas y resumen por DOI; SJR y cuartil por revista
#   07-coautores    listado COMPLETO de autores (FACSO y externos) con
#                   afiliacion y pais, via OpenAlex; base para redes de
#                   coautoria y colaboracion internacional
#   (este script)   integracion, filtro final y productos
#
# PRODUCTOS  (carpeta output/)
#   base-final.rdata         base larga: una fila por autor x publicacion
#   consolidado-depto.rdata  una fila por publicacion x departamento
#   consolidado-wide.rdata   una fila por publicacion, autores en columnas
#   coautores.rdata          una fila por autor x publicacion (FACSO y
#                             externos), con afiliacion y pais (etapa 07)
#   catalogo-revistas.csv    diccionario de revistas (etapa 03)
#
# COMO EJECUTAR
#   1. Abrir el proyecto (.Rproj) para que el directorio de trabajo sea la
#      raiz. Todas las rutas del pipeline son relativas a ella.
#   2. Dejar los insumos en input/ (ver README.md).
#   3. Definir SCOPUS_API_KEY en .Renviron si se quieren citas y resumenes.
#   4. source("proc-final.R")
#
# Con FORZAR_API = FALSE la corrida reutiliza las descargas guardadas en
# input/temp/ y toma minutos. Con TRUE vuelve a consultar todas las APIs y puede
# tardar varias horas.
# =====================================================================

rm(list = ls())


## ---------------------------------------------------------------------
## 1. PARAMETROS DE LA CORRIDA
## ---------------------------------------------------------------------

FORZAR_API  <- FALSE   # TRUE = reconsultar ORCID, SciELO y Scopus
ANIO_INICIO <- 2016
ANIO_FIN    <- 2025

source("proc/00-funciones.R", encoding = "UTF-8")


## ---------------------------------------------------------------------
## 2. EJECUCION DE LAS ETAPAS
## ---------------------------------------------------------------------

etapas <- c(
  "proc/01-sepavid.R",
  "proc/02-orcid.R",
  "proc/03-id-revistas.R",
  "proc/04-indexaciones.R",
  "proc/05-idiomas.R",
  "proc/06-scopus.R",
  "proc/07-coautores.R"
)

inicio <- Sys.time()

for (etapa in etapas) {
  message("\n=== ", etapa, " ", strrep("=", max(0, 50 - nchar(etapa))))
  source(etapa, encoding = "UTF-8", local = new.env())
}

message("\n=== proc-final.R ", strrep("=", 36))


## ---------------------------------------------------------------------
## 3. INTEGRACION DE TODAS LAS ETAPAS
## ---------------------------------------------------------------------
## Los atributos de revista se pegan por `revista_id` y los de publicacion
## por `doi`. La indexacion solo tiene sentido para articulos de revista:
## libros y capitulos quedan en NA.

base_final <- readRDS(ruta_temp("base-consolidada.rds")) |>
  left_join(readRDS(ruta_temp("indexacion-revista.rds")) |>
              select(revista_id, indexacion), by = "revista_id") |>
  left_join(readRDS(ruta_temp("idioma-revista.rds")),  by = "revista_id") |>
  left_join(readRDS(ruta_temp("sjr-revista.rds")),     by = "revista_id") |>
  left_join(readRDS(ruta_temp("scopus-publicaciones.rds")), by = "doi") |>
  mutate(indexacion = if_else(tipo_documento == "journal-article",
                              indexacion, NA_character_))


## ---------------------------------------------------------------------
## 4. FILTRO FINAL
## ---------------------------------------------------------------------
## Se conservan:
##   - todos los articulos de revista;
##   - los libros y capitulos validados en ORCID por su autor.
## SEPAVID sobrerregistra libros (mismo volumen declarado como libro y
## como capitulo, o material que no corresponde), por lo que el registro
## ORCID del propio academico opera como criterio de validacion.

libros_validados <- readRDS(ruta_temp("orcid-libros.rds"))$clave_pub

base_final <- base_final |>
  filter(tipo_documento == "journal-article" | clave_pub %in% libros_validados)

save(base_final, file = ruta_output("base-final.rdata"))


## ---------------------------------------------------------------------
## 5. CONSOLIDADO POR DEPARTAMENTO
## ---------------------------------------------------------------------
## Una fila por publicacion y departamento: una coautoria entre dos
## departamentos cuenta una vez en cada uno, pero no se duplica por cada
## coautor del mismo departamento.

consolidado_depto <- base_final |>
  distinct(clave_pub, departamento, .keep_all = TRUE) |>
  select(clave_pub, doi, anio, departamento, tipo_documento, indexacion,
         titulo, revista, revista_id, issn_p, issn_e, issn, issn_canonico,
         idioma, oa, n_citas = scopus_citas, quartil = cuartil_sjr, sjr)

save(consolidado_depto, file = ruta_output("consolidado-depto.rdata"))


## ---------------------------------------------------------------------
## 6. COAUTORES COMPLETOS (FACSO y externos)
## ---------------------------------------------------------------------
## A diferencia de autor_1..n (mas abajo), que solo trae RUT de planta
## FACSO, esta es la base para redes de coautoria y colaboracion
## internacional: una fila por autor x publicacion, sea o no de la
## facultad, con su afiliacion y pais (etapa 07, via OpenAlex). Solo cubre
## las publicaciones con DOI resoluble en OpenAlex.

## coautores.rds se arma sobre base-consolidada.rds (etapa 3), previo al
## FILTRO FINAL de la seccion 4 (que descarta libros/capitulos no validados
## por ORCID). Se acota aqui a las publicaciones que sí quedan en
## base_final, para que describa exactamente el mismo universo que el resto
## de los productos.
coautores <- readRDS(ruta_temp("coautores.rds")) |>
  filter(clave_pub %in% base_final$clave_pub)

save(coautores, file = ruta_output("coautores.rdata"))

# Resumen por publicacion, para pegar en consolidado_wide. n_paises = 0 y
# colaboracion_internacional = NA cuando la publicacion no tiene fila en
# coautores (sin DOI, o OpenAlex no la encontro): "se desconoce", no "no hay
# colaboracion".
paises_pub <- coautores |>
  reframe(pais = unique(na.omit(unlist(str_split(pais_iso, "; ")))), .by = clave_pub) |>
  summarise(
    n_paises = n(),
    paises   = paste(sort(pais), collapse = "; "),
    colaboracion_internacional = any(pais != "CL"),
    .by = clave_pub
  )

resumen_coautores <- coautores |>
  summarise(n_autores_total = n(), .by = clave_pub) |>
  left_join(paises_pub, by = "clave_pub") |>
  mutate(
    n_paises = coalesce(n_paises, 0L),
    colaboracion_internacional = if_else(n_paises == 0, NA, colaboracion_internacional)
  )


## ---------------------------------------------------------------------
## 7. CONSOLIDADO ANCHO
## ---------------------------------------------------------------------
## Una fila por publicacion, con los RUT de los coautores de la facultad
## en columnas autor_1 ... autor_n (sin cambios respecto de antes), mas un
## resumen de la coautoria completa (n_autores_total, paises,
## colaboracion_internacional) tomado de la tabla larga de la seccion 6.
##
## Se arma en dos pasos (atributos por un lado, autores por otro) porque
## un pivot_wider directo generaria filas duplicadas cada vez que dos
## fuentes registran el mismo titulo con diferencias menores de formato.

autores_wide <- base_final |>
  distinct(clave_pub, rut) |>
  mutate(autor_n = paste0("autor_", row_number()), .by = clave_pub) |>
  pivot_wider(id_cols = clave_pub, names_from = autor_n, values_from = rut)

atributos_pub <- base_final |>
  slice(1, .by = clave_pub) |>
  select(clave_pub, doi, anio, tipo_documento, indexacion, titulo, revista,
         revista_id, issn, issn_e, issn_p, issn_canonico, idioma, oa,
         quartil = cuartil_sjr, sjr, n_citas = scopus_citas,
         abstract = scopus_abstract, keywords = scopus_keywords)

consolidado_wide <- atributos_pub |>
  left_join(autores_wide,      by = "clave_pub") |>
  left_join(resumen_coautores, by = "clave_pub") |>
  # Ordena las columnas de autor de forma natural (autor_1, autor_2, ...)
  relocate(any_of(paste0("autor_", 1:50)), .after = last_col())

save(consolidado_wide, file = ruta_output("consolidado-wide.rdata"))


## ---------------------------------------------------------------------
## 8. VERIFICACIONES Y REGISTRO DE LA CORRIDA
## ---------------------------------------------------------------------

message("\n--- Resumen de la corrida ---")
message("Filas autor x publicacion : ", nrow(base_final))
message("Publicaciones unicas      : ", n_distinct(base_final$clave_pub))
message("Duracion                  : ",
        round(difftime(Sys.time(), inicio, units = "mins"), 1), " min")
message("Publicaciones con coautoria completa (OpenAlex) : ",
        n_distinct(coautores$clave_pub), " de ", n_distinct(base_final$clave_pub))
message("Publicaciones con colaboracion internacional    : ",
        sum(consolidado_wide$colaboracion_internacional, na.rm = TRUE))

print(count(base_final, tipo_documento, indexacion))
print(count(consolidado_depto, departamento, anio))

# Registro del entorno para poder reproducir la corrida mas adelante.
writeLines(capture.output(sessionInfo()), ruta_output("session-info.txt"))
