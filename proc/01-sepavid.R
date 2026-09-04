# =====================================================================
# 01-sepavid.R  |  Etapa 1 de 6
#
# Publicaciones declaradas por los academicos en SEPAVID + planta
# academica de la facultad.
#
# Entradas : input/original/publicaciones-<anio>.xlsx  (un archivo por anio)
#            input/original/acad.xlsx                  (planta academica)
# Salidas  : input/temp/acad.rds                    (un registro por RUT)
#            input/temp/sepavid-publicaciones.rds   (una fila por autor x publicacion)
#
# Nota: la clasificacion de indexacion que trae SEPAVID (columna
# `indexado_en`) NO se usa como variable final; se conserva solo para
# auditoria. La indexacion definitiva se calcula en 04-indexaciones.R
# cruzando ISSN contra los catalogos oficiales de WoS, Scopus y SciELO.
# =====================================================================


## ---------------------------------------------------------------------
## 1. PLANTA ACADEMICA
## ---------------------------------------------------------------------

verificar_archivos(ruta_input("acad.xlsx"))

acad <- read_xlsx(ruta_input("acad.xlsx")) |>
  clean_names()

verificar_columnas(
  acad,
  c("rut", "nombres", "paterno", "materno", "reparticion",
    "cargo", "sexo", "edad", "horas_reales"),
  "input/original/acad.xlsx"
)

## acad.xlsx ya trae una columna JERARQUIA ("Prof. Asociado - Categ.
## Academica Ord."), pero la variable de analisis se deriva de CARGO, que
## es el dato contractual. Se conserva la original como `jerarquia_origen`
## para poder auditar discrepancias en vez de sobrescribirla en silencio.

acad <- acad |>
  rename(jerarquia_origen = jerarquia) |>
  mutate(
    rut       = norm_rut(rut),
    materno   = limpiar_apellido(materno),
    # `apellidos` es la llave de cruce con colab.xlsx en la etapa ORCID
    apellidos = str_squish(paste(paterno, coalesce(materno, ""))),
    nombre_completo = str_squish(paste(nombres, paterno, coalesce(materno, ""))),
    departamento    = recodificar_departamento(reparticion),
    jerarquia       = recodificar_jerarquia(cargo)
  ) |>
  select(rut, nombre_completo, apellidos, sexo, edad,
         reparticion, departamento, cargo, jerarquia, jerarquia_origen,
         horas_reales) |>
  consolidar_horas()

# Ninguna reparticion deberia quedar sin departamento asignado.
sin_depto <- acad |> filter(is.na(departamento)) |> distinct(reparticion)
if (nrow(sin_depto) > 0) {
  message("  ADVERTENCIA: reparticiones sin departamento -> ",
          paste(sin_depto$reparticion, collapse = " | "))
}

saveRDS(acad, ruta_temp("acad.rds"))

message("  Academicos consolidados: ", nrow(acad))


## ---------------------------------------------------------------------
## 2. PUBLICACIONES DECLARADAS EN SEPAVID
## ---------------------------------------------------------------------
## El reporte trae ~194 columnas y las de RUT_AUTOR mezclan enteros
## (134718285) con texto (018741699K) dentro de una misma columna, porque
## el digito verificador puede ser una K. `col_types = "text"` obliga a
## readxl a leer el valor tal como esta en la celda: sin esto, la
## inferencia de tipo produce NA o notacion cientifica en algunos RUT.
## Como efecto adicional, garantiza que el bind_rows entre anios nunca
## falle por diferencias de tipo. Los tipos definitivos se fijan mas abajo.

archivos_pub <- ruta_input(sprintf("publicaciones-%d.xlsx", ANIO_INICIO:ANIO_FIN))
verificar_archivos(archivos_pub)

publicaciones <- archivos_pub |>
  map(\(archivo) read_xlsx(archivo, col_types = "text")) |>
  bind_rows() |>
  clean_names()

verificar_columnas(
  publicaciones,
  c("titulo_de_documento", "titulo_revista", "ano", "doi",
    "tipo_documento", "indexado_en", "issn_p", "issn_e"),
  "input/original/publicaciones-*.xlsx"
)


## ---------------------------------------------------------------------
## 3. DE ANCHO A LARGO: UNA FILA POR AUTOR
## ---------------------------------------------------------------------
## SEPAVID entrega hasta 25 columnas rut_autor1 ... rut_autor25. Se pasan
## a formato largo para poder cruzar cada autor con la planta academica.

patron_rut_autor <- "^rut_autor([1-9]|1[0-9]|2[0-5])$"

sepavid_long <- publicaciones |>
  mutate(across(matches(patron_rut_autor), norm_rut)) |>
  pivot_longer(
    cols         = matches(patron_rut_autor),
    names_to     = "num_autor",
    values_to    = "rut",
    names_prefix = "rut_autor"
  ) |>
  filter(!is.na(rut)) |>
  transmute(
    titulo         = str_squish(titulo_de_documento),
    revista        = str_squish(titulo_revista),
    anio           = suppressWarnings(as.integer(ano)),
    doi            = norm_doi(doi),
    tipo_origen    = tipo_documento,   # valor tal como viene de SEPAVID
    tipo_documento = recodificar_tipo_doc(tipo_documento),
    indexado_en,                       # solo para auditoria posterior
    issn_p         = norm_issn(issn_p),
    issn_e         = norm_issn(issn_e),
    rut,
    num_autor      = as.integer(num_autor)
  )

## Tipos documentales observados en SEPAVID: Artículo, Capítulo de libro,
## Libro, Revisión, Revisión de libros, Material Editorial, Resumen de
## reunión, Nota. Solo los tres primeros entran al analisis; el resto queda
## con tipo_documento = NA y se descarta aqui, igual que en la version
## anterior del pipeline.
##
## OJO: "Revisión" (articulos de revision) representa cerca del 5% del
## reporte anual y queda fuera. Si se decidiera incluirla, basta agregar
## el patron en recodificar_tipo_doc() dentro de 00-funciones.R.

descartados <- sepavid_long |>
  filter(is.na(tipo_documento)) |>
  count(tipo_origen, sort = TRUE)

if (nrow(descartados) > 0) {
  message("  Filas descartadas por tipo documental fuera de alcance: ",
          sum(descartados$n))
  print(descartados)
}

sepavid_long <- sepavid_long |>
  filter(!is.na(tipo_documento),
         between(anio, ANIO_INICIO, ANIO_FIN))


## ---------------------------------------------------------------------
## 4. CRUCE CON LA PLANTA ACADEMICA
## ---------------------------------------------------------------------
## inner_join: solo interesan las publicaciones de academicos vigentes de
## la facultad. El filtro por jerarquia excluye ayudantes y contratos en
## evaluacion.

sepavid_publicaciones <- sepavid_long |>
  inner_join(acad, by = "rut") |>
  filter(jerarquia %in% JERARQUIAS_VALIDAS)

saveRDS(sepavid_publicaciones, ruta_temp("sepavid-publicaciones.rds"))


## ---------------------------------------------------------------------
## 5. VERIFICACIONES
## ---------------------------------------------------------------------

message("  Filas autor x publicacion (SEPAVID): ", nrow(sepavid_publicaciones))
message("  Publicaciones unicas: ",
        n_distinct(clave_publicacion(sepavid_publicaciones$doi,
                                     sepavid_publicaciones$titulo)))
print(count(sepavid_publicaciones, anio, tipo_documento))
