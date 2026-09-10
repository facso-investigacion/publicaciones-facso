# =====================================================================
# 02a-orcid-consolidado.R  |  Etapa 2a
#
# Consolida en una sola tabla el ORCID de cada academico (si se conoce),
# combinando tres fuentes en orden de prioridad (la primera que responda
# gana): revision manual > colab.xlsx > perfil de OpenAlex ya resuelto.
# Reemplaza al cruce ad-hoc que hacia 02-orcid.R directamente contra
# colab.xlsx (sensible a tildes y sin lugar para correcciones manuales).
#
# Entradas : input/temp/acad.rds                 (etapa 1)
#            input/original/colab.xlsx           (puente ORCID <-> apellidos)
#            input/original/orcid-manual.csv     (opcional, mantenido a mano)
#            input/temp/acad-openalex.rds        (opcional, etapa 02b; puede
#                                                  no existir aun en la
#                                                  primera corrida)
# Salidas  : input/temp/acad-orcid-consolidado.rds (rut, nombre_completo,
#                                                    departamento, id_orcid,
#                                                    fuente)
#            output/orcid-pendientes.csv           (academicos sin ORCID
#                                                    tras las 3 fuentes, para
#                                                    revision manual)
#
# Por que "colab.xlsx" solo recuperaba 66/206 y no 68/206: el join original
# comparaba apellidos con str_squish() pero sin sacar tildes, asi que
# "Asún" (colab.xlsx) y "Asun" (acad.xlsx) no calzaban. El otro caso
# (Hillary Hiner Carroll) es un problema de la fuente: en acad.xlsx el
# materno quedo cargado como "." y "Carroll" termino dentro de `nombres`,
# asi que el apellido efectivo en acad.rds es solo "Hiner". Los 3 restantes
# (Viviani, Falabella Gonzalo, Mujica) no estan en la planta actual y no son
# recuperables por esta via.
# =====================================================================


## ---------------------------------------------------------------------
## 1. FUENTE: REVISION MANUAL (input/original/orcid-manual.csv)
## ---------------------------------------------------------------------
## Archivo mantenido a mano, no generado por el pipeline (input/original/
## es de solo lectura para los scripts). Formato esperado: columnas
## `rut, id_orcid`. Puede no existir o estar vacio; en ese caso esta fuente
## simplemente no aporta filas.

ruta_manual <- ruta_input("orcid-manual.csv")

fuente_manual <- if (file.exists(ruta_manual)) {
  manual_crudo <- read_csv(ruta_manual, col_types = cols(.default = "c"))
  verificar_columnas(manual_crudo, c("rut", "id_orcid"), "input/original/orcid-manual.csv")
  manual_crudo |>
    mutate(rut = norm_rut(rut), id_orcid = str_squish(id_orcid)) |>
    filter(!is.na(rut), !is.na(id_orcid), id_orcid != "") |>
    distinct(rut, .keep_all = TRUE) |>
    transmute(rut, id_orcid, fuente = "manual")
} else {
  message("  [omitido] ", ruta_manual, " no existe todavia (fuente manual vacia).")
  tibble(rut = character(), id_orcid = character(), fuente = character())
}


## ---------------------------------------------------------------------
## 2. FUENTE: colab.xlsx (join por apellidos, insensible a tildes)
## ---------------------------------------------------------------------

verificar_archivos(ruta_input("colab.xlsx"))

acad <- readRDS(ruta_temp("acad.rds"))

colab <- read_xlsx(ruta_input("colab.xlsx")) |>
  clean_names() |>
  mutate(
    ap_materno = limpiar_apellido(ap_materno),
    # Correccion puntual (ver encabezado): el materno de Hillary Hiner
    # quedo mal cargado en acad.xlsx, asi que su apellido efectivo ahi es
    # solo "Hiner". No amerita una regla general por un unico caso.
    ap_materno = if_else(ap_paterno == "Hiner", NA_character_, ap_materno),
    apellidos      = str_squish(paste(ap_paterno, coalesce(ap_materno, ""))),
    apellidos_norm = quitar_tildes(str_to_upper(apellidos))
  ) |>
  select(id_orcid, apellidos_norm) |>
  distinct()

duplicados_colab <- colab |> count(apellidos_norm) |> filter(n > 1)
if (nrow(duplicados_colab) > 0) {
  message("  ADVERTENCIA: ", nrow(duplicados_colab), " apellidos duplicados en ",
          "colab.xlsx tras normalizar tildes; el cruce por apellidos puede ",
          "generar filas espurias para esos casos.")
}

acad_norm <- acad |>
  mutate(apellidos_norm = quitar_tildes(str_to_upper(apellidos))) |>
  select(rut, apellidos_norm)

fuente_colab <- acad_norm |>
  inner_join(colab, by = "apellidos_norm", relationship = "many-to-many") |>
  distinct(rut, .keep_all = TRUE) |>
  transmute(rut, id_orcid, fuente = "colab")


## ---------------------------------------------------------------------
## 3. FUENTE: ORCID ya visible en el perfil de OpenAlex (etapa 02b)
## ---------------------------------------------------------------------
## Candidatos de author_id ya resueltos con alta confianza
## (propuesto_orcid / propuesto_exacto) que ademas traen su propio ORCID.
## Puede no existir aun (primera corrida, antes de que corra 02b): en ese
## caso esta fuente no aporta filas y el consolidado se completa en la
## proxima corrida del pipeline, una vez que 02b haya generado
## acad-openalex.rds (usar_cache evita repetir las consultas ya hechas).

ruta_openalex <- ruta_temp("acad-openalex.rds")

fuente_openalex <- if (file.exists(ruta_openalex)) {
  readRDS(ruta_openalex) |>
    filter(estado_perfil %in% c("propuesto_orcid", "propuesto_exacto"),
           !is.na(orcid_openalex)) |>
    distinct(rut, .keep_all = TRUE) |>
    transmute(rut, id_orcid = orcid_openalex, fuente = "openalex")
} else {
  message("  [omitido] ", ruta_openalex, " no existe todavia (etapa 02b no ha corrido).")
  tibble(rut = character(), id_orcid = character(), fuente = character())
}


## ---------------------------------------------------------------------
## 4. CONSOLIDACION (prioridad: manual > colab > openalex)
## ---------------------------------------------------------------------
## bind_rows respeta el orden de los argumentos; distinct(rut, .keep_all)
## conserva la PRIMERA fila de cada rut, que es exactamente la prioridad
## que se quiere.

mejor_fuente <- bind_rows(fuente_manual, fuente_colab, fuente_openalex) |>
  distinct(rut, .keep_all = TRUE)

acad_orcid_consolidado <- acad |>
  select(rut, nombre_completo, departamento) |>
  left_join(mejor_fuente, by = "rut")

saveRDS(acad_orcid_consolidado, ruta_temp("acad-orcid-consolidado.rds"))


## ---------------------------------------------------------------------
## 5. PENDIENTES (para revision manual)
## ---------------------------------------------------------------------

orcid_pendientes <- acad_orcid_consolidado |>
  filter(is.na(id_orcid)) |>
  arrange(departamento, nombre_completo) |>
  select(rut, nombre_completo, departamento)

write_excel_csv(orcid_pendientes, ruta_output("orcid-pendientes.csv"), na = "")


## ---------------------------------------------------------------------
## 6. VERIFICACIONES
## ---------------------------------------------------------------------

message("  Academicos con ORCID conocido : ", sum(!is.na(acad_orcid_consolidado$id_orcid)),
        " de ", nrow(acad_orcid_consolidado))
print(count(acad_orcid_consolidado |> filter(!is.na(id_orcid)), fuente))
message("  Pendientes (sin ORCID)        : ", nrow(orcid_pendientes),
        " -> ver output/orcid-pendientes.csv")
