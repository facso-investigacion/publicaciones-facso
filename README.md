

## Estructura del proyecto

```         

├── README.md
│
├── proc/                     
│   ├── proc-final.R            # MASTERSCRIPT: ejecuta todo y genera los productos
│   ├── 00-funciones.R         
│   ├── 01-sepavid.R
│   ├── 02-orcid.R
│   ├── 03-id-revistas.R
│   ├── 04-indexaciones.R
│   ├── 05-idiomas.R
│   ├── 06-scopus.R
│   ├── 07-coautores.R          # listado completo de autores (FACSO y externos) via OpenAlex
│   └── exportar-bibliometrix.R # utilidad standalone: exporta a formato bibliometrix
│
├── input/
│   ├── original/               
│   └── temp/                   
│
└── output/
|
└── .github/
│   ├── workflows/
|
└── _freeze/
|
└── docs/                     # Documentos renderizados
|
└── includes/                 # Estilo
|
└── ref/                      # Bibliografía
```

------------------------------------------------------------------------

## Replicabilidad y Reproducibilidad

### Input requeridos (`input/original/`)

| Archivo | Contenido | Fuente |
|------------------------|------------------------|------------------------|
| `publicaciones-2020.xlsx` … `-2025.xlsx` | reporte anual SEPAVID (194 columnas) | SEPA-VID |
| `acad.xlsx` | planta académica | Informática FACSO (jelizalde\@uchile.cl) |
| `orcid-ids.csv` | columna `id_orcid` | `colab.xlsx` |
| `colab.xlsx` | puente ORCID ↔ apellidos (`id_orcid`, `ap_paterno`, `ap_materno`) | Colaboratorio (renato.soto\@uchile) |
| `primera_jeraq.rdata` | objeto `primera_jeraq` (`rut_investigador`, `jerarquizacion`) | [CINDAI](https://github.com/facso-investigacion/bases-datos-dip) |
| `scie-wos.csv`, `ssci-wos.csv`, `ahci-wos.csv` | catálogos Web of Science | <https://www.webofscience.com/wos/mjl/collection-list-downloads> |
| `scopus-journals.xlsx` | Scopus Source List | <https://www.elsevier.com/products/scopus/content> |
| `scimagojr.csv` | ranking SCImago | <https://www.scimagojr.com/journalrank.php> |

## Cómo ejecutar

Desde la raíz del proyecto:

``` r
source("proc/proc-final.R")
```

Parámetros al inicio del masterscript:

``` r
FORZAR_API  <- FALSE
ANIO_INICIO <- 2016
ANIO_FIN    <- 2025
```

Con `FORZAR_API = FALSE` la corrida reutiliza lo descargado en `input/temp/` y toma minutos. Con `TRUE` vuelve a consultar todas las APIs

## Credenciales

Consultar a asistenteinvestigacion\@facso.cl

## Productos (`output/`)

| Archivo | Unidad de observación |
|------------------------------------|------------------------------------|
| `base-final.rdata` (`base_final`) | autor × publicación |
| `consolidado-depto.rdata` (`consolidado_depto`) | publicación × departamento |
| `consolidado-wide.rdata` (`consolidado_wide`) | publicación (coautores FACSO en `autor_1 … autor_n`; resumen de coautoría completa: `n_autores_total`, `paises`, `colaboracion_internacional`) |
| `coautores.rdata` (`coautores`) | autor × publicación (FACSO y externos, con afiliación y país — vía OpenAlex, etapa 07) |
| `catalogo-revistas.csv` | revista (`revista_id`, nombre, `issn_canonico`) |
| `session-info.txt` | entorno de la última corrida |
