# ============================================================
# Asignación de identificador único de revista (revista_id)
# a partir de las columnas issn_p, issn_e e issn (mezclada).
#
# Lógica:
#  1) Se extraen TODOS los ISSN de cada fila desde las tres
#     columnas (issn_p, issn_e, e issn separada por ';').
#  2) Se normalizan (mayúsculas, sin espacios).
#  3) Con Union-Find se agrupan las filas que comparten al
#     menos un ISSN => misma revista (esto une casos donde una
#     revista aparece a veces sólo con el impreso y otras sólo
#     con el electrónico).
#  4) Filas sin ISSN se agrupan por nombre de revista normalizado.
#  5) Se asigna un issn_canonico legible por revista.
# ============================================================

load("output/base_final.rdata")   # objeto: base_final
df <- base_final
n  <- nrow(df)

# --- Normalización de un ISSN ---
norm <- function(x){
  x <- toupper(trimws(x))
  x[x %in% c("", "NA", "NULL")] <- NA
  x
}

# --- Extraer múltiples ISSN de una columna (separados por ';') ---
split_issn <- function(col){
  lapply(strsplit(as.character(col), ";"), function(v){
    v <- norm(v); unique(v[!is.na(v)])
  })
}

# --- Reunir todos los ISSN de cada fila ---
p <- norm(df$issn_p)
e <- norm(df$issn_e)
m <- split_issn(df$issn)
all_issn <- Map(function(a, b, c) unique(c(na.omit(c(a, b)), c)), p, e, m)

# ---- Union-Find: agrupa filas que comparten al menos un ISSN ----
parent <- 1:n
find <- function(i){
  while (parent[i] != i){ parent[i] <<- parent[parent[i]]; i <- parent[i] }
  i
}
union <- function(a, b){
  ra <- find(a); rb <- find(b)
  if (ra != rb) parent[ra] <<- rb
}

issn_first <- new.env(hash = TRUE)   # mapa: ISSN -> primera fila que lo contiene
for (i in 1:n){
  for (code in all_issn[[i]]){
    if (is.null(issn_first[[code]])) issn_first[[code]] <- i
    else union(i, issn_first[[code]])
  }
}
roots <- sapply(1:n, find)

# --- Preparar asignación de identificador ---
has_issn <- lengths(all_issn) > 0
rev_norm <- norm(df$revista)
revista_id <- rep(NA_character_, n)

# Revistas con ISSN: id estable por raíz del union-find
roots_con_issn <- unique(roots[has_issn])
revista_id[has_issn] <- sprintf("REV-%04d",
                                match(roots[has_issn], roots_con_issn))

# Revistas sin ISSN: id por nombre de revista normalizado
sin_issn_con_nombre <- !has_issn & !is.na(rev_norm)
noissn_names <- unique(rev_norm[sin_issn_con_nombre])
revista_id[sin_issn_con_nombre] <- sprintf("REVNAME-%03d",
                                           match(rev_norm[sin_issn_con_nombre], noissn_names))
# (Filas sin ISSN y sin nombre quedan con revista_id = NA)

# --- ISSN canónico legible: todos los ISSN de la revista, ordenados ---
issn_canonico <- rep(NA_character_, n)
for (rid in unique(na.omit(revista_id))){
  filas <- which(revista_id == rid)
  codigos <- sort(unique(unlist(all_issn[filas])))
  if (length(codigos) > 0) issn_canonico[filas] <- paste(codigos, collapse = "; ")
}

# --- Adjuntar columnas nuevas ---
base_final$revista_id    <- revista_id
base_final$issn_canonico <- issn_canonico

# --- Reportes ---
cat("Revistas únicas identificadas por ISSN   :", length(roots_con_issn), "\n")
cat("Revistas únicas identificadas por nombre :", length(noissn_names), "\n")
cat("Filas sin identificador (sin ISSN/nombre):", sum(is.na(revista_id)), "\n")

# --- Guardar resultados ---
save(base_final, file = "output/base_final.rdata")

catalogo <- unique(base_final[!is.na(base_final$revista_id),
                                     c("revista_id", "revista", "issn_canonico")])
write.csv(catalogo, "catalogo-revistas.csv", row.names = FALSE)