
## Devuelve las columnas de un dataframe formateadas como c("","") para poder cambiar nombres, ordenes, etc.
## Inserta un espacio al principio que quito manualmente
## no devuelve un str, solo una representacion para copiar y pegar desde la consola

cols.tabla.comillas <- function(tabla.datos){
  cat(paste("c(\"", paste(colnames(tabla.datos), sep=" ", collapse = "\", \"")), "\")", sep="",collapse="")
}

cols.tabla <- function(tabla.datos){
  cat(paste("c(", paste(colnames(tabla.datos), sep="", collapse = ", ")), ")", sep="",collapse="")
}

### codigo aplicar función varias columnas a la vez de data table
fincas.total[, c("cod_finca_dgc", "GESCAL17", "tipo_cod_finca"):= NULL]
fincas.total[, c("X", "Y")] <- fincas.total[,lapply(list(X,Y),sub, pattern=",", replacement=".")]
fincas.total[, c("X", "Y")] <- fincas.total[,lapply(list(X,Y),as.double)]

# Copy data out of R
copy.table <- function(obj, size = 4096) {
  clip <- paste('clipboard-', size, sep = '')
  f <- file(description = clip, open = 'w')
  write.table(obj, f, row.names = FALSE, sep = '\t')
  close(f)  
}

# Paste data into R
paste.table <- function() {
  f <- file(description = 'clipboard', open = 'r')
  df <- read.table(f, sep = '\t', header = TRUE)
  close(f)
  return(df)
}


