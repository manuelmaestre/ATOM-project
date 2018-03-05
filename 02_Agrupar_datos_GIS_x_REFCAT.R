
#library load

library(rgdal)
library(stringr)
library(readxl)
library(data.table)
library(tidyr)
library(rgeos)

## Common settins & utils

# Environment cleanning

rm(list = ls())


## Utilities

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

#############################################################
##                                                         ##
##                    CODIGO DEL MODULO                    ##
##                                                         ##
#############################################################


#Static vars

gis.appartments.file <- './datos_gis/Fichero_cobertura.csv'
ruta.capa.unificada <- './capas_unificadas/Building.shp'
ruta.fichero.control <- './ficheros_excel/11_listado_archivos_buildings.xlsm'
buildings.REFCAT.file <- './buildings_gis_refcat/buildings_refcat.csv'
directorio.hist.buildings.refcat <- './buildings_gis_refcat/historico/'

## Cargar tabla de datos de GIS

dir.gis <- data.table(read.csv(file = gis.appartments.file, header = T, comment.char = "", sep = ";", encoding = "latin1", quote = "",dec = ".", colClasses = 'character'))
colnames(dir.gis)
unique(dir.gis$Poblacion)
unique(dir.gis[,c(Provincia, Poblacion)])

## Eliminar columnas no significativas, crear idbuilding (gescal24), pasar a double las coordenadas

buildings.gis <- dir.gis[,.(ID_DOMICILIO.TO, Provincia, Poblacion, Permiso)]
buildings.gis$id.building <- str_sub(buildings.gis$ID_DOMICILIO.TO,1,24)

buildings.gis$ID_DOMICILIO.TO <- NULL


## Cargamos los idbuildings ya con refcat
buildings.REFCAT <- read.table(
  buildings.REFCAT.file,
  header = T,
  sep = ";",
  dec = ',',
  encoding = 'UTF-8')

buildings.REFCAT <- buildings.REFCAT[,c("id.building", "refCAT")]
buildings.gis <- merge(buildings.gis, buildings.REFCAT, by.x = 'id.building', by.y ='id.building')

## Dejar un único registro por REFCAT, utilizando rank en funcion del numero de registros y Permiso

parcelas <- buildings.gis[, .N, by = c("refCAT","Permiso")]
parcelas[, ranking := rank(-N, ties = "first"), c("refCAT")]
parcelas <- parcelas[parcelas$ranking == 1, ]
parcelas$ranking <- NULL
rm(dir.gis)
rm(buildings.REFCAT)
rm(buildings.gis)

## Cargamos la capa general de poligonos de parcelas de municipios

capa.unificada <- readOGR(ruta.capa.unificada)
