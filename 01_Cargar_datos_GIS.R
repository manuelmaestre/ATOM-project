
#library load

library(rgdal)
library(stringr)
library(readxl)
library(data.table)
library(tidyr)

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


#Static vars

gis.appartments.file <- './datos_gis/Fichero_cobertura.csv'
#temporal.dir <- './temp' #Almacenar capas extraidas temporales
#dir.salida <- './capas_unificadas'

## Cargar tabla de datos de GIS

dir.gis <- data.table(read.csv(file = gis.appartments.file, header = T, comment.char = "", sep = ";", encoding = "latin1", quote = "",dec = ".", colClasses = 'character'))
colnames(dir.gis)
unique(dir.gis$Poblacion)
unique(dir.gis[,c(Provincia, Poblacion)])
View(unique(dir.gis[,.(Provincia, Poblacion)])[order(Provincia, Poblacion)])
View(head(dir.gis, 10))
summary(head(dir.gis,10000))

## Eliminar columnas no significativas, crear idbuilding (gescal24), pasar a double las coordenadas

buildings.gis <- dir.gis[,.(ID_DOMICILIO.TO, Provincia, Poblacion, ADDRESS.X, ADDRESS.Y)]
buildings.gis$id.building <- str_sub(buildings.gis$ID_DOMICILIO.TO,1,24)

buildings.gis$ID_DOMICILIO.TO <- NULL
View(head(buildings.gis, 10))
buildings.gis[, c("ADDRESS.X", "ADDRESS.Y")] <- buildings.gis[,lapply(list(ADDRESS.X,ADDRESS.Y),as.double)]
View(head(buildings.gis, 10))

## Dejar un único registro por id_building, utilizando rank en funcion del numero de registros

buildings.gis <- buildings.gis[, .N, by = c("id.building","Provincia","Poblacion","ADDRESS.X","ADDRESS.Y")]
buildings.gis <- buildings.gis[, ranking := rank(-N, ties = "random"), c("id.building")]
buildings.gis <- buildings.gis[buildings.gis$ranking == 1, ]
buildings.gis$ranking <- NULL

head(buildings.gis,10)
View(buildings.gis[Poblacion=="TARANCON",])



## Crear una función a la que se pasa una capa de un municipio y un dataframe de buildings del municipio
## Devuelve el dataframe con una columna más en la que se indica la parcela más cercana




