
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


## Preparamos los datos para simular los parametros de entrada de la funcion

DF_municipio <- buildings.gis[Poblacion == 'TARANCON',]
CLM <- readOGR('capas_unificadas/CLM_hasta_fase2/Building.shp')
capa_municipio <- CLM[CLM$municipio == 'TARANCON',]

#Establecemos el sistema de coordenadas de la capa municipio

proj4string(capa_municipio) <- CRS("+init=epsg:3042")

# Creamos la capa de puntos desde el DF
coordenadas <- as.matrix(DF_municipio[,.(ADDRESS.X, ADDRESS.Y)])
capa_puntos <- SpatialPointsDataFrame(coordenadas, DF_municipio, proj4string = CRS("+init=epsg:3857"), coords.nrs = c(4,5), match.ID = T )
# LLevamos a la misma proyeccion que los poligonos
capa_puntos <- spTransform(capa_puntos, CRS("+init=epsg:3042"))
proj4string(capa_puntos) <- CRS("+init=epsg:3042")

plot(capa_puntos)
plot(capa_municipio, add=T)

portales.con.parcela <- over(capa_puntos, capa_municipio)
portales.con.parcela$indice <- rownames(portales.con.parcela)
capa_puntos$indice <- rownames(capa_puntos@data)
portales.con.parcela <- merge(capa_puntos@data, portales.con.parcela, by.x = "indice", by.y = "indice")

# Seleccionamos solo columnas de interes
portales.con.parcela$indice <- NULL

## Separamos los que han solapado puntos con poligonos y los que no

portales.solape.directo <- portales.con.parcela[is.na(portales.con.parcela$refCAT)== F,]
portales.buscar.parcela.cercana <- portales.con.parcela[is.na(portales.con.parcela$refCAT),]
portales.buscar.parcela.cercana <- portales.buscar.parcela.cercana[,c("id.building", "Provincia", "Poblacion", "ADDRESS.X", "ADDRESS.Y", "N")]

## Hay que seleccionar de la capa de portales los que no han cruzado directamente, creando una capa de nuevo
# Creamos la capa de puntos desde el DF
portales.buscar.parcela.cercana <- as.data.table(portales.buscar.parcela.cercana)
coordenadas <- as.matrix(portales.buscar.parcela.cercana[,.(ADDRESS.X, ADDRESS.Y)])
capa_puntos <- SpatialPointsDataFrame(coordenadas, portales.buscar.parcela.cercana, proj4string = CRS("+init=epsg:3857"), coords.nrs = c(4,5), match.ID = T )
# LLevamos a la misma proyeccion que los poligonos
capa_puntos <- spTransform(capa_puntos, CRS("+init=epsg:3042"))
proj4string(capa_puntos) <- CRS("+init=epsg:3042")

dist <- gDistance(capa_puntos, capa_municipio, byid=T)
dist.DF <- data.frame(dist)
colnames(dist.DF) <- capa_puntos$id.building
dist.DF$parcela <- capa_municipio$refCAT
dist.DF <- as.data.table(dist.DF)
dist.DF <- melt.data.table(dist.DF, 'parcela')
setnames(dist.DF, c('variable', 'value'), c('building', 'dist'))
dist.DF[, rank := rank(dist), by = 'building']
dist.DF <- dist.DF[rank == 1,]
setnames(dist.DF, c('parcela', 'building'), c('refCAT', 'id.building'))



## Codigo interno funcion












