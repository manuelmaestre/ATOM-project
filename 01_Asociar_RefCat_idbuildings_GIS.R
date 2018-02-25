
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
##DEF FUNCION ASIGNA REFCAT A CADA EDIFICIO DE UN MUNICIPIO##
##                                                         ##
#############################################################


AsociaRefcat <- function(DF_municipio, capa_municipio){
  
  ##La funcion recibe una capa de polígonos (capa_municipio), en proyeccion CRS("+init=epsg:3042")
  ## y un DT de edificios con coordenadas en proyección CRS("+init=epsg:3857") Google
  ## Devuelve el DT de los edificios, con la refCAT asociada desde la capa de polígonos
  
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
  
  ## Crear DT con edificios cruce directo y por min distancia
  portales.solape.directo$dist <- 0.0
  total.portales <- rbind(portales.solape.directo[, c("id.building", "refCAT", "dist")], 
                          dist.DF[, c("id.building", "refCAT", "dist")])
  
  
  ## Agregar la refCAT al DF de entrada y este es el resultado de la funcion
  return(merge.data.frame(DF_municipio, total.portales, all.x = T, by.x ='id.building', by.y = 'id.building'))
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

## Cargar tabla de datos de GIS

dir.gis <- data.table(read.csv(file = gis.appartments.file, header = T, comment.char = "", sep = ";", encoding = "latin1", quote = "",dec = ".", colClasses = 'character'))
colnames(dir.gis)
unique(dir.gis$Poblacion)
unique(dir.gis[,c(Provincia, Poblacion)])

## Eliminar columnas no significativas, crear idbuilding (gescal24), pasar a double las coordenadas

buildings.gis <- dir.gis[,.(ID_DOMICILIO.TO, Provincia, Poblacion, ADDRESS.X, ADDRESS.Y)]
buildings.gis$id.building <- str_sub(buildings.gis$ID_DOMICILIO.TO,1,24)

buildings.gis$ID_DOMICILIO.TO <- NULL
buildings.gis[, c("ADDRESS.X", "ADDRESS.Y")] <- buildings.gis[,lapply(list(ADDRESS.X,ADDRESS.Y),as.double)]

## Dejar un único registro por id_building, utilizando rank en funcion del numero de registros

buildings.gis <- buildings.gis[, .N, by = c("id.building","Provincia","Poblacion","ADDRESS.X","ADDRESS.Y")]
buildings.gis <- buildings.gis[, ranking := rank(-N, ties = "random"), c("id.building")]
buildings.gis <- buildings.gis[buildings.gis$ranking == 1, ]
buildings.gis$ranking <- NULL
rm(dir.gis)

## Cargamos la capa general de poligonos de parcelas de municipios

capa.unificada <- readOGR(ruta.capa.unificada)

## Cargar fichero de control de municipios para buscar los nombres de municipios o INE en el shape general
## Y el nombre equivalente de GIS

fichero.control <- data.table(read_excel(ruta.fichero.control, sheet = 'municipios_ATOM', col_names = T, skip = 0))

## Cargar el fichero si existe de idbuildings ya asociados a una REFCAT. Haciendo copia histórica de este fichero tb.



## Dejar sólo para asociar una refCAT, aquellos buildings que no la tengan y estén en los municipios objetivo
## es decir, hay capa para cruzarlos




## Bucle para asociar la refCAT de los nuevos buildings, creando un DT de buildings con refCAT adicionales
## El bucle recorre todos los municipios en la capa general de buildings





## Guardar el fichero de buildings-refCAT generado




#############################################################
##                                                         ##
##                   TEST DE LA FUNCION                    ##
##                poligono a minima distancia              ##
##                                                         ##
#############################################################

## Preparamos los datos para simular los parametros de entrada de la funcion
CLM <- readOGR('capas_unificadas/CLM_hasta_fase2/Building.shp')


DT_municipio <- buildings.gis[Poblacion == 'HERENCIA',]
polig_municipio <- CLM[CLM$municipio == 'HERENCIA',]
proj4string(polig_municipio) <- CRS("+init=epsg:3042")

resultado <- AsociaRefcat(DT_municipio, polig_municipio)

#############################################################
##                                                         ##
##                   TEST DE LA FUNCION                    ##
##                cadena mas parecida                      ##
##                                                         ##
#############################################################


DF.asociar <- (fichero.control[`Integrar en capa`==1, c('Municipio')])
DF.buscados <- unique(buildings.gis[,c('Poblacion')])
View(data.frame(cartesian(DF.asociar$Municipio,DF.buscados$Poblacion)))
matriz <- data.table(expand.grid(DF.asociar$Municipio, DF.buscados$Poblacion, KEEP.OUT.ATTRS = F))
matriz[,distancia:=adist(Var1, Var2), by = 'Var1']
matriz[, ranking:=rank(distancia, ties.method = 'first'), by = 'Var1']
View(matriz[ranking==1 & distancia <3 ,])



