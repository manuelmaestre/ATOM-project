## Asociamos los literales de calle de catastro a los de Ivima para cada calle de este último listado.
## así podremos llevar el año de construcción de catastro a Ivima para utilizarlo como predictor en el 
## modelo. También asociaremos las coordenadas xy de catastro para poblar el barrio en Ivima desde el 
## shape de barrios

library(data.table)
library(stringr)
library(tidyr)
library(rgdal)
library(readxl)


clean.data.dir <- '../../../data/clean'
file.fincas.ivima <- paste(clean.data.dir,  "/IVIMA/fincas_ivima.csv", sep = "")
file.fincas.cat <- paste(clean.data.dir,  "/ficheros_preparados/BI_28_900_U_2016-01-23.csv.gz", sep = "")
file.tipos.via.cat <- paste(clean.data.dir,  "/CAT/tipo_via_cat.csv", sep = "")
file.calles.cruzadas <- paste(clean.data.dir,  "/IVIMA/calles_cruzadas.csv", sep = "")
ruta.shp <- './capas/A.ES.SDGC.BU.02001.buildingpart.gml'
capa1 <- 'A.ES.SDGC.BU.02001.buildingpart'
capa2 <- 'A.ES.SDGC.BU.08093.buildingpart'

## obtenido por cruce espacial con el shape de barrios de madrid

## La capa de barrios de Madrid se puede obtener en ed50 de: http://www.madrid.org/nomecalles/DescargaBDTCorte.icm. (Delimitaciones territoriales, barrios)

## Cargamos la capa de barrios. Está en proyección EPGS:23030, ED50/UTM30
ogrListLayers(ruta.shp)
capa1.shp <- readOGR(dsn = ruta.shp, layer = "BuildingPart", encoding = "UTF-8")
capa2.shp <- readOGR(dsn = './capas/A.ES.SDGC.BU.08093.buildingpart.gml', layer = "BuildingPart", encoding = "UTF-8")
proj4string(capa1.shp)
proj4string(capa2.shp)

proj4string(barrios.shp) <- CRS("+init=epsg:23030")


# Creamos la capa de puntos desde las xy del portalero de catastro

### Generamos el shape para poder importar directamente en GIS (solo las fincas con coordenadas),proyeccion EPGS:25830
portalero.cat.con.coor <- portalero.catastro[portalero.catastro$x_coor != 0,]
coordenadas <- as.matrix(portalero.cat.con.coor[,.(x_coor, y_coor)])
capa.puntos <- SpatialPointsDataFrame(coordenadas, portalero.cat.con.coor, proj4string = CRS("+init=epsg:25830"), coords.nrs = c(4, 5), match.ID = T)
capa.puntos <- spTransform(capa.puntos, CRS("+init=epsg:23030"))

## Agregamos los datos del barrio al df de portales
portales.con.barrio <- over(capa.puntos, barrios.shp)
portales.con.barrio$indice <- rownames(portales.con.barrio)
capa.puntos$indice <- rownames(capa.puntos@data)
portales.con.barrio <- merge(capa.puntos@data, portales.con.barrio, by.x = "indice", by.y = "indice")
