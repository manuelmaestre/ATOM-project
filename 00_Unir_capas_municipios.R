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


# Definición variables estáticas

capas.dir <- './buildings'
dir.capa.unificada <- './capas_unificadas'
name.capa.unificada <- 'Building'
temporal.dir <- './temp' #Almacenar capas extraidas temporales
dir.salida <- './capas_unificadas'
ruta.capa.unificada <- './capas_unificadas/Building.shp'
ruta.fichero.control <- './ficheros_excel/11_listado_archivos_buildings.xlsm'

EPSG <- make_EPSG()
EPSG[grepl("ETRS89$", EPSG$note),]
#EPSG[grepl("UTM$", EPSG$note),]

## Objetivo: unir todas las capas de municipios de interes en una única capa nacional
## desde los .zip individuales

## Funcion que devuelve una capa desde un fichero de entrada, eliminando columnas irrelevantes y añadiendo información 
## adicional necesaria

capa.prepara <- function(in.argumento){
  
  tmp.argumento <- unlist(strsplit(toString(in.argumento), split = "^", fixed = T))
  
  ruta <- tmp.argumento[1]
  municipio <- tmp.argumento[2]
  INE <- tmp.argumento[3]
  
  ficheros.en.zip <- unzip(ruta, list = T)
  fichero.extraer <- ficheros.en.zip[grep('building.gml',ficheros.en.zip$Name),1]
  unzip(zipfile = ruta, exdir = temporal.dir, overwrite = T, files = c(fichero.extraer))
  capa.temp <- readOGR(dsn = str_c(temporal.dir, '/',fichero.extraer), layer = "Building", encoding = "UTF-8", disambiguateFIDs = TRUE)
  
  # Establecer proyeccion
  
  capa.temp <- spTransform(capa.temp, CRS("+init=epsg:3042"))
  proj4string(capa.temp) <- CRS("+init=epsg:3042")
  
  ## eliminar columnas no relevantes
  
  capa.temp <- capa.temp[, c("reference", "currentUse", "numberOfBuildingUnits", "numberOfDwellings", "value", "value_uom")]
  colnames(capa.temp@data) <- c("refCAT", "Uso", "UUII", "Viviendas", "area", "unidad")
  
  ## Agregar ine y nombre de municipio
  
  capa.temp$INE <- INE
  capa.temp$municipio <- municipio
  
  return(capa.temp)
  
}

## Cargar listado general de municipios


fichero.control <- data.table(read_excel(ruta.fichero.control, sheet = 'municipios_ATOM', col_names = T, skip = 0))
fichero.control <- fichero.control[,c("ine_txt", "Municipio", "Nombre fichero", "Existe fichero", "Integrar en capa")]
total.munis.unificada <- fichero.control[`Existe fichero`==1 & `Integrar en capa` == 1]
total.munis.unificada <- unite(total.munis.unificada,col = "parametro",c("Nombre fichero", "Municipio", "ine_txt"), sep = '^', remove = F)
#total.munis.unificada$parametro <- str_c(capas.dir,total.munis.unificada$parametro,sep = '/', collapse = F)
total.munis.unificada$parametro <- paste(capas.dir, total.munis.unificada$parametro, sep = '/')


error.open.file <- try(readOGR(dsn = dir.capa.unificada, name.capa.unificada))

if (class(error.open.file) != "try-error"){
  
  ## Cargar la capa unificada
  capa.unificada <- readOGR(dsn = dir.capa.unificada, name.capa.unificada)
  
}

if(!exists("capa.unificada")){
 
  ## No hay capa a la que añadir municipios, cargamos el primer municipio como capa inicial
  capa.unificada <- capa.prepara(total.munis.unificada[2, c("parametro")])

  
} 
## Identificar municipios a añadir a la capa unificada existente

munis.incrementales <- total.munis.unificada[!(ine_txt %in% unique(capa.unificada$INE))]

## Generar capa incremental de municipios

if (nrow(munis.incrementales)>0){
  list.fich <- as.list(munis.incrementales$parametro)
  capa.incremental <-  do.call(rbind.SpatialPolygonsDataFrame, c(lapply(list.fich, capa.prepara), makeUniqueIDs = T))
}

## Identificar municipios a excluir de la capa unificada existente en base al listado general de municipios
munis.salida <- unique(total.munis.unificada$ine_txt)
munis.unificada <- as.character(unique(capa.unificada$INE))
munis.excluir.ine <- munis.unificada[!(munis.unificada %in% munis.salida)]
## Eliminar municipios a excluir de la capa unificada

if (length(munis.excluir.ine)>0){

capa.unificada.filtrada <- capa.unificada[!(capa.unificada$INE %in% munis.excluir.ine),]    
  
}else{
  
  capa.unificada.filtrada <- capa.unificada
}


## Unir capa incremental con capa unificada, si hay capa incremental

if (exists("capa.incremental")){
  
  capa.unificada.filtrada <- spTransform(capa.unificada.filtrada, CRS("+init=epsg:3042"))
  proj4string(capa.unificada.filtrada) <- CRS("+init=epsg:3042")
  capa.buildings <- rbind.SpatialPolygonsDataFrame(capa.unificada.filtrada, capa.incremental, makeUniqueIDs = T)

  } else{
  
  capa.buildings <- capa.unificada.filtrada
  
}

unique(capa.buildings@data[,c("INE", "municipio")])

suppressWarnings(writeOGR(capa.buildings, dsn = dir.salida, layer = "Building", driver = "ESRI Shapefile",overwrite_layer = T ))

