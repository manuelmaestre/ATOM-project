
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
  
  #Establecemos el sistema de coordenadas de la capa municipio reproyectamos si es necesario
  if(proj4string(capa_municipio) != "+init=epsg:3042 +proj=utm +zone=30 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"){
    
    capa_municipio <- spTransform(capa_municipio, CRS("+init=epsg:3042"))
    
  }
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
  if (nrow(portales.solape.directo)>0){
    portales.solape.directo$dist <- 0.0
  }
  
  if (nrow(portales.buscar.parcela.cercana)>0){
    
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
    
    
    if (nrow(portales.solape.directo)>0){
      ## Crear DT con edificios cruce directo y por min distancia
    
      total.portales <- rbind(portales.solape.directo[, c("id.building", "refCAT", "dist")], 
                            dist.DF[, c("id.building", "refCAT", "dist")])
    
    }else{
      
      total.portales <- dist.DF[, c("id.building", "refCAT", "dist")]
    
    }
    
    
    }else{
      
      total.portales <- portales.solape.directo[, c("id.building", "refCAT", "dist")]
      
    }
    
  
  
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
buildings.REFCAT.file <- './buildings_gis_refcat/buildings_refcat.csv'
directorio.hist.buildings.refcat <- './buildings_gis_refcat/historico/'

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

error.open.file <- try(read.table(
  buildings.REFCAT.file,
  header = T,
  sep = ";",
  dec = ',',
  encoding = 'UTF-8'))

if (class(error.open.file) != "try-error"){ ##Ya hay un fichero de buildings-REFCAT
  
  ## Cargamos los idbuildings ya con refcat
  buildings.REFCAT <- read.table(
    buildings.REFCAT.file,
    header = T,
    sep = ";",
    dec = ',',
    encoding = 'UTF-8')
  
  ## Historificamos el fichero existente, como'./buildings_gis_refcat/historico/AAMMDD_HHMM_buildings_refcat.csv'
  
  AAMMDDHH <- Sys.time()
  AAMMDDHH <- substr(AAMMDDHH,1,nchar(AAMMDDHH-2))
  AAMMDDHH <- str_replace_all(AAMMDDHH," ", "_")
  AAMMDDHH <- str_replace_all(AAMMDDHH,"-", "")
  AAMMDDHH <- str_replace_all(AAMMDDHH,":", "")
  AAMMDDHH <- substr(AAMMDDHH, 3, nchar(AAMMDDHH))
  
  
  file.copy(buildings.REFCAT.file, str_c(directorio.hist.buildings.refcat, AAMMDDHH, '_buildings_refcat.csv', sep = '', collapse = T), overwrite = T)

  ## Detectamos los nuevos buildings gis sin REFCAT
  
  buildings.sin.REFCAT <- buildings.gis[!(id.building %in% unique(buildings.REFCAT$id.building)),]
  primera.ejecucion = FALSE
  
}else{  ## No hay fichero anterior de buildings-REFCAT
  
  ## Utilizamos todos los buildings de gis para buscar su REFCAT
  buildings.sin.REFCAT <- buildings.gis
  primera.ejecucion = TRUE
  
}

## Dejar sólo para asociar una refCAT, aquellos buildings sin refcat que estén en los municipios objetivo
## es decir, hay capa para cruzarlos y tienen un municipio de GIS en la tabla de control

## filtramos la tabla de control a los municipios existentes en la capa gráfica


munis.objetivo <- fichero.control[(ine_txt %in% unique(capa.unificada$INE)),]
munis.objetivo <- munis.objetivo[(Nombre_GIS %in% unique(buildings.sin.REFCAT$Poblacion)),]



## Bucle para asociar la refCAT de los nuevos buildings, creando un DT de buildings con refCAT adicionales
## El bucle recorre todos los municipios en la capa general de buildings


if (nrow(munis.objetivo)>0){
  
  for (i in seq(nrow(munis.objetivo))){
    
    fila <- munis.objetivo[i]
    print(nrow(munis.objetivo)-i)
    print(fila$Municipio)
    if (.Platform$OS.type == "windows") flush.console()
    
    ## Llamamos a la funcion que asocia la refcat
    
    DT_municipio <- buildings.sin.REFCAT[Poblacion == fila$Nombre_GIS,]
    polig_municipio <- capa.unificada[capa.unificada$municipio == fila$Municipio,]
    proj4string(polig_municipio) <- CRS("+init=epsg:3042")
    
    resultado <- AsociaRefcat(DT_municipio, polig_municipio)
    
    if (i == 1){
      
      buildings.new.REFCAT <- resultado
      
    }else{
      
      buildings.new.REFCAT <- rbind(buildings.new.REFCAT, resultado)
      
    }

  }
  
}

## Unir los nuevos buildings asociados a REFCAT con los existentes

if (nrow(buildings.new.REFCAT)>0){
  
  if (primera.ejecucion){
    
    DT.salida <- buildings.new.REFCAT
    
    }else{
      
      DT.salida <- unique(rbind(buildings.REFCAT, buildings.new.REFCAT))
    
    }
    
  
}else{
  
  DT.salida <- buildings.REFCAT
  
}


## Guardar el fichero de buildings-refCAT generado
#'./buildings_gis_refcat/buildings_gis_refcat.csv'

write.table(DT.salida,
            file = buildings.REFCAT.file,
            sep=";",
            na = "",
            dec=",",
            row.names = F,
            fileEncoding = 'UTF-8')








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
polig_municipio <- capa.unificada[capa.unificada$municipio == 'HERENCIA',]
proj4string(polig_municipio) <- CRS("+init=epsg:3042")
DF_municipio <- DT_municipio
capa_municipio <- polig_municipio
##La funcion recibe una capa de polígonos (capa_municipio), en proyeccion CRS("+init=epsg:3042")
## y un DT de edificios con coordenadas en proyección CRS("+init=epsg:3857") Google
## Devuelve el DT de los edificios, con la refCAT asociada desde la capa de polígonos

#Establecemos el sistema de coordenadas de la capa municipio reproyectamos si es necesario
if(proj4string(capa_municipio) != "+init=epsg:3042 +proj=utm +zone=30 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"){
  
  capa_municipio <- spTransform(capa_municipio, CRS("+init=epsg:3042"))
  
}
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
portales.solape.directo$dist <- 0.0

if (nrow(portales.buscar.parcela.cercana)>0){
  
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
  
  
  if (nrow(portales.solape.directo)>0){
    ## Crear DT con edificios cruce directo y por min distancia
    
    total.portales <- rbind(portales.solape.directo[, c("id.building", "refCAT", "dist")], 
                            dist.DF[, c("id.building", "refCAT", "dist")])
    
  }else{
    
    total.portales <- dist.DF[, c("id.building", "refCAT", "dist")]
    
  }
  
  
}else{
  
  total.portales <- portales.solape.directo[, c("id.building", "refCAT", "dist")]
  
}



## Agregar la refCAT al DF de entrada y este es el resultado de la funcion
return(merge.data.frame(DF_municipio, total.portales, all.x = T, by.x ='id.building', by.y = 'id.building'))

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
copy.table(matriz[ranking==1 & distancia <6 ,])
write.csv(matriz[ranking==1 & distancia <3 ,],'./cruce_munis_gis.csv',row.names = F)
copy.table(DF.buscados)

