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
dir_capas <- '../10_Catastro_ATOM/capas_test/'
#temporal.dir <- './temp' #Almacenar capas extraidas temporales
#dir.salida <- './capas_unificadas'

## Cargamos la capa de barrios. Está en proyección EPGS:23030, ED50/UTM30

edificios.shp <- readOGR(dsn = dir_capas, layer = "edificios", encoding = "latin-1")
proj4string(edificios.shp) <- CRS("+init=epsg:4326")

parcelas.shp <- readOGR(dsn = dir_capas, layer = "parcelas", encoding = "latin-1")
proj4string(parcelas.shp) <- CRS("+init=epsg:4326")

dist <- gDistance(edificios.shp, parcelas.shp, byid=T)
dist.DF <- data.frame(dist)
colnames(dist.DF) <- edificios.shp$building
dist.DF$parcela <- parcelas.shp$parcela
dist.DF <- as.data.table(dist.DF)
dist.DF <- melt.data.table(dist.DF, 'parcela')
setnames(dist.DF, c('variable', 'value'), c('building', 'dist'))
dist.DF[, rank := rank(dist), by = 'building']
dist.DF <- dist.DF[rank == 1,]
