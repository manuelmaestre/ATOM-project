

library(data.table)
library(stringr)

#Static vars

gis.appartments.file <- './datos_gis/Fichero_cobertura.csv'
#temporal.dir <- './temp' #Almacenar capas extraidas temporales
#dir.salida <- './capas_unificadas'

dir.gis <- data.table(read.csv(file = gis.appartments.file, header = T, comment.char = "", sep = ";", encoding = "latin1", quote = ""))
colnames(dir.gis)
unique(dir.gis$Poblacion)
unique(dir.gis[,c(Provincia, Poblacion)])
View(unique(dir.gis[,.(Provincia, Poblacion)])[order(Provincia, Poblacion)])
