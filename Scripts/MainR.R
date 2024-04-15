####### Script main #######

rm(list = ls()) #LIMPIAR ENTORNO
###Correr 

#Correr script 1
setwd(paste0(getwd(),'/Scripts'))
source('manipulacion.R', encoding = 'UTF-8')

#Correr script 2
setwd(paste0(getwd(),'/Scripts'))
source('modelos JAS.R', encoding = 'UTF-8')

#Correr script 3
setwd(paste0(getwd(),'/Scripts'))
source('Script2HS.R',  encoding = 'UTF-8')

#Correr script 4
setwd(paste0(getwd(),'/Scripts'))
source('modelo HS.R',  encoding = 'UTF-8')

#Correr script 5
setwd(paste0(getwd(),'/Scripts'))
source('Ensamblaje predicciones.R',  encoding = 'UTF-8')
