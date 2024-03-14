#### Script problem set 2 ######

# Realizamos inicialmente una limpieza del entorno
rm(list = ls())

# Set directory:
setwd('/Users/juansilva/Documents/GitHub/Problem-set-2')
# Llamamos las librerías necesarias para la realización del trabajo
require(pacman)
require(tidyverse)
require(rvest)
require(stargazer)
require(rio)
require(caret)
require(gridExtra)
require(skimr)
require(boot)
require(tidytable)
require(VIM)
require(leaps)
require(margins)

# Importar la base de datos:
EH = read.table(unz("Datos/train_hogares.csv.zip", "train_hogares.csv"), header=T, sep=",")
EP = read.table(unz("Datos/train_personas.csv.zip", "train_personas.csv"), header=T, sep=",")
TH = read.table(unz("Datos/test_hogares.csv.zip", "test_hogares.csv"), header=T, sep=",")
TP = read.table(unz("Datos/test_personas.csv.zip", "test_personas.csv"), header=T, sep=",")

# Seleccion de variables testeo = entrenamiento:
colnames(EH)
colnames(TH)
colnames(EP)
colnames(TP)

# Data pre process:
# Exclusion de variables:
rm = c('Clase', 'Fex_c', 'Fex_dpto')
categoricas <- c('Dominio', 'P5090', 'Depto', )
binarias <- c('Pobre', 'Indigente', )
continuas <- c('P5000', 'P5010', 'P5100', 'P5130', 'P5140', 'Nper', 'Npersug', 'Ingtotug', 
               'Ingtotugarr', 'Ingpcug', 'Npobres', 'Nindigentes', ) 

# Variables personas:
rm = c('Clase', 'Oc', 'Fex_c', 'Fex_dpto')
categoricas <- c('Dominio', 'P6040', 'P6100', 'P6240', 'Oficio', 'P6430', 'P6920', 'P7050', 'P7350', 'Depto')
tordinal = c('P6210', 'P6870')
binarias12 = c('P6020','P6090', 'P6510', 'P6510s2', 'P6545', 'P6545s2', 'P6580', 'P6580s2', 'P6585s1','P6585s1a2', 
               'P6585s2', 'P6585s2a2', 'P6585s3', 'P6585s3a2', 'P6585s4', 'P6585s4a2', 'P6590', 'P6600', 'P6610', 
               'P6620', 'P6630s1', 'P6630s2', 'P6630s3', 'P6630s4', 'P6630s6', 'P7040', 'P7090', 'P7110', 
               'P7120', 'P7140s1', 'P7140s2', 'P7150', 'P7160', 'P7310', 'P7422', 'P7472', 'P7495', 'P7500s1', 'P7500s2',
               'P7500s3', 'P7505', 'P7510s1', 'P7510s2', 'P7510s3', 'P7510s5','P7510s6', 'P7510s7',   )
binarias <- c('Pet', 'Des', 'Ina', 'Cclasnr2', 'Cclasnr3', 'Cclasnr4', 'Cclasnr5', 'Cclasnr6', 'Cclasnr7', 'Cclasnr8', 
              'Cclasnr11')

continuas <- c('Estrato1', 'P6040', 'P6210s1', 'P6426', 'P6500', 'P6510s1', 'P6545s1', 'P6580s1', 'P6585s1a1', 'P6585s2a1', 
               'P6585s3a1', 'P6585s4a1', 'P6590s1', 'P6600s1', 'P6610s1', 'P6620s1', 'P6630s1a1',  'P6630s2a1', 'P6630s3a1', 
               'P6630s4a1', 'P6630s6a1', 'P6750', 'P6760', 'P550', 'P6800', 'P7045', 'P7070', 'P7422s1', 'P7472s1', 'P7500s1a1',
               'P7500s2a1', 'P7500s3a1', 'P7510s1a1', 'P7510s2a1', 'P7510s3a1', 'P7510s5a1', 'P7510s6a1', 'P7510s7a1', 'Impa', 
               'Isa', 'Ie', 'Imdi', 'Iof1', 'Iof2', 'Iof3h', 'Iof3i', 'Iof6', 'Impaes', 'Isaes', 'Iees', 'Imdies', 'Iof1es', 
               'Iof2es', 'Iof3hes', 'Iof3ies', 'Iof6es', 'Ingtotob', 'Ingtotes', 'Ingtot', ) 

# Variables dependientes:
Yhogar = colnames(EH)[!(colnames(EH) %in% colnames(TH))]

Ypersona = colnames(EP)[!(colnames(EP) %in% colnames(TP))]

# eliminamos las filas con vars == 9
table(EP$P6090)

table(EP$P6510)

table(EP$P6545)

table(EP$P6580)

table(EP$P6585s1)

table(EP$P6585s2)

table(EP$P6585s3)

table(EP$P6585s4)

table(EP$P6590)

table(EP$P6600)

table(EP$P6610)

table(EP$P6620)

table(EP$P7500s1)

table(EP$P7500s2)

table(EP$P7500s3)

table(EP$P7510s1)

table(EP$P7510s2)

table(EP$P7510s3)

table(EP$P7510s5)

table(EP$P7510s6)

table(EP$P7510s7)
# Eliminar la categoria 9 para estas variables:

# Analisis variables 'P6510' 'P6545' 'P6585s1', 'P6585s2', 'P6585s3', 'P6585s4':

# Preprocesamiento de continuas GABI:
# porcentaje de missings por variable:
DF <- DF %>% mutate_at(c(continuas, ordinales), ~ (scale(.) %>% as.vector()))

# Preprocesamiento de binarias JUAN:
resta1 <- function(x) {
  y <- x - 1
  returnValue(y)
}
DF <- DF %>% mutate_at(binar12, ~ (resta1(.)))

# Preprocesamiento de categoricas MAPA:
DF <- get_dummies(
  DF,
  cols = categoricas,
  prefix = TRUE,
  prefix_sep = "_",
  drop_first = FALSE,
  dummify_na = TRUE
)
DF <- DF %>% select(-categoricas)
ED <- ED[!(ED$Variable %in% categoricas), ]

# Preprocesamiento de tordinales HUGO:

# Rename HUGO:

# Tratamiento de outliers:

# Tratamiento de missings:

# Creacion de variables:

# Estadisticas descriptivas:
