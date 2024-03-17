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

# Seleccion de variables testeo y entrenamiento:
colnames(EH)
colnames(TH)
colnames(EP)
colnames(TP)

# Data pre process:
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
# Se perdio unicamente un .043% de las observaciones
nueve = c('P7510s7', 'P7510s6', 'P7510s5', 'P7510s3', 'P7510s2', 'P7510s1', 'P7500s3', 'P7500s2', 'P7500s1', 'P6620',
          'P6610', 'P6600', 'P6590', 'P6585s4', 'P6585s3', 'P6585s2', 'P6585s1', 'P6580', 'P6545', 'P6510', 'P6090')

for(var in nueve){
  EP = EP %>% filter(.data[[var]] != 9 | is.na(.data[[var]]))
}

# Porcentaje de missings por variable:
# Hay un cantidad de variables que no sirven de la base ep (demasiados missings/constantes)
vars <- length(colnames(EP))
descEP <- data.frame("Variable" = colnames(EP), "Missings" = rep(NA, vars), "Media" = rep(NA, vars), "Desviacion Estandard" = rep(NA, vars))

for (col in colnames(EP)) {
  df <- EP[, colnames(EP) == col]
  NAs <- sum(is.na(df))/nrow(EP)
  mean <- mean(df, na.rm = T)
  sd <- sqrt(var(df, na.rm = T))
  
  descEP[descEP$Variable == col, 2] <- NAs
  descEP[descEP$Variable == col, 3] <- mean
  descEP[descEP$Variable == col, 4] <- sd
}

# La base de hogares por el contrario esta super buena en terminos de missings, pero las clases no estan balanceadas.
vars <- length(colnames(EH))
descEH <- data.frame("Variable" = colnames(EH), "Missings" = rep(NA, vars), "Media en Y=1" = rep(NA, vars), "Media en Y=0" = rep(NA, vars),  
                     "Desviacion Estandard en Y = 1" = rep(NA, vars), "Desviacion Estandard en Y = 0" = rep(NA, vars))

for (col in colnames(EH)) {
  df <- EH[, colnames(EH) == col]
  df1 = EH[EH$Pobre == 1, colnames(EH) == col]
  df0 = EH[EH$Pobre == 0, colnames(EH) == col]
  NAs <- sum(is.na(df))/nrow(EP)
  mean1 <- mean(df1, na.rm = T)
  mean0 <- mean(df0, na.rm = T)
  sd1 <- sqrt(var(df1, na.rm = T))
  sd0 <- sqrt(var(df0, na.rm = T))
  
  descEH[descEH$Variable == col, 2] <- NAs
  descEH[descEH$Variable == col, 3] <- mean1
  descEH[descEH$Variable == col, 4] <- mean0
  descEH[descEH$Variable == col, 5] <- sd1
  descEH[descEH$Variable == col, 6] <- sd0
}

# Exclusion de variables:
rm = c('Clase', 'Fex_c', 'Fex_dpto')
categoricas <- c('Dominio', 'P5090', 'Depto')
binarias <- c('Pobre', 'Indigente')
continuas <- c('P5000', 'P5010', 'P5100', 'P5130', 'P5140', 'Nper', 'Npersug', 'Ingtotug', 
               'Ingtotugarr', 'Ingpcug', 'Npobres', 'Nindigentes') 

EH <- EH %>% select(-all_of(rm))

# Preprocesamiento de categoricas MAPA:
EH <- get_dummies(
  E,
  cols = categoricas,
  prefix = TRUE,
  prefix_sep = "_",
  drop_first = FALSE,
  dummify_na = TRUE
)
EH <- EH %>% select(-all_of(categoricas))


# Preprocesamiento de continuas GABI:
EHstd <- EH %>% mutate_at(continuas, ~ (scale(.) %>% as.vector()))

# Variables personas:
rm = c('Clase', 'Oc', 'Fex_c', 'Fex_dpto')
categoricas <- c('Dominio', 'P6040', 'P6100', 'P6240', 'Oficio', 'P6430', 'P6920', 'P7050', 'P7350', 'Depto')
tordinal = c('P6210', 'P6870')
binarias12 = c('P6020','P6090', 'P6510', 'P6510s2', 'P6545', 'P6545s2', 'P6580', 'P6580s2', 'P6585s1','P6585s1a2', 
               'P6585s2', 'P6585s2a2', 'P6585s3', 'P6585s3a2', 'P6585s4', 'P6585s4a2', 'P6590', 'P6600', 'P6610', 
               'P6620', 'P6630s1', 'P6630s2', 'P6630s3', 'P6630s4', 'P6630s6', 'P7040', 'P7090', 'P7110', 
               'P7120', 'P7140s1', 'P7140s2', 'P7150', 'P7160', 'P7310', 'P7422', 'P7472', 'P7495', 'P7500s1', 'P7500s2',
               'P7500s3', 'P7505', 'P7510s1', 'P7510s2', 'P7510s3', 'P7510s5','P7510s6', 'P7510s7')
binarias <- c('Pet', 'Des', 'Ina', 'Cclasnr2', 'Cclasnr3', 'Cclasnr4', 'Cclasnr5', 'Cclasnr6', 'Cclasnr7', 'Cclasnr8', 
              'Cclasnr11')

continuas <- c('Estrato1', 'P6040', 'P6210s1', 'P6426', 'P6500', 'P6510s1', 'P6545s1', 'P6580s1', 'P6585s1a1', 'P6585s2a1', 
               'P6585s3a1', 'P6585s4a1', 'P6590s1', 'P6600s1', 'P6610s1', 'P6620s1', 'P6630s1a1',  'P6630s2a1', 'P6630s3a1', 
               'P6630s4a1', 'P6630s6a1', 'P6750', 'P6760', 'P550', 'P6800', 'P7045', 'P7070', 'P7422s1', 'P7472s1', 'P7500s1a1',
               'P7500s2a1', 'P7500s3a1', 'P7510s1a1', 'P7510s2a1', 'P7510s3a1', 'P7510s5a1', 'P7510s6a1', 'P7510s7a1', 'Impa', 
               'Isa', 'Ie', 'Imdi', 'Iof1', 'Iof2', 'Iof3h', 'Iof3i', 'Iof6', 'Impaes', 'Isaes', 'Iees', 'Imdies', 'Iof1es', 
               'Iof2es', 'Iof3hes', 'Iof3ies', 'Iof6es', 'Ingtotob', 'Ingtotes', 'Ingtot') 
EP <- EP %>% select(-all_of(rm))

# Preprocesamiento de binarias JUAN:
EP <- EP %>% mutate_at(binar12, ~ (resta1(.)))

# Preprocesamiento de categoricas MAPA:
EP <- get_dummies(
  EP,
  cols = categoricas,
  prefix = TRUE,
  prefix_sep = "_",
  drop_first = FALSE,
  dummify_na = TRUE
)
EP <- EP %>% select(-categoricas)

# Preprocesamiento de continuas GABI:
EPstd <- EP %>% mutate_at(continuas, ~ (scale(.) %>% as.vector()))

# Preprocesamiento de tordinales HUGO:

# Rename HUGO:

# Tratamiento de outliers:

# Tratamiento de missings:

# Creacion de variables:

# Estadisticas descriptivas:

