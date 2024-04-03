#### Script problem set 2 ######

# Realizamos inicialmente una limpieza del entorno
rm(list = ls())

# Set directory:
setwd(substr(getwd(), 1, nchar(getwd()) - 8))
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
EH = read.table(unz("Stores/train_hogares.csv.zip", "train_hogares.csv"), header=T, sep=",")
EP = read.table(unz("Stores/train_personas.csv.zip", "train_personas.csv"), header=T, sep=",")
TH = read.table(unz("Stores/test_hogares.csv.zip", "test_hogares.csv"), header=T, sep=",")
TP = read.table(unz("Stores/test_personas.csv.zip", "test_personas.csv"), header=T, sep=",")

# Seleccion de variables testeo y entrenamiento:
colnames(EH)
colnames(TH)
colnames(EP)
colnames(TP)

# Data pre process:
# Variables dependientes:
Yhogar = colnames(EH)[!(colnames(EH) %in% colnames(TH))]

Ypersona = colnames(EP)[!(colnames(EP) %in% colnames(TP))]

# eliminamos las filas de categoricas con vars == 9
# Eliminar la categoria 9 para estas variables:
# Se realiza una tabla de frecuencia para las variables==9
nueve = c('P7510s7', 'P7510s6', 'P7510s5', 'P7510s3', 'P7510s2', 'P7510s1', 'P7500s3', 'P7500s2', 'P7500s1', 'P6620',
          'P6610', 'P6600', 'P6590', 'P6585s4', 'P6585s3', 'P6585s2', 'P6585s1', 'P6580', 'P6545', 'P6510', 'P6090')

data_nueve<-EP %>% select(all_of(nueve))
frecuencia_nueve <- lapply(data_nueve, table)
frecuencia_nueve

# Se perdio unicamente un .043% de las observaciones
for(var in nueve){
  EP = EP %>% filter(.data[[var]] != 9 | is.na(.data[[var]]))
}

# 1. Variables personas:
rm = c('Clase', 'Oc', 'Fex_c', 'Fex_dpto', 'Dominio', 'Depto')
categoricas <- c('P6100', 'P6240', 'Oficio', 'P6430', 'P6920', 'P7050', 'P7350')
tordinal = c('P6210', 'P6870','P6050')
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

# Preprocesamiento de binarias:
resta1 <- function(x) {
  y <- x - 1
  returnValue(y)
}
EP <- EP %>% mutate_at(binarias12, ~ (resta1(.)))

# Preprocesamiento de categoricas:
EP <- get_dummies(
  EP,
  cols = categoricas,
  prefix = TRUE,
  prefix_sep = "_",
  drop_first = FALSE,
  dummify_na = TRUE
)
EP <- EP %>% select(-categoricas)

# P6210:
EP$P6210 = ifelse(EP$P6210 == 9, 0, EP$P6210)

# Preprocesamiento de continuas:
EPstd <- EP %>% mutate_at(continuas, ~ (scale(.) %>% as.vector()))

# Porcentaje de missings por variable:
# Hay un cantidad de variables que no sirven de la base ep (demasiados missings/constantes)
vars <- length(colnames(EP))
descEP <- data.frame("Variable" = colnames(EP), "Missings" = rep(NA, vars), "Media" = rep(NA, vars), "Desviacion Estandard" = rep(NA, vars))

for (col in colnames(EP)) {
  df <- EP %>% select(col)
  NAs <- sum(is.na(df))/nrow(EP)
  mean <- mean(as.numeric(unlist(df)), na.rm = T)
  sd <- sqrt(var(df, na.rm = T))
  
  descEP[descEP$Variable == col, 2] <- NAs
  descEP[descEP$Variable == col, 3] <- mean
  descEP[descEP$Variable == col, 4] <- sd
}

# Agrupar variables de personas:
descEP = descEP[descEP$Missings < .2,]
descEP = descEP[!(descEP$Variable %in% Ypersona),]

miss = descEP$Variable[descEP$Missings < .2]
EP = EP %>% select(all_of(miss))

# Porcentaje de mujeres:
pmujer = EP %>% group_by(id) %>% summarise(pmujer = sum(P6020)/length(P6020))

# Edades:
edad = EP %>% group_by(id) %>% summarise(nninos = length(P6040[P6040 <= 18]), nviejos = length(P6040[P6040 >= 70]))

# Educacion:
edu = EP %>%  group_by(id) %>% summarise(maxedu = max(P6210))

# Seguridad:
ss = EP %>% filter(P6050 == 1) %>% select(c('id', 'P6090'))

# Arriendos jefe y conyuge:
ajc = EP %>% filter(P6050 == 1 | P6050 == 2) %>% select(c('id', 'P7495')) %>% group_by(id) %>%
  summarise(arrypen = max(P7495))

# ingresos no laborales:
ingnolab = EP %>% group_by(id) %>% summarise(ingsec = max(P7505))

# Jefe de hogar categoricas:
catjefe = EP %>% filter(P6050 == 1) %>% select(-c('Orden','P6020', 'P6040','P6050','P6090', 'P6210', 'P6210s1', 
                                             'P7495', 'P7505', 'Pet'))
# Juntar bases de datos:
EH = EH %>% left_join(pmujer, by = c('id' = 'id'))
EH = EH %>% left_join(edad, by = c('id' = 'id'))
EH = EH %>% left_join(edu, by = c('id' = 'id'))
EH = EH %>% left_join(ss, by = c('id' = 'id'))
EH = EH %>% left_join(ajc, by = c('id' = 'id'))
EH = EH %>% left_join(ingnolab, by = c('id' = 'id'))
EH = EH %>% left_join(catjefe, by = c('id' = 'id'))

# 1.1 Test:
rm = rm[rm %in% colnames(TP)]
categoricas <- categoricas[categoricas %in% colnames(TP)]
binarias12 = binarias12[binarias12 %in% colnames(TP)]
binarias <- binarias[binarias %in% colnames(TP)]
continuas <- continuas[continuas %in% colnames(TP)] 
TP <- TP %>% select(-all_of(rm))

# Preprocesamiento de binarias:
TP <- TP %>% mutate_at(binarias12, ~ (resta1(.)))

# Preprocesamiento de categoricas:
TP <- get_dummies(
  TP,
  cols = categoricas,
  prefix = TRUE,
  prefix_sep = "_",
  drop_first = FALSE,
  dummify_na = TRUE
)
TP <- TP %>% select(-categoricas)

# P6210:
TP$P6210 = ifelse(TP$P6210 == 9, 0, TP$P6210)

# Preprocesamiento de continuas:
TPstd <- TP %>% mutate_at(continuas, ~ (scale(.) %>% as.vector()))

# Porcentaje de mujeres:
pmujer = TP %>% group_by(id) %>% summarise(pmujer = sum(P6020)/length(P6020))

# Edades:
edad = TP %>% group_by(id) %>% summarise(nninos = length(P6040[P6040 <= 18]), nviejos = length(P6040[P6040 >= 70]))

# Educacion:
edu = TP %>%  group_by(id) %>% summarise(maxedu = max(P6210))

# Seguridad:
ss = TP %>% filter(P6050 == 1) %>% select(c('id', 'P6090'))

# Arriendos jefe y conyuge:
ajc = TP %>% filter(P6050 == 1 | P6050 == 2) %>% select(c('id', 'P7495')) %>% group_by(id) %>%
  summarise(arrypen = max(P7495))

# ingresos no laborales:
ingnolab = TP %>% group_by(id) %>% summarise(ingsec = max(P7505))

# Jefe de hogar categoricas:
catjefe = TP %>% filter(P6050 == 1) %>% select(-c('Orden','P6020', 'P6040','P6050','P6090', 'P6210', 'P6210s1', 
                                                  'P7495', 'P7505', 'Pet'))
# Juntar las bases de datos:
TH = TH %>% left_join(pmujer, by = c('id' = 'id'))
TH = TH %>% left_join(edad, by = c('id' = 'id'))
TH = TH %>% left_join(edu, by = c('id' = 'id'))
TH = TH %>% left_join(ss, by = c('id' = 'id'))
TH = TH %>% left_join(ajc, by = c('id' = 'id'))
TH = TH %>% left_join(ingnolab, by = c('id' = 'id'))
TH = TH %>% left_join(catjefe, by = c('id' = 'id'))


## 2. Variables de Hogares:
# Exclusion de variables:
rm = c('Clase', 'Fex_c', 'Fex_dpto')
categoricas <- c('Dominio', 'P5090', 'Depto')
binarias <- c('Pobre', 'Indigente', 'P6090', 'arrypen', 'ingsec')
continuas <- c('P5000', 'P5010', 'P5100', 'P5130', 'P5140', 'Nper', 'Npersug', 'Ingtotug', 
               'Ingtotugarr', 'Ingpcug', 'Npobres', 'Nindigentes', 'pmujer', 'nninos', 'nviejos', 
               'maxedu') 

EH <- EH %>% select(-all_of(rm))

# Preprocesamiento de categoricas:
EH <- get_dummies(
  EH,
  cols = categoricas,
  prefix = TRUE,
  prefix_sep = "_",
  drop_first = FALSE,
  dummify_na = TRUE
)
EH <- EH %>% select(-all_of(categoricas))

# Estandarizacion de Lp:
EH$Lp = (EH$Lp - mean(EH$Ingpcug, na.rm =T))/sqrt(var(EH$Ingpcug, na.rm = T))

# Preprocesamiento de continuas:
EHstd <- EH %>% mutate_at(continuas, ~ (scale(.) %>% as.vector()))

# La base de hogares por el contrario esta super buena en terminos de missings, pero las clases no estan balanceadas.
vars <- length(colnames(EH))
descEH <- data.frame("Variable" = colnames(EH), "Missings" = rep(NA, vars), "Media en Y=1" = rep(NA, vars), "Media en Y=0" = rep(NA, vars),  
                     "Desviacion Estandard en Y = 1" = rep(NA, vars), "Desviacion Estandard en Y = 0" = rep(NA, vars))

for (col in colnames(EH)) {
  df <- EH %>% select(col)
  df1 = EH %>% filter(Pobre == 1) %>% select(col)
  df0 = EH %>% filter(Pobre == 0) %>% select(col)
  NAs <- sum(is.na(df))/nrow(EP)
  mean1 <- mean(as.numeric(unlist(df1)), na.rm = T)
  mean0 <- mean(as.numeric(unlist(df0)), na.rm = T)
  sd1 <- sqrt(var(df1, na.rm = T))
  sd0 <- sqrt(var(df0, na.rm = T))
  
  descEH[descEH$Variable == col, 2] <- NAs
  descEH[descEH$Variable == col, 3] <- mean1
  descEH[descEH$Variable == col, 4] <- mean0
  descEH[descEH$Variable == col, 5] <- sd1
  descEH[descEH$Variable == col, 6] <- sd0
}

# Eliminacion de variables con mas de 21% de missings:
miss = descEH$Variable[descEH$Missings < .21]
EHstd = EHstd %>% select(all_of(miss))

descEH = descEH[descEH$Missings < .21,]
# Analisis de covarianza:
Corr <- as.data.frame(cor(EHstd[,-1], use = "pairwise.complete.obs"))

descEH$Corr <- as.vector(rep(NA, nrow(descEH)))

for (var in rownames(Corr)) {
  COR <- Corr %>% select(var)
  names <- colnames(Corr)[abs(COR) > 0.999]
  names <- names[!is.na(names)]
  descEH$Corr[descEH$Variable == var] <- toString.default(names)
}

# Variables a descartar:
r <- c('P6100_NA', 'P7050_NA', 'P6430_NA', 'P6920_NA', 'Depto_11', 'Li')

EHstd<- EHstd %>% select(-all_of(r))

# Eliminacion de las Y que no necesitamos:
Yhogar = Yhogar[c(1, 2, 5, 6, 7)]
EHstd<- EHstd %>% select(-all_of(Yhogar))

# Tratamiento de outliers:
continuas = c('P5000', 'P5010', 'P5130', 'P5140', 'Nper', 'Npersug', 
              'pmujer', 'nninos', 'nviejos','maxedu')

EHstd = EHstd %>% mutate_at(continuas, ~ (ifelse((.) >= 2.5, 2.5, (.))))
EHstd = EHstd %>% mutate_at(continuas, ~ (ifelse((.) <= -2.5, -2.5, (.))))

# 2.1 Testeo:
rm = rm[rm %in% colnames(TH)]
categoricas <- categoricas[categoricas %in% colnames(TH)]
binarias12 = binarias12[binarias12 %in% colnames(TH)]
binarias <- binarias[binarias %in% colnames(TH)]
continuas <- continuas[continuas %in% colnames(TH)] 
TH <- TH %>% select(-all_of(rm))

# Preprocesamiento de categoricas:
TH <- get_dummies(
  TH,
  cols = categoricas,
  prefix = TRUE,
  prefix_sep = "_",
  drop_first = FALSE,
  dummify_na = TRUE
)
TH <- TH %>% select(-all_of(categoricas))

# Estandarizacion de Lp:
TH$Lp =  as.vector(rep(EH$Lp[1], nrow(TH)))

# Preprocesamiento de continuas:
THstd <- TH %>% mutate_at(continuas, ~ (scale(.) %>% as.vector()))

# Seleccion de variables:
THstd = THstd %>% select(colnames(THstd)[colnames(THstd) %in% colnames(EHstd)])
colnames(EHstd)[!(colnames(EHstd) %in% colnames(THstd))]

EHstd = EHstd %>% select(-c('Oficio_0', 'P7050_9', 'Dominio_BOGOTA'))

# Tratamiento de missings:
list = ls()
list = list[!(list %in% c('EHstd', 'THstd'))]
rm(list = list)

# Guardamos las bases de datos limpias:
write.csv(x = EHstd, file = "Stores/EHstd.csv", row.names = FALSE)
write.csv(x = THstd, file = "Stores/THstd.csv", row.names = FALSE)

# Importamos las bases de datos limpias:
EH = read.table()
EP = read.table()

# Imputación de variables por knn:
# k = 5
EHimp <- kNN(EHstd)
DFimp <- DFimp[,]


# Estadisticas descriptivas:

## Modelaje:

