rm(list = ls())
# Set directory:
setwd("")

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

#Importamos bases de datos
EH = read.table(unz("Stores/train_hogares.csv.zip", "train_hogares.csv"), header=T, sep=",")
EP = read.table(unz("Stores/train_personas.csv.zip", "train_personas.csv"), header=T, sep=",")
TH = read.table(unz("Stores/test_hogares.csv.zip", "test_hogares.csv"), header=T, sep=",")
TP = read.table(unz("Stores/test_personas.csv.zip", "test_personas.csv"), header=T, sep=",")

#Identificación de las variables dependientes que se utilizan para el desarrollo del trabajo
VarDep_train = EH %>% select(id,Pobre,Ingpcug) 

#Vamos a dejar las mismas variables tanto en la base de entrenamiento como en la base de prueba
EH = EH %>% select(colnames(TH))
#Ahora para las bases de datos de personas
EP = EP %>% select(colnames(TP))

#Inicialmente manipulamos las bases de datos de personas (tanto de entranmiento como de prueba)
#Eliminamos todas las observaciones que tengan la categoría =9

#Base de entrenamiento:
nueve = c('P7510s7', 'P7510s6', 'P7510s5', 'P7510s3', 'P7510s2', 'P7510s1', 'P7500s3', 'P7500s2', 'P6620',
          'P6610', 'P6600', 'P6590', 'P6585s4', 'P6585s3', 'P6585s2', 'P6585s1', 'P6580', 'P6545', 'P6510', 'P6090')

data_nueve<-EP %>% select(all_of(nueve))
frecuencia_nueve <- lapply(data_nueve, table)
frecuencia_nueve #La tabla de frecuencia solo se realiza para identificar que efectivamente se hayan tomado las observaciones correctas.

# Se perdió unicamente un 0.043% de las observaciones
for(var in nueve){
  EP = EP %>% filter(.data[[var]] != 9 | is.na(.data[[var]]))
}

#Base de prueba:
for(var in nueve){
  TP = TP %>% filter(.data[[var]] != 9 | is.na(.data[[var]]))
}

#Filtrar la variable Oficio solo para el jefe de hogar, para analizar el % de missings más adelante
Of_jefetrain = EP %>% filter(P6050 == 1)

#Ahora para test
Of_jefetest = ET %>% filter(P6050 == 1)

#Tabla de estadísticas descriptivas para las variables, únicamente por jefe del hogar
jefe <- length(colnames(Of_jefetrain))
descEP_jefe <- data.frame("Variable" = colnames(Of_jefetrain), "Missings" = rep(NA, jefe), "Media" = rep(NA, jefe), "Desviacion Estandard" = rep(NA, jefe))

for (col in colnames(Of_jefetrain)) {
  df <- Of_jefetrain %>% select(col)
  NAs <- sum(is.na(df))/nrow(Of_jefetrain)
  mean <- mean(as.numeric(unlist(df)), na.rm = T)
  sd <- sqrt(var(df, na.rm = T))
  
  descEP_jefe[descEP_jefe$Variable == col, 2] <- NAs
  descEP_jefe[descEP_jefe$Variable == col, 3] <- mean
  descEP_jefe[descEP_jefe$Variable == col, 4] <- sd
}
#De la nueva base de datos exclusiva para los jefes de hogar, se eliminarán las var con más del 30% de missing values.
descEP_jefe = descEP_jefe[descEP_jefe$Missings < .3,]
miss_jef = descEP_jefe$Variable[descEP_jefe$Missings < .3]
Of_jefetrain = Of_jefetrain  %>% select(all_of(miss_jef))

#Para test
Of_jefetest = Of_jefetest  %>% select(all_of(miss_jef))


#Tabla de estadísticas descriptivas importantes para elegir buenas variables y comparar con la tabla de jefes de hogar
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


##Seleccionamos variables a nivel de jefe de hogar (para train y test)
var_jefe<-c('id','Clase','P6090', 'P6240', 'Oficio','P6426', 'P6800', 'P6870',
            'P6920', 'P7040')

Of_jefetrain<-Of_jefetrain %>% select(var_jefe)
Of_jefetest<-Of_jefetest %>% select(var_jefe)

##Seleccionamos variables a nivel de personas (para train y test)
var_personas<-c('id','Clase','P6020', 'P6040', 'P6050', 'P6210', 
                'P7495','P7505')

EP<-EP %>% select(var_personas)
ET<-ET %>% select(var_personas)

##Creación de nuevas variables (agrupamiento por hogar)

# Porcentaje de mujeres:
pmujertrain = EP %>% group_by(id) %>% summarise(pmujertrain = sum(P6020)/length(P6020))
pmujertest = TP %>% group_by(id) %>% summarise(pmujertest = sum(P6020)/length(P6020))

# Edades:
edad_train = EP %>% group_by(id) %>% summarise(nninos = length(P6040[P6040 <= 18]), nviejos = length(P6040[P6040 >= 70]))
edad_test = TP %>% group_by(id) %>% summarise(nninos = length(P6040[P6040 <= 18]), nviejos = length(P6040[P6040 >= 70]))

# Educacion:
# P6210:
EP$P6210 = ifelse(EP$P6210 == 9, 1, EP$P6210) #Asumimos que las personas que no saben/no responden es porque tienen 0 años de educación.
edu = EP %>%  group_by(id) %>% summarise(maxedu = max(P6210))

TP$P6210 = ifelse(TP$P6210 == 9, 1, TP$P6210) #Asumimos que las personas que no saben/no responden es porque tienen 0 años de educación.
edut = TP %>%  group_by(id) %>% summarise(maxedu = max(P6210))

# Numero de personas que reciben arriendos en el hogar:
EP$P7495 = ifelse(EP$P7495 == 1, 1, 0)
ajc = EP %>% group_by(id) %>% summarise(pensiones = sum(P7495))

TP$P7495 = ifelse(TP$P7495 == 1, 1, 0)
ajct = TP %>% group_by(id) %>% summarise(pensiones = sum(P7495))


# ingresos no laborales:
EP$P7505 = ifelse(EP$P7505 == 1, 1, 0)
ingnolab = EP %>% group_by(id) %>% summarise(ingsec = max(P7505))

TP$P7505 = ifelse(TP$P7505 == 1, 1, 0)
ingnolabt = TP %>% group_by(id) %>% summarise(ingsec = max(P7505))

## Creando base de test
train<-



#Categorización de variables por su tipo 
#Se eliminan variables que generan ruido
rm = c('Clase', 'Oc', 'Fex_c', 'Fex_dpto', 'Dominio', 'Depto')
EP <- EP %>% select(-all_of(rm))
TP <- TP %>% select(-all_of(rm))

#Categóricas
categoricas <- c('P6100', 'P6240', 'Oficio', 'P6430', 'P6920', 'P7050', 'P7350')
EP <- get_dummies(
  EP,
  cols = categoricas,
  prefix = TRUE,
  prefix_sep = "_",
  drop_first = FALSE,
  dummify_na = TRUE
)
EP <- EP %>% select(-categoricas)

#Binarias
binarias12 = c('P6020','P6090', 'P6510', 'P6545', 'P6580', 'P6585s1', 
               'P6585s2', 'P6585s3', 'P6585s4', 'P6590', 'P6600', 'P6610', 
               'P6620', 'P6630s1', 'P6630s2', 'P6630s3', 'P6630s4', 'P6630s6', 'P7040', 'P7090', 'P7110', 
               'P7120', 'P7150', 'P7160', 'P7310', 'P7422', 'P7472', 'P7495', 'P7500s2',
               'P7500s3', 'P7505', 'P7510s1', 'P7510s2', 'P7510s3', 'P7510s5','P7510s6', 'P7510s7')
binarias <- c('Pet', 'Des', 'Ina', 'Cclasnr2', 'Cclasnr3', 'Cclasnr4', 'Cclasnr5', 'Cclasnr6', 'Cclasnr7', 'Cclasnr8', 
              'Cclasnr11')
resta1 <- function(x) {
  y <- x - 1
  returnValue(y)
}
EP <- EP %>% mutate_at(binarias12, ~ (resta1(.)))

#Continuas
continuas <- c('P6040', 'P6210s1', 'P6426','P6800', 'P7045', 'P7422s1', 'P7472s1', 'P7500s1a1',
               'P7500s2a1', 'P7500s3a1', 'P7510s1a1', 'P7510s2a1', 'P7510s3a1', 'P7510s5a1', 'P7510s6a1', 'P7510s7a1', 'Impa', 
               'Isa', 'Ie', 'Imdi', 'Iof1', 'Iof2', 'Iof3h', 'Iof3i', 'Iof6', 'Impaes', 'Isaes', 'Iees', 'Imdies', 'Iof1es', 
               'Iof2es', 'Iof3hes', 'Iof3ies', 'Iof6es', 'Ingtotob', 'Ingtotes', 'Ingtot') 

EPstd <- EP %>% mutate_at(continuas, ~ (scale(.) %>% as.vector()))



