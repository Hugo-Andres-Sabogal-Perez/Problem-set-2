rm(list = ls())
# Set directory:
setwd("/Users/mapaosuna/Desktop/Octavo Semestre/Big Data/Talleres/Taller 2/Problem-set-2")

# Llamamos las librerías necesarias para la realización del trabajo
require(pacman)
require(tidyverse)
require(rio)
require(caret)
require(gridExtra)
require(skimr)
require(tidytable)
require(VIM)
require(leaps)

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
Of_jefetest = TP %>% filter(P6050 == 1)

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

Of_jefetrain<-Of_jefetrain %>% select(all_of(var_jefe))
Of_jefetest<-Of_jefetest %>% select(all_of(var_jefe))

##Seleccionamos variables a nivel de personas (para train y test)
var_personas<-c('id','Clase','P6020', 'P6040', 'P6050', 'P6210', 
                'P7495','P7505')

EP<-EP %>% select(all_of(var_personas))
TP<-TP %>% select(all_of(var_personas))

##Creación de nuevas variables (agrupamiento por hogar)

# Porcentaje de mujeres:
EP$P6020 = ifelse(EP$P6020==2,1,0)
pmujertrain = EP %>% group_by(id) %>% summarise(pmujer = sum(P6020)/length(P6020))
TP$P6020 = ifelse(TP$P6020==2,1,0)
pmujertest = TP %>% group_by(id) %>% summarise(pmujer = sum(P6020)/length(P6020))

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

#Salud
Of_jefetrain$P6090 = ifelse(Of_jefetrain$P6090== 1, 1, 0)
Of_jefetest$P6090 = ifelse(Of_jefetest$P6090== 1, 1, 0)

# ingresos no laborales:
EP$P7505 = ifelse(EP$P7505 == 1, 1, 0)
ingnolab = EP %>% group_by(id) %>% summarise(ingsec = max(P7505))

TP$P7505 = ifelse(TP$P7505 == 1, 1, 0)
ingnolabt = TP %>% group_by(id) %>% summarise(ingsec = max(P7505))


# Join de las bases de datos a nivel de hogares:
# 1. Entrenamiento:
EH = EH %>% left_join(pmujertrain, by = c('id' = 'id'))
EH = EH %>% left_join(edad_train, by = c('id' = 'id'))
EH = EH %>% left_join(edu, by = c('id' = 'id'))
EH = EH %>% left_join(ajc, by = c('id' = 'id'))
EH = EH %>% left_join(ingnolab, by = c('id' = 'id'))
EH = EH %>% left_join(Of_jefetrain, by = c('id' = 'id'))
EH = EH %>% left_join(VarDep_train, by = c('id' = 'id'))

r <- c('Fex_c', 'Fex_dpto', 'Li', 'Lp', 'Clase.y')
EH<- EH %>% select(-all_of(r))

# 2. Testeo:
TH = TH %>% left_join(pmujertest, by = c('id' = 'id'))
TH = TH %>% left_join(edad_test, by = c('id' = 'id'))
TH = TH %>% left_join(edut, by = c('id' = 'id'))
TH = TH %>% left_join(ajct, by = c('id' = 'id'))
TH = TH %>% left_join(ingnolabt, by = c('id' = 'id'))
TH = TH %>% left_join(Of_jefetest, by = c('id' = 'id'))

r <- c('Fex_c', 'Fex_dpto', 'Li', 'Clase.y')
TH<- TH %>% select(-all_of(r))

# Metricas de hogares:
vars <- length(colnames(EH))
descEH <- data.frame("Variable" = colnames(EH), "Missings" = rep(NA, vars), "Media en Y=1" = rep(NA, vars), "Media en Y=0" = rep(NA, vars),  
                     "Desviacion Estandard en Y = 1" = rep(NA, vars), "Desviacion Estandard en Y = 0" = rep(NA, vars))

for (col in colnames(EH)) {
  df <- EH %>% select(col)
  df1 = EH %>% filter(Pobre == 1) %>% select(col)
  df0 = EH %>% filter(Pobre == 0) %>% select(col)
  NAs <- sum(is.na(df))/nrow(EH)
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

# Seleccion de variables con menos a 30% de falktantes:
# Entrenamiento:
miss = descEH$Variable[descEH$Missings < .3]
EH = EH %>% select(all_of(miss))

descEH = descEH[descEH$Missings < .3,]

# Testeo:
TH = TH %>% select(any_of(miss))

# Base de datos para estadisticas descriptivas:
write.csv(x = EH, file = "Stores/EstDesc.csv", row.names = FALSE)
write.csv(x = TH, file = "Stores/EstDesc_Test.csv", row.names = FALSE)

# Importamos desde aquí para evitar demoras.

#Estadisticas descriptivas:

EH = read.csv("/Users/mapaosuna/Desktop/Octavo Semestre/Big Data/Talleres/Taller 2/Problem-set-2/Stores/EstDesc.csv")
TH = read.csv("/Users/mapaosuna/Desktop/Octavo Semestre/Big Data/Talleres/Taller 2/Problem-set-2/Stores/EstDesc_Test.csv")


# Histograma Ingresos
require(ggplot2)
histograma_ingreso <- ggplot(EH, aes(x = Ingpcug)) +
  geom_histogram(color = "white", fill = "darkgreen") +
  xlab("Ingreso después de Imputaciones") +
  ylab("Frecuencia") +
  theme_bw()
histograma_ingreso

ggsave("Views/histograma_ing.pdf", width = 6, height = 4, plot = histograma_ingreso)

# Dada la distribución del ingreso se debe utilizar el logaritmo
EH$ln_ingpcug <- log(EH$Ingpcug)

# Histograma Logaritmo de Ingreso
histograma_ingreso_log <- ggplot(EH, aes(x = ln_ingpcug)) +
  geom_histogram(color = "white", fill = "darkgreen") +
  xlab("Logaritmo del Ingreso después de Imputaciones") +
  ylab("Frecuencia") +
  theme_bw()
histograma_ingreso_log

ggsave("Views/histograma_ing_log.pdf", width = 6, height = 4, plot = histograma_ingreso_log)

# Barras de log ingreso con maxedu
#Calculamos las medias para cada categoría de la variable de educación
EH$ln_ingpcug = ifelse(EH$ln_ingpcug == -Inf, 0,log(EH$Ingpcug)) #Cambiamos las observaciones que que tienen ln(0)
medias =tapply(EH$ln_ingpcug, EH$maxedu, mean)
base_educ = data.frame(maxedu= c(1,2,3,4,5,6), medias=medias)

#Realizamos la gráfica
barras_1 <- ggplot(base_educ, aes(x = maxedu, y = medias))+
  geom_bar(stat= "identity", fill = "darkgreen") +
  theme_bw() +
  geom_smooth(method ="lm", se=F, color = "firebrick") +
  xlab("Máximo Año de Educación Alcanzada en el Hogar") +
  ylab("Logaritmo del Ingreso")
barras_1
ggsave("Views/barras_1.pdf", width = 6, height = 4, plot = barras_1)

#Número de personas en el hogar
dispersion_1 = ggplot(EH, aes(x=Nper, y= ln_ingpcug)) +
  geom_point(color = "darkgreen") +
  geom_smooth(method= "lm", color ="firebrick")+
  xlab("Número de personas en el hogar") +
  ylab("Logaritmo de los ingresos")
dispersion_1
ggsave("Views/dispersion_per.pdf",width = 6, height = 4, plot = dispersion_1)

#Tabla de estadísticas descriptivas
#Renombramos las variables que serán utilizadas para la tabla 
EH = rename(EH, num_cuartos =P5000)
EH = rename(EH, num_dormitorios =P5010)
EH = rename(EH, cot_salud =P6090)
EH = rename(EH, horas_trabajadas =P6800)
EH = rename(EH, tamaño_empresa =P6870)


tabla = data.frame(EH$num_cuartos, EH$num_dormitorios, EH$nninos, EH$nviejos, EH$cot_salud, EH$horas_trabajadas, EH$Oficio, EH$tamaño_empresa, EH$pmujer, EH$Nper)
vars <- length(colnames(tabla))
EstDesc <- data.frame("Variable" = colnames(tabla), "Observaciones" = rep(NA, vars), "Media" = rep(NA, vars), 
                     "Desviacion_Estandar" = rep(NA, vars), "Min" = rep(NA, vars), "Max"= rep(NA, vars))


for (col in colnames(tabla)) {
  df <- tabla %>% select(col)
  Obs <- nrow(df)
  mean <- mean(as.numeric(unlist(df)), na.rm = T)
  sd <- sqrt(var(df, na.rm = T))
  min = min(as.numeric(unlist(df)), na.rm=T)
  max= max(as.numeric(unlist(df)), na.rm=T)
  
  EstDesc[EstDesc$Variable == col, 2] <- Obs
  EstDesc[EstDesc$Variable == col, 3] <- mean
  EstDesc[EstDesc$Variable == col, 4] <- sd
  EstDesc[EstDesc$Variable == col, 5] <- min
  EstDesc[EstDesc$Variable == col, 6] <- max
}
require(xtable)
tabla_estadisticas = xtable(EstDesc, type="latex")
print(tabla_estadisticas , file = "Views/estadisticas.tex")

# Estandarizacion de Lp:
mediay = mean(EH$Ingpcug, na.rm = T)
sdy = sqrt(var(EH$Ingpcug, na.rm = T))
Lpstd = (TH$Lp[1] - mediay)/sdy
TH$Lp =  as.vector(rep(Lpstd, nrow(TH)))

# Estandarizacion de variables continuas:
continuas = c('num_cuartos', 'num_dormitorios', 'Nper', 'Npersug', 'pmujer', 'nninos', 
              'nviejos','P6426', 'horas_trabajadas', 'Ingpcug')

# Entrenamiento:
EH <- EH %>% mutate_at(continuas, ~ (scale(.) %>% as.vector()))

# Testeo:
continuas = c('num_cuartos', 'num_dormitorios', 'Nper', 'Npersug', 
              'pmujer', 'nninos', 'nviejos','P6426', 'horas_trabajadas')

TH <- TH %>% mutate_at(continuas, ~ (scale(.) %>% as.vector()))

# Tratamiento de valores extremos:
# Entrenamiento:
EH = EH %>% mutate_at(continuas, ~ (ifelse((.) >= 2.5, 2.5, (.))))
EH = EH %>% mutate_at(continuas, ~ (ifelse((.) <= -2.5, -2.5, (.))))

# Testeo:
TH = TH %>% mutate_at(continuas, ~ (ifelse((.) >= 2.5, 2.5, (.))))
TH = TH %>% mutate_at(continuas, ~ (ifelse((.) <= -2.5, -2.5, (.))))

# Limpieza del environment:
list = ls()
list = list[!(list %in% c('EH', 'TH'))]
rm(list = list)

### Base #1:
# Eliminamos todas las observaciones que tengan missing values:
E_HS <- EH
write.csv(x = E_HS, file = "Stores/E_HS.csv", row.names = FALSE)


write.csv(x = TH, file = "Stores/test.csv", row.names = FALSE)

### Base #2: 
# Utilizamos NA como una categoria adicional:
# Entrenamiento:
categoricas <- c('Dominio', 'Clase.x', 'P5090', 'Depto', 'maxedu', 'Oficio', 
                 'P6240', 'tamaño_empresa')

EHMP = get_dummies(
  EH,
  cols = categoricas,
  prefix = TRUE,
  prefix_sep = "_",
  drop_first = FALSE,
  dummify_na = TRUE
)

# Testeo:
THMP = get_dummies(
  TH,
  cols = categoricas,
  prefix = TRUE,
  prefix_sep = "_",
  drop_first = FALSE,
  dummify_na = TRUE
)

# Knn para missings:

EHMPimp = EHJASimp[, 1:189]
write.csv(x = EHMPimp, file = "Stores/EstDesc.csv", row.names = FALSE)

### Base #3:
# No dummyficamos los missing:
# Entrenamiento:
categoricas <- c('Dominio', 'Clase.x', 'P5090', 'Depto', 'maxedu', 'Oficio', 
                 'P6240', 'tamaño_empresa')
EHJAS = get_dummies(
  EH,
  cols = categoricas,
  prefix = TRUE,
  prefix_sep = "_",
  drop_first = FALSE,
  dummify_na = FALSE
)

# Testeo:
THJAS = get_dummies(
  TH,
  cols = categoricas,
  prefix = TRUE,
  prefix_sep = "_",
  drop_first = FALSE,
  dummify_na = FALSE
)

# Llenar los missings:
EHJASimp <- kNN(EHJAS)
EHJASimp = EHJASimp[, 1:185]
write.csv(x = EHJASimp, file = "Stores/EstDesc.csv", row.names = FALSE)

# Analisis de covarianza:
Corr <- as.data.frame(cor(EH, use = "pairwise.complete.obs"))

descEH$Corr <- as.vector(rep(NA, nrow(descEH)))

for (var in rownames(Corr)) {
  COR <- Corr %>% select(var)
  names <- colnames(Corr)[abs(COR) > 0.999]
  names <- names[!is.na(names)]
  descEH$Corr[descEH$Variable == var] <- toString.default(names)
}



