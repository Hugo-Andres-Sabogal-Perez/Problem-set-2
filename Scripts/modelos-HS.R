#### Modelos problem set 2-HS ####

# Set directory:
setwd('')

# Realizamos inicialmente una limpieza del entorno
rm(list = ls())

# Llamamos las librerías necesarias para la realización del trabajo
require(pacman)
require(tidyverse)
require(rio)
require(caret)
require(skimr)
require(VIM)
require(glmnet)

##Impotar bases de datoa limpias
train_H<-import('Stores/E_HS.csv')
test_H<-import('Stores/test.csv')

train_H<-train_H %>% select(-id)

#Convierto en facotores la categoricas para training
train_H<- train_H %>% 
  mutate(Pobre=factor(Pobre,levels=c(0,1),labels=c("No","si")),
         Dominio=factor(Dominio),
         Clase.x=factor(Clase.x, levels=c(1,2), labels=c('cabecera', 'resto')),
         Depto=factor(Depto),
         maxedu=factor(maxedu),
         Oficio=factor(Oficio),
         P6090=factor(P6090),
         P6240=factor(P6240),
         P6870=factor(P6870),
         P7040=factor(P7040),
         P6920=factor(P6920))

#Configuracion de cross validation
ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    savePredictions = T)

set.seed(098063)

#Elastic net
modelO1 <- train(Pobre~.,
                data=train_H,
                metric = "F1",
                method = "glmnet",
                trControl = ctrl,
                tuneGrid=expand.grid(
                  alpha = seq(0,1,by=.2),
                  lambda =10^seq(10, -2, length = 10)
                )
                 
)
