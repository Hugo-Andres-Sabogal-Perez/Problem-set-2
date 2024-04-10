#### Modelos problem set 2-HS ####

# Set directory:
setwd("C:/Users/hugos/Desktop/Problem-set-2-main/Problem-set-2-main")

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
require(MLmetrics)
require(rpart)

##Impotar bases de datoa limpias
train_H<-import('Stores/E_HS.csv')
test_H<-import('Stores/T_HS_KNNIMP.csv')

#Guardamos en otro datafrmae las variables de ingreso y las borramos
train_Hing<-train_H %>% select(-id,-Pobre)
train_H<-train_H %>% select(-id,-Ingpcug)

###Imputamos para missing para train
train_H<-hotdeck(train_H)
train_Hing<-hotdeck(train_Hing)

### eliminamos algunas columnas
train_H <- subset(train_H, select = -c(22:ncol(train_H)))
train_Hing <- subset(train_Hing, select = -c(22:ncol(train_Hing)))
test_H <- subset(train_H, select = -c(22:ncol(test_H)))

#Convierto en facotores la categoricas para training_classification
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

#Convierto en facotores la categoricas para training_ing
train_Hing<- train_Hing %>% 
  mutate(Dominio=factor(Dominio),
         Clase.x=factor(Clase.x, levels=c(1,2), labels=c('cabecera', 'resto')),
         Depto=factor(Depto),
         maxedu=factor(maxedu),
         Oficio=factor(Oficio),
         P6090=factor(P6090),
         P6240=factor(P6240),
         P6870=factor(P6870),
         P7040=factor(P7040),
         P6920=factor(P6920))

#Convierto en facotores la categoricas para training
test_H<- test_H %>% 
  mutate(Clase.x=factor(Clase.x, levels=c(1,2), labels=c('cabecera', 'resto')),
         Dominio=factor(Dominio),
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
                    summaryFunction = prSummary,
                    savePredictions = T)


set.seed(1001)

#Elastic net clasificacion
modelO1 <- train(Pobre~.,
                data=train_H,
                metric = "F1_Score",
                method = "glmnet",
                trControl = ctrl,
                tuneGrid=expand.grid(
                  alpha = seq(0,1,by=.2),
                  lambda =10^seq(10, -2, length = 10)
                )
                 
)

#resumen del modelo 1 (Elastic net)
modelO1

#ajustamos la prediccion

#Creamos dataframe con prediccion
predict1 <- test_H   %>% 
  mutate(pobre_lab = predict(modelO1, newdata = test_H, type = "raw")    ## predicted class labels
  )  %>% select(id,pobre_lab)

#ajustamos para el submit
predict1<- predict1 %>% 
  mutate(pobre=ifelse(pobre_lab=="si",1,0)) %>% 
  select(id,pobre)


write.csv(predict1,"Stores/classification_elasticnet1.csv", row.names = FALSE)   

##CART clasification

grid <- expand.grid(cp = seq(0, 0.03, 0.001))

arbol <- train(Pobre~.,
                 data = train_H,
                 method = "rpart", 
                 trControl = ctrl, 
                 tuneGrid = grid, 
                 metric= "F1_Score"
)

arbol
