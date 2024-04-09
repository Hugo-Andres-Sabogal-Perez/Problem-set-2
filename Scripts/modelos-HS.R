#### Modelos problem set 2-HS ####

# Set directory:
setwd("C:/Users/h.sabogal/Documents/Problem-set-2-main")

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

#Guardamos en otro datafrmae las variables de ingreso y las borramos
ingpug<-train_H %>% select(id,Ingpcug)
train_H<-train_H %>% select(-id,-Ingpcug)

#Imputamos missings en test
test_H<-hotdeck(test_H)


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

set.seed(098063)

#Elastic net
modelO1 <- train(Pobre~.,
                data=train_H,
                metric = "F",
                method = "glmnet",
                trControl = ctrl,
                tuneGrid=expand.grid(
                  alpha = seq(0,1,by=.2),
                  lambda =10^seq(10, -2, length = 10)
                )
                 
)

#resumen del modelo 1 (Elastic net)
modelO1

#Ahora predecimos fuera de muestra

test_H<-test_H %>% filter(P6240!=5)

predict1 <- test_H   %>% 
  mutate(pobre_lab = predict(modelO1, newdata = test_H, type = "raw")    ## predicted class labels
  )  %>% select(id,pobre_lab)

head(predict1)

#ajustamos la prediccion
predict1<- predict1 %>% 
  mutate(pobre=ifelse(pobre_lab=="si",1,0)) %>% 
  select(id,pobre)


write.csv(predict1,"Stores/classification_elasticnet1.csv", row.names = FALSE)    


