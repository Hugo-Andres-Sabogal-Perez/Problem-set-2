#### Modelos problem set 2-HS ####

# Set directory:
setwd(substr(getwd(), 1, nchar(getwd()) - 8))

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
require(ranger)
require(xgboost)

##Impotar bases de datoa limpias
train_H<-import('Stores/E_HS.csv')
test_H<-import('Stores/T_HS_KNNIMP.csv')
test_lp<-import('Stores/lp_test.csv')


#Guardamos en otro datafrmae las variables de ingreso y las borramos
train_Hing<-train_H %>% select(-id,-Pobre)
train_H<-train_H %>% select(-id,-Ingpcug)

###Imputamos para missing para train
train_H<-hotdeck(train_H)
train_Hing<-hotdeck(train_Hing)

### eliminamos algunas columnas
train_H <- subset(train_H, select = -c(22:ncol(train_H)))
train_Hing <- subset(train_Hing, select = -c(22:ncol(train_Hing)))
test_H <- subset(test_H, select = -c(22:ncol(test_H)))

#Unimos test_H con test_LP

test_H<-left_join(test_H, test_lp, by="id")

#Creamos ingresos en logaritmo
train_Hing$ln_ingpcug<-log(train_Hing$Ingpcug)
test_H$lnlptrain<-log(test_H$lp_tarin)
test_H$lnlptest<-log(test_H$Lp)

#quitamos missings
train_Hing<- train_Hing %>%subset(Ingpcug!=0) %>% select(-Ingpcug)


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


ctrl2<- trainControl(method = "cv",
                    number = 5,
                    savePredictions = T)


set.seed(1001)

#Elastic net clasificacion
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
                 metric= "F"
)

arbol

#Creamos dataframe con prediccion
predict2 <- test_H   %>% 
  mutate(pobre_lab = predict(RForest, newdata = test_H, type = "raw")    ## predicted class labels
  )  %>% select(id,pobre_lab)

#ajustamos para el submit
predict2<- predict2 %>% 
  mutate(pobre=ifelse(pobre_lab=="si",1,0)) %>% 
  select(id,pobre)

write.csv(predict2,"Stores/classification_CART.csv", row.names = FALSE) 

#Random forest classification

#Configuración mtry_grid
mtry_grid<-expand.grid(mtry =c(2,4,8, 12, 16, 20), # 
                       min.node.size= c(10, 50, 100, 200, 400), #controla la complejidad del arbol
                       splitrule= 'gini') #splitrule fija en gini. 
mtry_grid

## cross validacion del arbol
RForest <- train(Pobre~., 
                    data = train_H, 
                    method = "ranger",
                    trControl = ctrl,
                    metric="F",
                    tuneGrid = mtry_grid,
                    ntree=500)

RForest

#Creamos dataframe con prediccion
predict3 <- test_H   %>% 
  mutate(pobre_lab = predict(RForest, newdata = test_H, type = "raw")    ## predicted class labels
  )  %>% select(id,pobre_lab)

#ajustamos para el submit
predict3<- predict3 %>% 
  mutate(pobre=ifelse(pobre_lab=="si",1,0)) %>% 
  select(id,pobre)

write.csv(predict3,"Stores/classification_randomforest.csv", row.names = FALSE) 

#Elastic net regression

ENet<-train(ln_ingpcug ~.,
            data=train_Hing,
            method = 'glmnet', 
            trControl = ctrl2,
            tuneGrid = expand.grid(
              alpha = seq(0,1,by=.01),
              lambda =10^seq(10, -2, length = 10)))

ENet

###creamos prediccion 

#Creamos dataframe con prediccion
predict4 <- test_H   %>% 
  mutate(ln_ing_lab = predict(ENet, newdata = test_H, type = "raw")    ## predicted class labels
  )  %>% select(id,ln_ing_lab, lnlptest)

#ajustamos para el submit
predict4<- predict4 %>% 
  mutate(pobre=ifelse(ln_ing_lab<lnlptest ,1,0)) %>% 
  select(id,pobre)


write.csv(predict4,"Stores/regression_elasticnet.csv", row.names = FALSE) 


##Regression CART


reg_arbol <- train(ln_ingpcug~.,
               data = train_Hing,
               method = "rpart", 
               trControl = ctrl2, 
               tuneGrid = grid, 
               
)

reg_arbol


#Creamos dataframe con prediccion
predict5 <- test_H   %>% 
  mutate(ln_ing_lab = predict(reg_arbol, newdata = test_H, type = "raw")    ## predicted class labels
  )  %>% select(id,ln_ing_lab, lnlptest)

#ajustamos para el submit
predict5<- predict5 %>% 
  mutate(pobre=ifelse(ln_ing_lab<lnlptest ,1,0)) %>% 
  select(id,pobre)

write.csv(predict5,"Stores/regression_CART.csv", row.names = FALSE) 

##Elastic net whit backward selection


formula<- ln_ingpcug~poly(P5000,3,raw=TRUE) +poly(P5010,3,raw=TRUE) + poly(Nper,3,raw=TRUE) + 
          poly(Npersug,3,raw=TRUE) + poly(pmujer,3,raw=TRUE) + poly(nninos,3,raw=TRUE) +
          poly(nviejos,3,raw=TRUE) + poly(P6426,3,raw=TRUE) + poly(P6800,3,raw=TRUE) +
          poly(pmujer,3,raw=TRUE):maxedu  + maxedu + P6090 + P6240 +Oficio +P6870 + P6920 + 
          P7040
          


Enet_backward <- train(formula,
                     data=train_Hing,
                     method="glmnet", 
                     direction="backward", # Backward selection
                     trControl=ctrl2,
                     tuneGrid = expand.grid(
                       alpha = seq(0,1,by=.01),
                       lambda =10^seq(10, -2, length = 10))
                     )

Enet_backward 
#Creamos dataframe con prediccion
predict6 <- test_H   %>% 
  mutate(ln_ing_lab = predict(Enet_backward, newdata = test_H, type = "raw")    ## predicted class labels
  )  %>% select(id,ln_ing_lab, lnlptest)

#ajustamos para el submit
predict6<- predict6 %>% 
  mutate(pobre=ifelse(ln_ing_lab<lnlptest ,1,0)) %>% 
  select(id,pobre)


#randdom forest regression

mtry_grid2<-expand.grid(mtry =c(2,4,8, 12, 16, 20), # 
                        min.node.size= c(10, 50, 100, 200, 400),
                        splitrule="variance") #controla la complejidad del arbol
#splitrule fija en gini. 
mtry_grid2


reg_rforest<-train(ln_ingpcug ~.,
                   data=train_Hing,
                   method = 'ranger', 
                   trControl = ctrl2,
                   tuneGrid = mtry_grid2)

reg_rforest

#Creamos dataframe con prediccion
predict7 <- test_H   %>% 
  mutate(ln_ing_lab = predict(reg_rforest, newdata = test_H, type = "raw")    ## predicted class labels
  )  %>% select(id,ln_ing_lab, lnlptest)

#ajustamos para el submit
predict7<- predict7 %>% 
  mutate(pobre=ifelse(ln_ing_lab<lnlptest,1,0)) %>% 
  select(id,pobre)

predict7 <-predict7 %>%  select(id,pobre)

write.csv(predict7,"Stores/Regression_randomforest.csv", row.names = FALSE) 

### boosting regression

grid_xbgoost <- expand.grid(nrounds = c(250,500),
                            max_depth = c(1, 2),
                            eta = c(0.1,  0.01), 
                            gamma = c(0, 1), 
                            min_child_weight = c(20, 50,70,100),
                            colsample_bytree = c(0.4, 0.6), 
                            subsample = c(0.7))
grid_xbgoost

xb_forest<-train(ln_ingpcug ~.,
                   data=train_Hing,
                   method = 'xgbTree', 
                   trControl = ctrl2,
                   tuneGrid = grid_xbgoost)

xb_forest


#Creamos dataframe con prediccion
predict8 <- test_H   %>% 
  mutate(ln_ing_lab = predict(xb_forest, newdata = test_H, type = "raw")    ## predicted class labels
  )  %>% select(id,ln_ing_lab, lnlptest)

#ajustamos para el submit
predict9<- predict8 %>% 
  mutate(pobre=ifelse(ln_ing_lab<lnlptest,1,0)) %>% 
  select(id,pobre)


write.csv(predict9,"Stores/Regression_boosting.csv", row.names = FALSE) 

### boosting classification


xbclas_forest<-train(Pobre ~.,
                 data=train_H,
                 method = 'xgbTree', 
                 trControl = ctrl,
                 metric='F',
                 tuneGrid = grid_xbgoost)

xbclas_forest

#Creamos dataframe con prediccion
predict10 <- test_H   %>% 
  mutate(pobre_lab = predict(xbclas_forest, newdata = test_H, type = "raw")    ## predicted class labels
  )  %>% select(id,pobre_lab)

#ajustamos para el submit
predict10<- predict10 %>% 
  mutate(pobre=ifelse(pobre_lab=="si",1,0)) %>% 
  select(id,pobre)

write.csv(predict10,"Stores/classification_boosting.csv", row.names = FALSE) 
