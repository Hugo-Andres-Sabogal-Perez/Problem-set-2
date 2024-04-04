#### Modelos problem set 2 ####

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
require(glmnet)

##Impotar bases de datoa limpias
train_H<-import('Stores/EHstd.csv')
test_H<-import('Stores/THstd.csv')

train_H<-train_H %>% select(-id, -Lp)

train_H <- hotdeck(train_H)
test_H <- hotdeck(test_H)


#Convierto en facotores la categoricas para training
train_H<- train_H %>% 
  mutate(Pobre=factor(Pobre,levels=c(0,1),labels=c("No","Yes")))

train_H[, 13:ncol(train_H)] <- lapply(train_H[, 13:ncol(train_H)], factor)

train_H[, 13:ncol(train_H)] <- lapply(train_H[, 13:ncol(train_H)], factor)

# Luego, asignas las etiquetas "si" y "no" a los niveles de los factores
for (i in 13:ncol(train_H)) {
  levels(train_H[[i]]) <- c("no", "si")
}


#Convierto en facotores la categoricas para test
test_H[, 13:ncol(test_H)] <- lapply(test_H[, 13:ncol(test_H)], factor)



#Configuracion de cross validation
ctrl<- trainControl(method = "cv",
                    number = 10,
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

varcov<-var(train_H)