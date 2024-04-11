setwd('')

require(pacman)
require(tidyverse)
require(rio)
require(caret)
require(gridExtra)
require(skimr)
require(tidytable)
require(VIM)
require(leaps)
require(usethis)
require(devtools)
require(MLmetrics)


EH = read.csv("", header = T, sep = ",")
TH = read.csv("", header = T, sep = ",")

set.seed(1001)
ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    savePredictions = T)

grid <- expand.grid(nrounds = c(1000, 1500),
                    max_depth = 6,
                    eta = c(.01, .001),
                    gamma = c(.5, .2),
                    colsample_bytree = .5,
                    min_child_weight = c(10, 5),
                    subsample = .8)

XGBreg <- train(Ingpcug~., 
             data = EHPobre, 
             method = "xgbTree",
             trControl = ctrl,
             tuneGrid = grid,
             metric="MSE")

XGBreg 

# Extraer Predicciones
predXGBshort <- THJAS  %>% 
  mutate(Ingpcug = predict(XGBreg, newdata = TH, type = "raw"))  %>% select(id,Ingpcug, Lpstd)


#ajustamos la prediccion
predXGBshort<- predXGBshort %>% mutate(pobre=ifelse(Ingpguc > Lpstd,0,1)) %>% select(id, pobre)

write.csv(predXGBshort, "regression_XGB.csv", row.names = F)