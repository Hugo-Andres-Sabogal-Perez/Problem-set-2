setwd(substr(getwd(), 1, nchar(getwd()) - 8))

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
TH = TH %>% rename("Dominio_RESTO_URBANO" = "Dominio_RESTO.URBANO", "Dominio_SANTA_MARTA" = "Dominio_SANTA.MARTA")

set.seed(1001)
ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = F,
                    summaryFunction = defaultSummary,
                    savePredictions = T)

grid <- expand.grid(nrounds = c(5000, 10000),
                    max_depth = 6,
                    eta = .01,
                    gamma = .5,
                    colsample_bytree = c(.2, .4, .6, .8),
                    min_child_weight = 10,
                    subsample = .8)

XGBreg <- train(Ingpcug~., 
             data = EHXGB, 
             method = "xgbTree",
             trControl = ctrl,
             tuneGrid = grid,
             metric="MSE")

XGBreg 

# Extraer Predicciones
predXGBshort <- THXGB  %>% 
  mutate(Ingpcug = predict(XGBreg, newdata = THXGB, type = "raw"))  %>% select(id,Ingpcug, Lpstd)


#ajustamos la prediccion
predXGBshort<- predXGBshort %>% mutate(pobre=ifelse(Ingpcug > Lpstd,0,1)) %>% select(id, pobre)

write.csv(predXGBshort, "regression_XGB_subset.csv", row.names = F)
