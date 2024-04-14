setwd('/Users/juansilva/Documents/GitHub/Problem-set-2')

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
require(h2o)
require(mboost)

EH = read.csv("Stores/E_HS.csv", header = T, sep = ",")
TH = read.csv("Stores/T_HS_KNNIMP.csv", header = T, sep = ",", )
LP = read.table(unz("Stores/test_hogares.csv.zip", "test_hogares.csv"), header=T, sep=",")
LP = LP %>% select(id, Lp)
I = read.table(unz("Stores/train_hogares.csv.zip", "train_hogares.csv"), header=T, sep=",")
I = I %>% select(Ingpcug)

# Se omiten los valores faltantes de EH
EH = na.omit(EH)

# Se seleccionan las variables imputadas:
TH = TH[,1:21]

# Dummyficacion de las bases de datos:
categoricas <- c('Dominio', 'Clase.x', 'P5090', 'Depto', 'maxedu', 'Oficio', 
                 'P6240', 'P6870', "P6920")

EHJAS = get_dummies(
  EH,
  cols = categoricas,
  prefix = TRUE,
  prefix_sep = "_",
  drop_first = FALSE,
  dummify_na = TRUE
)

EHJAS = EHJAS %>% select(-categoricas)

# Testeo:
THJAS = get_dummies(
  TH,
  cols = categoricas,
  prefix = TRUE,
  prefix_sep = "_",
  drop_first = FALSE,
  dummify_na = TRUE
)

THJAS = THJAS %>% select(-categoricas)

# Seleccion de las categorias presentes en ambas bases:
Y = EH %>% select(id,Pobre,Ingpcug) 

#Vamos a dejar las mismas variables tanto en la base de entrenamiento como en la base de prueba
EHJAS = EHJAS %>% select(any_of(colnames(THJAS)))

# Mismas variablesd en el test:
THJAS = THJAS %>% select(colnames(EHJAS))

# Analisis de covarianza:
vars <- length(colnames(EHJAS))
varcovEH <- data.frame("Variable" = colnames(EHJAS), "Cov" = rep(NA, vars))

Corr <- as.data.frame(cor(EHJAS[,-1]))

for (var in rownames(Corr)) {
  COR <- Corr %>% select(var)
  names <- colnames(Corr)[abs(COR) > 0.999]
  names <- names[!is.na(names)]
  varcovEH$Cov[varcovEH$Variable == var] <- toString.default(names)
}

EHJAS = EHJAS %>% select(-c("Clase.x_1", "Clase.x_2"))
THJAS = THJAS %>% select(-c("Clase.x_1", "Clase.x_2"))

#
EHPobre = EHJAS %>% left_join(Y[,1:2], by= c("id" = "id"))
EHIng = EHJAS %>% left_join(Y[,c(1,3)], by= c("id" = "id"))

#
list = ls()
list = list[!(list %in% c("EHPobre", "Y", "THJAS", "EHIng"))]
rm(list = list)

#
EHPobre = EHPobre %>% select(-c("id"))
EHIng = EHIng %>% select(-c("id"))

# 
fact = colnames(EHPobre)[12:170]
fact = append(fact, c("P6090", "P7040"))

EHPobre = EHPobre %>% mutate_at(fact, ~ as.factor(.))

#
fact = fact[!(fact %in% c("Pobre"))]
THJAS = THJAS %>% mutate_at(fact, ~ as.factor(.))
EHIng = EHIng %>% mutate_at(fact, ~ as.factor(.))

### Modelos de clasificacion:
levels(EHPobre$Pobre) = c("NO", "POBRE")

# 1. Random forest optimizado:
ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = TRUE,
                    summaryFunction = prSummary,
                    savePredictions = T)

#Configuramos el grid
grid<-expand.grid(mtry = 42,
                min.node.size= c(1000, 500),
                  splitrule='gini') 

RForest <- train(Pobre~., 
                 data = EHPobre, 
                 method = "ranger",
                 trControl = ctrl,
                 tuneGrid = grid,
                 metric="F",
                 ntree = 40)

RForest

# Extraer Predicciones
predict1 <- THJAS  %>% 
  mutate(pobre = predict(RForest, newdata = THJAS, type = "raw"))  %>% select(id,pobre)

head(predict1)

#ajustamos la prediccion
predict1<- predict1 %>% 
  mutate(pobre=ifelse(pobre=="Pobre",1,0))

head(predict1)

write.csv(predict1, "classification_CART.csv", row.names = F)

# Importancia de las variables:

# 2. CART Optimizado:

# 3. XGBost:
set.seed(1001)
grid <- expand.grid(nrounds = c(100, 500, 1000),
                    max_depth = c(4, 6, 8),
                    eta = c(.3, .1, .01),
                    gamma = c(0, .5),
                    colsample_bytree = .5,
                    min_child_weight = c(10, 30, 50),
                    subsample = .6) 

XGB <- train(Ing~., 
                 data = EHPobre, 
                 method = "xgbTree",
                 trControl = ctrl,
                 tuneGrid = grid,
                 metric="F")

XGB 

# Extraer Predicciones
predXGB <- THJAS  %>% 
  mutate(pobre = predict(XGB, newdata = THJAS, type = "raw"))  %>% select(id,pobre)

head(predXGB)

#ajustamos la prediccion
predXGB<- predXGB %>% 
  mutate(pobre=ifelse(pobre=="POBRE",1,0))

head(predXGB)

write.csv(predXGB, "classification_XGB.csv", row.names = F)

# Importancia de las variables como criterio de selerccion:
Ivar = varImp(XGB, scale = F)
IMP = data.frame('Variables' = as.character(row.names(Ivar[['importance']])), 'Importancia' = Ivar[['importance']][['Overall']])
IMP = IMP %>% filter(Importancia > .00099) %>% select('Variables')

#
imp <- gsub("1$", "",IMP$Variables)
THXGB = THJAS %>% select(any_of(imp), c('Dominio_RESTO_URBANO', 'P7040', 'id'))
EHXGB = EHIng %>% select(any_of(imp), c('Dominio_RESTO_URBANO', 'P7040', 'Ingpcug'))

write.csv(EHXGB, "Entrenamiento_subset.csv", row.names = F)
write.csv(THXGB, "Testeo_subset.csv", row.names = F)


### Modelos de regresion:
# Creacion de LP estandarizado:
mediay = mean(I$Ingpcug, na.rm = T)
sdy = sqrt(var(I$Ingpcug, na.rm = T))
THJAS = THJAS %>% left_join(LP, by = c('id' = 'id'))
THJAS$Lpstd = (THJAS$Lp - mediay)/sdy

# Formula general para la selección de variables:
varspoly <- c('P5000', 'P5010', 'Nper', 'Npersug', 'pmujer', 'nninos', 'nviejos','P6426', 'P6800')
varcat = colnames(EHIng)[13:170]
varcat <- append(fact, c("P6090", "P7040"))
varcat <- gsub(" ", "_", varcat)
# Construct the categorical part of the formula
cat_formula <- paste(varcat, collapse = " + ")

# Construct the polynomial part of the formula
poly_formula <- paste("poly(", varspoly, ", 2, raw=TRUE)", collapse = " + ")

# Combine both parts into the full formula
formula <- paste("Ingpcug ~", cat_formula, "+", poly_formula)

# Convert the formula string to a formula object
maxmodel <- as.formula(formula)

EHIng = EHIng %>% rename("Dominio_RESTO_URBANO" = "Dominio_RESTO URBANO", "Dominio_SANTA_MARTA" = "Dominio_SANTA MARTA")
THJAS = THJAS %>% rename("Dominio_RESTO_URBANO" = "Dominio_RESTO URBANO", "Dominio_SANTA_MARTA" = "Dominio_SANTA MARTA")

# 1. Forward selection:
set.seed(1001)
folds <- sample(rep(1:10, length = nrow(EHIng)))
crossval <- matrix(NA, 10, 50, dimnames = list(NULL, paste(1:50)))
for (j in 1:10) {
  fit <- regsubsets(maxmodel, data = EHIng[folds != j, ], nvmax = 84, method = "forward")
  test <- model.matrix(maxmodel, data = EHIng[folds == j, ])
  for (i in 1:50) {
    coefi <- coef(fit, id = i)
    pred <- test[, names(coefi)] %*% coefi
    crossval[j, i] <- mean((EHIng$Ingpcug[folds == j] - pred)^2)
  }
}

# Se calcula la raiz del error cuadratico medio:
errforward <- apply(crossval, 2, mean)

nvars <- which.min(errforward)[[1]]

# Se estima el mejor modelo con 24 variables elegidas mediante el algoritmo forward.
forward_model <- regsubsets(maxmodel,
                            data = EHIng,
                            nvmax = nvars,
                            method = "forward"
)

forward_model_names <- names(coef(forward_model, id = nvars))

# Se plentea la forma funcional de forward selection:
formfor <- as.formula('Ingpcug ~ Dominio_FLORENCIA + Dominio_MEDELLIN +Dominio_RESTO_URBANO+Dominio_RIOHACHA+
                      Dominio_RURAL+Dominio_VALLEDUPAR+P5090_1+P5090_2+Depto_20+Depto_41+maxedu_3+maxedu_5+
                      Oficio_3+Oficio_4+Oficio_5+Oficio_7+Oficio_8+Oficio_9+Oficio_11+Oficio_12+Oficio_13+Oficio_14+
                      Oficio_15+Oficio_16+Oficio_18+Oficio_30+Oficio_31+Oficio_32+Oficio_33+Oficio_34+Oficio_44+
                      Oficio_50+Oficio_54+P6870_5+P6870_6+poly(P5000, 2, raw = TRUE)+P5010+Nper^2+Npersug+
                      pmujer+poly(nninos, 2, raw = TRUE)+poly(nviejos, 2, raw = TRUE)+poly(P6426, 2, raw = TRUE)+
                      poly(P6800, 2, raw = TRUE)+maxedu_6')

# Se calcula el RMSE fuera de muestra:
modelo_forward_final <- lm(formfor, data = EHIng)

# Extraer Predicciones
predfor <- THJAS  %>% 
  mutate(Ingpguc = predict(modelo_forward_final, newdata = THJAS))  %>% select(id,Ingpguc,Lpstd)

# Incluimos la linea de pobreza:
predfor = predfor %>% mutate(pobre =ifelse(Ingpguc > Lpstd,0,1)) %>% select(id, pobre)

write.csv(predfor, "regression_forward.csv", row.names = F)

# 2. Backward selection:
for (j in 1:10) {
  fit <- regsubsets(maxmodel, data = EHIng[folds != j, ], nvmax = 84, method = "backward")
  test <- model.matrix(maxmodel, data = EHIng[folds == j, ])
  for (i in 1:50) {
    coefi <- coef(fit, id = i)
    pred <- test[, names(coefi)] %*% coefi
    crossval[j, i] <- mean((EHIng$Ingpcug[folds == j] - pred)^2)
  }
}

# Se calcula la raiz del error cuadratico medio:
errbackward <- apply(crossval, 2, mean)

nvars <- which.min(errbackward)[[1]]

# Se estiam el mejor modelo de 35 variables:
backward_model <- regsubsets(maxmodel,
                             data = EHIng,
                             nvmax = nvars,
                             method = "backward"
)

backward_model_names <- names(coef(backward_model, id = nvars))

# Se plentea la forma funcional de forward selection:
formback <- as.formula('Ingpcug ~ Dominio_FLORENCIA +Dominio_ARMENIA+Dominio_CARTAGENA+Dominio_CUCUTA+
                      Dominio_IBAGUE+Dominio_MONTERIA+Dominio_NEIVA+Dominio_PASTO+Dominio_POPAYAN+
                      Dominio_QUIBDO+Dominio_RESTO_URBANO+Dominio_RIOHACHA+Dominio_RURAL+Dominio_SANTA_MARTA+
                      Dominio_SINCELEJO+Dominio_VALLEDUPAR+P5090_1+P5090_2+maxedu_3+maxedu_5+Oficio_1+Oficio_2+
                      Oficio_5+Oficio_8+Oficio_9+Oficio_12+Oficio_13+Oficio_14+Oficio_15+Oficio_16+Oficio_30+
                      Oficio_31+Oficio_32+Oficio_33+Oficio_44+Oficio_50+Oficio_51+Oficio_61+P5000^2+P5010+Nper^2+
                      Npersug+pmujer+nninos+poly(nviejos, 2, raw = TRUE)+P6426+
                      poly(P6800, 2, raw = TRUE)+maxedu_6')

# Se estima el modelo con toda la info:
modelo_backward_final <- lm(formback, data = EHIng)

# Extraer Predicciones
predback <- THJAS  %>% 
  mutate(Ingpguc = predict(modelo_forward_final, newdata = THJAS))  %>% select(id,Ingpguc,Lpstd)

# Incluimos la linea de pobreza:
predback = predback %>% mutate(pobre=ifelse(Ingpguc > Lpstd,0,1)) %>% select(id, pobre)

write.csv(predback, "regression_backward.csv", row.names = F)

# 3. Elastic net optimizado:
THXGB = read.csv("Testeo_subset.csv", header = T, sep = ',')
EHXGB = read.csv("Entrenamiento_subset.csv", header = T, sep = ',')

#
THXGB = THXGB %>% left_join(THJAS[,c('id', 'Lpstd')], by = c('id' = 'id'))
set.seed(1001)
ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = T,
                    summaryFunction = prSummary,
                    savePredictions = T)


modelOnet <- train(Ingpcug~.,
                   data=EHIng,
                   metric = "RMSE",
                   method = "glmnet",
                   trControl = ctrl,
                   tuneGrid= grid)

modelOnet

# Extraer Predicciones
prednetL <- THJAS  %>% 
  mutate(Ingpcug = predict(modelOnet, newdata = THJAS, type = "raw"))  %>% select(id,Ingpcug, Lpstd)


#ajustamos la prediccion
prednetL<- prednetL %>% mutate(pobre=ifelse(Ingpguc > Lpstd,0,1)) %>% select(id, pobre)

write.csv(prednetL, "regression_ENlarge.csv", row.names = F)

#### GBMaschine:
# Limpieza del environment:
list = ls()
list = list[!(list %in% c('THXGB', 'EHXGB'))]
rm(list = list)

set.seed(1001)

grid = expand.grid(mstop = c(7000, 10000, 12000),
                   prune = c('no', 'yes'))

glmboost <- train(Ingpcug~.,
                   data= EHXGB,
                   metric = "RMSE",
                   method = "glmboost",
                   trControl = ctrl,
                   tuneGrid= grid)

glmboost

# Extraer Predicciones
prednetS <- THXGB  %>% 
  mutate(Ingpcug = predict(glmboost, newdata = THXGB, type = "raw"))  %>% select(id,Ingpcug, Lpstd)


#ajustamos la prediccion
prednetS<- prednetS %>% mutate(pobre=ifelse(Ingpcug > Lpstd,0,1)) %>% select(id, pobre)

write.csv(prednetS, "regression_glmboost12k.csv", row.names = F)

