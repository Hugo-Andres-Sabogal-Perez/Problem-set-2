setwd("C:/Users/j.silval/Downloads")

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


EH = read.csv("E_HS.csv", header = T, sep = ",")
TH = read.csv("T_HS_KNNIMP.csv", header = T, sep = ",")

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
  prefix_sep = "",
  drop_first = FALSE,
  dummify_na = TRUE
)

EHJAS = EHJAS %>% select(-categoricas)

# Testeo:
THJAS = get_dummies(
  TH,
  cols = categoricas,
  prefix = TRUE,
  prefix_sep = "",
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

EHJAS = EHJAS %>% select(-c("Clase.x1", "Clase.x2"))
THJAS = THJAS %>% select(-c("Clase.x1", "Clase.x2"))

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
fact <- gsub(" ", "",fact)
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
grid <- expand.grid(nrounds = c(1, 5, 10),
                    max_depth = c(1, 5, 10), 
                    subsample = c(.8, 1)) 

XGB <- train(Pobre~., 
                 data = EHPobre, 
                 method = "xgbTree",
                 trControl = ctrl,
                 tuneGrid = grid,
                 metric="F")

XGB

### Modelos de regresion:
# Formula general para la selección de variables:
varspoly <- c('P5000', 'P5010', 'Nper', 'Npersug', 'pmujer', 'nninos', 'nviejos','P6426', 'P6800')
varcat = colnames(EHIng)[13:170]
varcat <- append(fact, c("P6090", "P7040"))
varcat <- gsub(" ", "", varcat)
# Construct the categorical part of the formula
cat_formula <- paste(varcat, collapse = " + ")

# Construct the polynomial part of the formula
poly_formula <- paste("poly(", varspoly, ", 2, raw=TRUE)", collapse = " + ")

# Combine both parts into the full formula
formula <- paste("Ingpcug ~", cat_formula, "+", poly_formula)

# Convert the formula string to a formula object
maxmodel <- as.formula(formula)

# Rename:
EHIng = EHIng %>% rename("DominioRESTOURBANO" = "DominioRESTO URBANO", "DominioSANTAMARTA" = "DominioSANTA MARTA")

# Modelo Forward Selection:
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
formfor <- as.formula()

# Se calcula el RMSE fuera de muestra:
modelo5 <- lm(formfor, data = training)
predictions <- predict(modelo5, testing)
score5a <- RMSE(predictions, testing$lnw)


# Modelo Backward Selection:
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

# Se extrae el RMSE del modelo de 35 variables:
score6 <- errbackward[nvars]

# Se plantea la forma funcional del modelo:
formback <- as.formula()

# Se calcula el RMSE fuera de muestra:
modelo6 <- lm(formback, data = training)
predictions <- predict(modelo6, testing)
score6a <- RMSE(predictions, testing$lnw)

# 1. Forward selection:

# 2. Backward selection:

# 3. Elastic net optimizado:
ctrl<- trainControl(method = "cv",
                    number = 5,
                    classProbs = T,
                    summaryFunction = defaultSummary,
                    savePredictions = T)
grid = expand.grid(
  alpha = seq(0,1,by=.01),
  lambda =10^seq(-2, 2, by = .2))
  
modelOnet <- train(Ingpcug~.,
                 data=EHIng,
                 metric = "MSE",
                 method = "glmnet",
                 trControl = ctrl,
                 tuneGrid= grid)

modelOnet


predictnet <- THJAS   %>% 
  mutate(ing_pred = predict(modelOnet, newdata = test, type = "raw")    ## predicted class labels
  )  %>% select(id,pobre_lab)

