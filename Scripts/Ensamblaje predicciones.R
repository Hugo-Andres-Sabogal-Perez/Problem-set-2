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

a <- read.csv("Stores/submits/Regression_boosting.csv", header = T, sep = ",")
b <- read.csv("Stores/submits/classification_boosting.csv", header = T, sep = ",")
c <- read.csv("Stores/submits/Regression_randomforest.csv", header = T, sep = ",")
d <- read.csv("Stores/submits/classification_randomforest.csv", header = T, sep = ",")
e <- read.csv("Stores/submits/regression_elasticnet.csv", header = T, sep = ",")
f <- read.csv("Stores/submits/regression_ENsmall.csv", header = T, sep = ",")
g <- read.csv("Stores/submits/regression_glmboost.csv", header = T, sep = ",")
h <- read.csv("Stores/submits/regression_glmboost12k.csv", header = T, sep = ",")
i <- read.csv("Stores/submits/classification_XGB.csv", header = T, sep = ",")
j <- read.csv("Stores/submits/regression_CART.csv", header = T, sep = ",")
k <- read.csv("Stores/submits/regression_CART.csv", header = T, sep = ",")

Ensamble <- a %>% left_join(b, by = c("id" = "id"), suffix = c("_a", "_b"))
Ensamble <- Ensamble %>% left_join(c, by = c("id" = "id"))
Ensamble <- Ensamble %>% left_join(d, by = c("id" = "id"), suffix = c("_c", "_d"))
Ensamble <- Ensamble %>% left_join(e, by = c("id" = "id"))
Ensamble <- Ensamble %>% left_join(f, by = c("id" = "id"), suffix = c("_e", "_f"))
Ensamble <- Ensamble %>% left_join(g, by = c("id" = "id"))
Ensamble <- Ensamble %>% left_join(h, by = c("id" = "id"), suffix = c("_g", "_h"))
Ensamble <- Ensamble %>% left_join(i, by = c("id" = "id"))
Ensamble <- Ensamble %>% left_join(j, by = c("id" = "id"), suffix = c("_i", "_j"))
Ensamble <- Ensamble %>% left_join(k, by = c("id" = "id"))

PRE <- Ensamble %>%
  mutate(
    preds_all = pobre_a + pobre_b + pobre_c + pobre_d + pobre_e + pobre_f + pobre_g + pobre_h + pobre,
    preds_3 = pobre_a + pobre_b + pobre_c,
    preds_peso = pobre_a + (4 / 5) * pobre_b + (4 / 5) * pobre_c +
      (2 / 5) * pobre_d + (1 / 5) * pobre_e + (1 / 5) * pobre_f + (1 / 5) * pobre_g +
      (1 / 5) * pobre_h + (1 / 10) * pobre_i + (1 / 5) * pobre_j + (1 / 5) * pobre
  ) %>%
  select(id, preds_3, preds_all, preds_peso)

PRE <- PRE %>%
  mutate() %>%
  select(id, preds_3, preds_all)

mayoria11 <- PRE %>%
  mutate(pobre = ifelse(preds_all > 5, 1, 0)) %>%
  select(id, pobre)

granmayoria11 <- PRE %>%
  mutate(pobre = ifelse(preds_all > 8, 1, 0)) %>%
  select(id, pobre)

mayoria3 <- PRE %>%
  mutate(pobre = ifelse(preds_3 > 1, 1, 0)) %>%
  select(id, pobre)

unanimidad3 <- PRE %>%
  mutate(pobre = ifelse(preds_3 == 3, 1, 0)) %>%
  select(id, pobre)

quantile(PRE$preds_peso, probs = seq(0, 1, 0.01))



write.csv(mayoria11, "Stores/submits/classification_ensamble_mayoria_simple_11.csv", row.names = F)
write.csv(granmayoria11, "Stores/submits/classification_ensamble_mayor8_11.csv", row.names = F)
write.csv(mayoria3, "Stores/submits/classification_ensamble_mayoria_simple_3.csv", row.names = F)
write.csv(unanimidad3, "Stores/submits/classification_ensamble_unanimidad_3.csv", row.names = F)


pesos <- PRE %>%
  mutate(pobre = ifelse(preds_peso >= .5, 1, 0)) %>%
  select(id, pobre)


write.csv(pesos, "Stores/submits/classification_ensamble_weighted.csv", row.names = F)
