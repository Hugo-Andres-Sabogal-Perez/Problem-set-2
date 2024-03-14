#### Script problem set 2 ######

# Realizamos inicialmente una limpieza del entorno
rm(list = ls())

# Set directory:
setwd('/Users/juansilva/Documents/GitHub/Problem-set-2')
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

# Importar la base de datos:
EH = read_csv('train_hogares.csv')
EP = read_csv('train_personas.csv')
TH = read_csv('test_hogares.csv')
TP = read_csv('test_personas.csv')
