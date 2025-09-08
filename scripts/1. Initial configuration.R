#####################################################
#####PROBLEM SET 1 
#BIG DATA Y MACHINE LEARNING PARA ECONOMIA APLICADA
#GRUPO 10
#NATALIA BUITRAGO BUITRAGO
#NICOLÁS MOCETÓN HERRERA
#####################################################-

if(!require(pacman)) install.packages("pacman") ; require(pacman)
p_load(rio, # import/export data
       tidyverse, # tidy-data
       skimr, # summary data
       visdat, # visualizing missing data
       corrplot, # Correlation Plots 
       stargazer) # tables/output to TEX. 
install.packages("caret")
install.packages("modelsummary")

library(rvest)
library(dplyr)
library(tidyverse)
library(readr)
library(lubridate)
library(openxlsx)
library(stringr)
library(readxl)
library(ggplot2)
library(boot)
library(sandwich)
library(lmtest)
library(caret)
library(skimr)
library(flextable)
library(modelsummary)

options(scipen = 999)
 