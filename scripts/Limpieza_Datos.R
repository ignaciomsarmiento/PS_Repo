

###Limpieza de los datos 

#SelecciOnamos las variables de interés para el modelo que se quiere estimar
p_load(rio, #import/export data
       tidyverse, #tidy-data
       skimr, #sumary data
       caret, #clasification and regreation trading)

library(readr)
GEIH <- read_csv("C:/Users/lordb/OneDrive - Universidad de los andes/2. GRUPOS/2. BIG DATA/PS_Repo_Taller1_G10/stores/GEIH.csv")
View(GEIH)


dt_total <- base %>% 
  select(