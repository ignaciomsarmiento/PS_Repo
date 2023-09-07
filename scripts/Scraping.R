################################################################################
############TALLER 1 
#SCRAPING
#Limpieza de area de trabajo --------------------------------------------

rm(list = ls())


setwd("C:\Users\lordb\OneDrive - Universidad de los andes\2. GRUPOS\2. BIG DATA\PS_Repo_Taller1_G10")

## llamar la librería pacman: contiene la función p_load()
require(pacman)


p_load(rio,
       tidyverse,
       skimr,
       caret,
       readxl,
       rvest,
       stargazer,
       knitr,
       boot,
       data.table)


##Se genera un vector con el link y sus bases de datos

link <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", 1:10, ".html")


#Se agrupan las bases de datos 

base <- data.frame()
for(i in link){
  print(i)
  GEIH<- read_html(i) %>% 
    html_table() %>%
    as.data.frame()
  base <- rbind(base, GEIH)
}

View(base)

#Exportamos la base 

write.csv(base, "GEIH.csv")
