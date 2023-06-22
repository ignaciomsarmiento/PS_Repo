############################## Problem Set 1 ###################################
# Autores: David Stiven Peralta M...
# Si encuentra alg?n error o tiene sugerencias por favor cont?cteme
# correo: ds.peralta@uniandes.edu.co
# fecha: 01/02/2021

rm(list = ls()) # Limpiar Rstudio
options(scipen = 20,  digits=1)
require(pacman)
p_load(ggplot2, rio, tidyverse, skimr, caret, rvest, magrittr) # Cargar varios paquetes al tiempo

table1 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html") %>%
  html_table()
head(table1)
as.data.frame(table1)

# creo un loop para descargar las 10 bases de datos
url_base <- "https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_"
for (i in 1:10) {
  url <- paste0(url_base, i, ".html") 
  read_html(url)
}
head(url)
