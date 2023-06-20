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
