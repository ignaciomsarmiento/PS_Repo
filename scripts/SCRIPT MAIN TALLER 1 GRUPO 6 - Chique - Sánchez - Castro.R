#GRUPO 6 
#SCRAPPING DATOS GEIH DE LA PAGINA DE GITHUB DEL PROFESOR
## Librería necesaria
require(pacman)
library(pacman)

p_load(tidyverse,rvest) 

#Scrapping tabla 1
set_datos_1<-"https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html"
set_datos_1_html<-read_html(set_datos_1)

tabla_datos_1<-set_datos_1_html%>%
  html_table()%>%
  as.data.frame()

#Scrapping tabla 2
set_datos_2<-"https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_2.html"
set_datos_2_html<-read_html(set_datos_2)

tabla_datos_2<-set_datos_2_html%>%
  html_table()%>%
  as.data.frame()


#Scrapping tabla 3
set_datos_3<-"https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_3.html"
set_datos_3_html<-read_html(set_datos_3)

tabla_datos_3<-set_datos_3_html%>%
  html_table()%>%
  as.data.frame()

#Scrapping tabla 4
set_datos_4<-"https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_4.html"
set_datos_4_html<-read_html(set_datos_4)

tabla_datos_4<-set_datos_4_html%>%
  html_table()%>%
  as.data.frame()

#Scrapping tabla 5
set_datos_5<-"https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_5.html"
set_datos_5_html<-read_html(set_datos_5)

tabla_datos_5<-set_datos_5_html%>%
  html_table()%>%
  as.data.frame()

#Scrapping tabla 6
set_datos_6<-"https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_5.html"
set_datos_6_html<-read_html(set_datos_6)

tabla_datos_6<-set_datos_6_html%>%
  html_table()%>%
  as.data.frame()

#Scrapping tabla 7
set_datos_7<-"https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_7.html"
set_datos_7_html<-read_html(set_datos_6)

tabla_datos_7<-set_datos_7_html%>%
  html_table()%>%
  as.data.frame()

#Scrapping tabla 8
set_datos_8<-"https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_8.html"
set_datos_8_html<-read_html(set_datos_8)

tabla_datos_8<-set_datos_8_html%>%
  html_table()%>%
  as.data.frame()

#Scrapping tabla 9
set_datos_9<-"https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_9.html"
set_datos_9_html<-read_html(set_datos_9)

tabla_datos_9<-set_datos_9_html%>%
  html_table()%>%
  as.data.frame()

#Scrapping tabla 10
set_datos_10<-"https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_10.html"
set_datos_10_html<-read_html(set_datos_10)

tabla_datos_10<-set_datos_10_html%>%
  html_table()%>%
  as.data.frame()

#Uniendo tablas de datos

base_todo <- rbind.data.frame(tabla_datos_1, tabla_datos_2, tabla_datos_3, tabla_datos_4, tabla_datos_5, tabla_datos_6, tabla_datos_7, tabla_datos_8, tabla_datos_9, tabla_datos_10)
as.data.frame(base_todo)
base_todo[is.na(base_todo) | base_todo=="-Inf"] = NA

#Cálculo del salario:
base_todo<-base_todo %>%
  mutate(salario=impa+isa+ie)

#Logaritmo del salario
base_todo<-base_todo %>%
  mutate(log_salario=log(salario))

#Variable mujer. Cambio de Sex a female con female=1
base_todo<-base_todo %>%
  mutate(female = if_else(sex == 0, 1, 0) )
