library (pacman)
p_load(rvest, tidyverse)


table1 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html") %>%
  html_table()
head(table1)
as.data.frame(table1)

table2 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_2.html") %>%
  html_table()
head(table2)
as.data.frame(table2)

table3 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_3.html") %>%
  html_table()
head(table3)
as.data.frame(table3)

table4 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_4.html") %>%
  html_table()
head(table4)
as.data.frame(table4)

table5 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_5.html") %>%
  html_table()
head(table5)
as.data.frame(table5)

table6 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_6.html") %>%
  html_table()
head(table6)
as.data.frame(table6)

table7 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_7.html") %>%
  html_table()
head(table7)
as.data.frame(table7)

table8 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_8.html") %>%
  html_table()
head(table8)
as.data.frame(table8)

table9 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_9.html") %>%
  html_table()
head(table9)
as.data.frame(table9)

table10 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_10.html") %>%
  html_table()
head(table10)
as.data.frame(table10)

GEIH <- bind_rows(table1, table2, table3, table4, table5, table6, table7, table8, table9, table10)

library(openxlsx)
write.xlsx(GEIH, "Datos GEIH.xlsx", )

#mostrar las primeras entradas, viene del paquete base
str(GEIH)

# viene del paquete dplyr
glimpse(GEIH)

#estadisticas descriptivas sin desviacion estandar
summary(GEIH)

#estadisticas descriptivas mas pros

p_load(rio,
       tidyverse,
       skimr,
       caret)
estadisticas <- skim(GEIH)
estadisticas_tbl <- as.data.frame(estadisticas)
p_load(stargazer)
stargazer(estadisticas_tbl, type = "latex", summary = F)
stargazer(GEIH, type = "latex", summary = T)

#Eliminar menores de 18 años
GEIHmod4 <- subset(GEIH, age>=18)

#Exportar BD a Excel
library(openxlsx)
write.xlsx(GEIHmod4, "Datos GEIHmod4.xlsx", )

#para contar cuantos missing hay
is.na(GEIHmod4$y_salary_m_hu) %>% table()

#Primero lo intentamos con todos los missing values a ver como da
#"Crear age2"#
GEIHmod4 = GEIHmod4 %>% 
  mutate(age2 = age*age)

#Crear log(w)#
GEIHmod4 = GEIHmod4 %>% 
  mutate(log_y_salary_m_hu = log(y_salary_m_hu))


library(openxlsx)
write.xlsx(GEIHmod4, "Datos GEIHmod5.xlsx", )


modelopunto3 <- lm(log_y_salary_m_hu~age + age2, data = GEIHmod4)

summary(modelopunto3)

ggplot(data = GEIHmod4 , mapping = aes(x = B0+ B1_model_p3*age+ B2_model_p3*age2, y = log_y_salary_m_hu)) +
  geom_point(col = "red" , size = 0.5)

B1_model_p3 <-6.031e-02
B2_model_p3 <- -6.550e-04 

#Calcular la edad que maximiza la funcion de salario#

peak_age2 <- -B1_model_p3/(2*B2_model_p3)

B0 <- 7.388e+00


#Ahora hay que intentarlo sin los missing values a ver cómo difieren los resultados

GEIHmod4_no_miss = GEIHmod4 %>% dplyr::filter(is.na(y_salary_m_hu)==F)

library(openxlsx)
write.xlsx(GEIHmod4_no_miss, "Datos GEIHmod_no_miss.xlsx", )

modelopunto3_no_miss <- lm(log_y_salary_m_hu~age + age2, data = GEIHmod4_no_miss)

summary(modelopunto3_no_miss)

B0 <- 7.388e+00
B1_model_p3_no_miss <-6.031e-02
B2_model_p3_no_miss <- -6.550e-04

ggplot(data = GEIHmod4 , mapping = aes(x = B0+ B1_model_p3_no_miss*age+ B2_model_p3_no_miss*age2, y = log_y_salary_m_hu)) +
  geom_point(col = "red" , size = 0.5)

#Calcular la edad que maximiza la funcion de salario#

peak_age2 <- -B1_model_p3/(2*B2_model_p3)

#Bootstrap

p_load("boot")

#Definimos el bootstrap para la edad máxima#
eta_mod3_fn<-function(data,index){
  
  f<-lm(log_y_salary_m_hu~age + age2, GEIHmod4, subset = index)
  
  coefs<-f$coefficients
  
  b1<-coefs[2]
  b2<-coefs[3]
  
  peak_age_b<- -b1/(2*b2)
  
  
  return(peak_age_b)
}

#Vemos que la funcion eta_mod3_fn da igual a peak_age2=46,03

eta_mod3_fn(GEIHmod4,1:nrow(GEIHmod4))

#Cuando hacemos bootstrap con R=1000 da un error estandar de 0.84971, entre mas R entonces más alto el error estandar y menos sesgo
results2 <- boot(data = GEIHmod4, statistic = eta_mod3_fn, R = 1000)
results2

#Definimos el intervalo de confianza del 95%#
z <- 1.96
std_error <- 0.9261287

#Calcular los intervalos de confianza### Nos dan 44.21861<peak_age_3<47.84903

peak_age_3 <- 46.03382 
confid_int1_eta_mod3_fn <- peak_age_3 + (z*std_error)
confid_int2_eta_mod3_fn <- peak_age_3 - (z*std_error)

confid_int1_eta_mod3_fn
confid_int2_eta_mod3_fn

#Punto 4------------------------------------------------FWL###


