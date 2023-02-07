#GRUPO 6 
#SCRAPPING DATOS GEIH DE LA PAGINA DE GITHUB DEL PROFESOR
## Librería necesaria
require(pacman)
library(pacman)

p_load(tidyverse,rvest,skimr) 

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
base_nueva<-select(base_todo, sex, relab, maxEducLevel, p6870, impa, isa, ie)

base_nueva<-na.omit(base_nueva)

##base_todo[is.na(base_todo) | base_todo=="-Inf"] = NA

#Cálculo del salario:
base_nueva<-base_nueva %>%
  mutate(salario=impa+isa+ie)

#Logaritmo del salario
base_nueva<-base_nueva %>%
  mutate(log_salario=log(salario))

#Variable mujer. Cambio de Sex a female con female=1
base_nueva<-base_nueva %>%
  mutate(female = if_else(sex == 0, 1, 0) )


# PREGUNTA 3 -------------- Age-wage profile --------------------

library(boot)

# Reestructurando la base de datos para la pregunta 3

df3<-base_todo %>%
  select(y_salary_m_hu,age)

# Limpiando la base de datos

df3 <- df3 %>%
  drop_na()

# Base de datos de personas mayores de 18 annos

df3 <- df3 %>%
  filter(age >= 18)

# Transformando variables

df3 <- df3 %>%
  mutate(age2 = age^2, lnw =log(y_salary_m_hu))

head(df3)

# Analisis descriptivo y grafico

summary(df3$y_salary_m_hu)

ggplot(data = df3, mapping = aes(x = age, y= lnw))+
  geom_point(col = "#A80000" , size = 0.9)+
  labs(x = "edad",
       y = "logaritmo del salario por hora",
       title = "Edad y Salario")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = df3)+
  geom_histogram(mapping = aes(x=lnw),col= "#A80000", fill = "#A80000")+
  labs(x="logaritmo del ingreso",
       y="frecuencia",
       title = "Distribucion del salario")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = df3)+
  geom_boxplot(mapping = aes(x=y_salary_m_hu), fill = "#A80000", alpha =0.5)+
  labs(x="salario",
       title = "Box Plot")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

ggplot(data = df3)+
  geom_boxplot(mapping = aes(x=lnw), fill = "#A80000", alpha =0.5)+
  labs(x="logaritmo del salario",
       title = "Box Plot")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

quantile(x=df3$y_salary_m_hu)

IQR(x=df3$y_salary_m_hu)

# i) Regresion 

reglnw <- lm(lnw ~ age + age2, data = df3)
summary(reglnw)

stargazer(reglnw, type = "text")

df3$lnw_hat = p#A80000ict(reglnw)

### plot of the estimated age-earning profile

summ = df3 %>%
  group_by(
    age, age2
  ) %>%
  summarize(
    mean_lnw = mean(lnw),
    lnw_hat_reg = mean(lnw_hat), .groups = "drop"
  )

ggplot(summ) +
  geom_point(
    aes(x = age, y = mean_lnw),
    color = "#A80000", size = 2
  ) +
  geom_line(
    aes(x = age, y = lnw_hat_reg),
    color = "#003399", size = 1
  ) +
  labs(
    title = "Logaritmo del salario por edad",
    x = "edad",
    y = "logaritmo del salario"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

### Bootstrap de "peak age"

reglnw <- lm(lnw ~ age + age2, data = df3)
stargazer(reglnw,type = "text")

# i) Obtenemos los coeficientes de la regresion

coefs <- reglnw$coefficients
coefs

# ii) Extrayendo los coeficientes como escalar

b0 <- coefs[1]
b1 <- coefs[2]
b2 <- coefs[3]

# Calculo del "peak age"

peak_age <- (-b1/(2*b2))
peak_age

# Calculo de errores estandar

eta_reglnw_fn <- function(data, index){
  coefs <- lm(lnw ~ age + age2, data, subset = index)$coefficients
  b1 <- coefs[2]
  b2 <- coefs[3]
  peak_age <- (-b1/(2*b2))
  return(peak_age)
}

eta_reglnw_fn(df3,1:nrow(df3))

# Implementamos boot

resultados <- boot(df3, eta_reglnw_fn, R=1000)
resultados

# Intervalo de confianza

boot.ci(resultados, type = c("norm", "basic"))

names(resultados)
hist(resultados$t)
qqnorm(resultados$t, datax = T)

#PROBLEMA 4

##a. Estimación incondicional logaritmo salario vs género
base_nueva<-base_nueva %>%
  filter(salario!=0.0)

reg_1<-lm(log_salario ~ female, base_nueva)
pred_reg_1<-predict(reg_1)

## newdata=base_nueva
length(pred_reg_1)
nrow(base_nueva)

stargazer(reg_1, type="text")

#b.i. EQUAL PAY FOR EQUAL JOB - TEOREMA FWL
#Variables que se van a utilizar en el modelo: log_salario, mujer, relación laboral, 
#máximo nivel educativo alcanzado, tamaño de la empresa donde labora:

#Cambio de nombre para variable p6870: total personas de empresa donde labora:
base_nueva<-base_nueva %>% 
  rename(tam_empresa = p6870)

#Modelo lineal ln Salario con variables dependientes female y categoría de trabajo
reg_2<-lm(log_salario ~ female+relab+maxEducLevel+tam_empresa, base_nueva)
stargazer(reg_2, type="text")

pred_reg_2<-predict(reg_2)
length(pred_reg_2)

#UTILIZANDO FWL:
#PASO 1: Cálculo de los residuales de la regresión en los X2

reg_3<-lm(log_salario~relab+maxEducLevel+tam_empresa, data=base_nueva)
stargazer(reg_3, type="text")

pred_reg_3<-predict(reg_3)

resid_reg_3<-resid(reg_3)

length(resid_reg_3)
nrow(base_nueva)

#PASO 2: Cálculo de los residuales de la regresión de X1 y X2:
reg_4<-lm(female~relab+maxEducLevel+tam_empresa, base_nueva)
stargazer(reg_4, type="text")
resid_reg_4=resid(reg_4)

length(resid_reg_4)

#PASO 3: Regresión de ambos residuales
reg_5<-lm(resid_reg_3 ~ resid_reg_4 , base_nueva)
stargazer(reg_5, type="text")


#COMPARACION CON LA REGRESION INICIAL
stargazer(reg_5, reg_2, type="text")

##BOOTSTRAP
p_load("boot")

sal_fn<-function(data,index) {
  coef(lm(log_salario ~ female+relab+maxEducLevel+tam_empresa, data=base_todo, subset=index))
}

boot(base_todo, sal_fn, R = 1000)

##

