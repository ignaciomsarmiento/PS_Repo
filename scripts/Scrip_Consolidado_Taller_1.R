############################################################
# Scrip Taller 1  ->        GRUPO 6                        #
# authors: Victor Chique    BIG DATA AND MACHINE LEARNING  #
#          Natalia Castro                                  #    
#          Victor Sanchez                                  #
############################################################

##################
##  EJERCICIO 2.##
##################

# ---------------SCRAPPING DATOS GEIH----------------------#

## Librería necesaria
require(pacman)
library(pacman)
library(stargazer)
library(rio)
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

# ---------------UNIR BASES DE DATOS---------------#

base_todo <- rbind.data.frame(tabla_datos_1, tabla_datos_2, tabla_datos_3, tabla_datos_4, tabla_datos_5, tabla_datos_6, tabla_datos_7, tabla_datos_8, tabla_datos_9, tabla_datos_10)
as.data.frame(base_todo)
base_nueva<-select(base_todo, sex, age, relab, maxEducLevel, p6870, y_salary_m_hu, p6500, p6510s1, hoursWorkUsual)

base_nueva<-na.omit(base_nueva)




# ---------------CONSTRUCCIÓN VARIABLE SALARIO-------------------#

#Cálculo del salario:
base_nueva<-base_nueva %>%
  mutate(salario_mensual=p6500+p6510s1)%>%
  mutate(salario_mensual_hora=salario_mensual/(hoursWorkUsual*4))


#Logaritmo del salario
base_nueva<-base_nueva %>%
  mutate(log_salario_mensual_hora=log(salario_mensual_hora))

#Variable mujer. Cambio de Sex a female con female=1
base_nueva<-base_nueva %>%
  mutate(female = if_else(sex == 0, 1, 0) )

#En edad sólo mayores de 18 años
base_nueva<-base_nueva%>%
  filter(age>=18)

#Cambio de nombre para variable p6870: total personas de empresa donde labora:
base_nueva<-base_nueva %>% 
  rename(tam_empresa = p6870)

#Cambio de nombre para variable edad
base_nueva<-base_nueva %>% 
  rename(edad = age)
  
#Variable edad al cuadrado
base_nueva<-base_nueva %>% 
  mutate(edad_2 = edad*edad)

# ---------------ESTADISTICAS DESCRIPTIVAS Y GRAFICOS---------------#

head(base_nueva)
glimpse(base_nueva)
skim(base_nueva)
skim(base_nueva$age)

#Graficos

#Scatter

ggplot(data = base_nueva, aes(x = edad, y = salario_mensual)) + 
  geom_point(color = "red", size = 0.5) +
  labs(x = "Edad", y = "Logaritmo Ingresos mensuales") +
  scale_y_continuous(labels = scales::dollar, trans = "log1p",
                     breaks = c(1e5, 1e6, 1e7, 1e8))

ggplot(data = base_nueva, aes(x = maxEducLevel, y = salario_mensual)) + 
  geom_point(color = "red", size = 0.5) +
  labs(x = "Educación", y = "Logaritmo de Ingresos mensuales") +
  scale_y_continuous(labels = scales::dollar, trans = "log1p",
                     breaks = c(1e5, 1e6, 1e7, 1e8))

#Distribución horas de trabajo con "Histogram"

ggplot(data = base_nueva, aes(x = hoursWorkUsual)) +
  geom_histogram() +
  labs(x = "Total Horas Trabajadas por semana",
       y = "No Individuos",
       title = "Distribución de horas trabajadas") +
  theme_minimal()

#Distribución horas de trabajo cpn "boxplot"

ggplot(data = base_nueva, aes(x = hoursWorkUsual, y = as.factor(sex), 
                              color = as.factor(sex))) +
  geom_boxplot() +
  theme_minimal() +
  scale_color_manual(values = c("0" ="#219ebc","1" = "#ffafcc"), 
                     label = c("0" = "Hombre", "1" = "Mujer"),
                     name = "Sexo") +
  labs(x = "Total Horas Trabajadas por semana",
       y = "Sexo",
       title = "Distribución de horas trabajadas por sexo ")


#Distribución de las variables

ggplot(base_nueva, aes(x = edad)) +
  geom_histogram ()

#Estadisticas descriptivas

stargazer(base_nueva, header = FALSE, type="text", title="Variables incluidas en la base de Datos")

stargazer(base_nueva, header = FALSE, type = "text", 
          title = "Estadisticas Descriptivas", digits = 1,
          covariate.labels = c("Salario", "Edad"),
          summary.stat = c("n", "min", "p25", "median", "mean","p75", "sd", "max"))

as.data.frame(base_nueva)
bd<-select(base_todo, sex, age, relab, maxEducLevel, p6870, y_salary_m_hu, p6500, p6510s1, hoursWorkUsual)

bd<-na.omit(bd)


#Cálculo del salario:
bd<-bd %>%
  mutate(salario_mensual=p6500+p6510s1)%>%
  mutate(salario_mensual_hora=salario_mensual/(hoursWorkUsual*4))

bd<-select(bd, sex, edad, relab, maxEducLevel, hoursWorkUsual, salario_mensual, salario_mensual_hora, log_salario_mensual_hora)

bd<-na.omit(bd)


#Logaritmo del salario
bd<-bd %>%
  mutate(log_salario_mensual_hora=log(salario_mensual_hora))

#Variable mujer. Cambio de Sex a female con female=1
bd<-bd %>%
  mutate(female = if_else(sex == 0, 1, 0) )

#En edad sólo mayores de 18 años
bd<-bd%>%
  filter(age>=18)

#Cambio de nombre para variable p6870: total personas de empresa donde labora:
bd<-bd %>% 
  rename(tam_empresa = p6870)

#Cambio de nombre para variable edad
bd<-bd %>% 
  rename(edad = age)

#Variable edad al cuadrado
bd<-bd %>% 
  mutate(edad_2 = edad*edad)

stargazer(bd, header = FALSE, type="text", title="Variables incluidas en la base de Datos")

##################
##  EJERCICIO 3.##
##################

#Analisis descriptivo y gráfico

summary(base_nueva$salario_mensual_hora)

stargazer(base_nueva, type = "text", digits = 1)

stargazer(base_nueva[c("salario_mensual_hora", "edad")], header = FALSE, type = "text", 
          title = "Estadisticas Descriptivas", digits = 1,
          covariate.labels = c("Salario", "Edad"),
          summary.stat = c("n", "min", "p25", "median", "mean","p75", "sd", "max")
)

g1 <- ggplot(data = base_nueva, mapping = aes(x = edad, y= log_salario_mensual_hora))+
  geom_point(col = "#A80000" , size = 0.9)+
  labs(x = "edad",
       y = "logaritmo del salario por hora",
       title = "Edad y Salario")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

g1

g2<- ggplot(data = base_nueva)+
  geom_histogram(mapping = aes(x=log_salario_mensual_hora),col= "#A80000", fill = "#A80000")+
  labs(x="logaritmo del salario",
       y="frecuencia",
       title = "Distribucion del salario")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

g2

g3 <- ggplot(data = base_nueva)+
  geom_boxplot(mapping = aes(x=salario_mensual_hora), fill = "#A80000", alpha =0.5)+
  labs(x="salario",
       title = "Box Plot")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

g3

g4 <- ggplot(data = base_nueva)+
  geom_boxplot(mapping = aes(x=log_salario_mensual_hora), fill = "#A80000", alpha =0.5)+
  labs(x="logaritmo del salario",
       title = "Box Plot")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

g4

grid.arrange(g3,g4, nrow = 1, widths = c(1,1.5))

quantile(x=base_nueva$salario_mensual_hora)

IQR(x=base_nueva$salario_mensual_hora)

# i) Regresion 

reglsalario_mes_hora <- lm(log_salario_mensual_hora ~ edad + edad_2, data = base_nueva)
reglsalario_mes_hora

stargazer(reglsalario_mes_hora, type = "text",
          title = "Resultados de la Regresion",
          aling = TRUE, digits=7,
          dep.var.labels = "Logaritmo del Salario",
          covariate.labels = c("edad", "edad al cuadrado")
)


base_nueva$res_log_salario_mensual_hora <- reglsalario_mes_hora$residuals

base_nueva$log_salario_mensual_hora_hat = predict(reglsalario_mes_hora)

# iii) Model's in sample fit

mean(base_nueva$res_log_salario_mensual_hora)

ggplot(data=base_nueva, mapping = aes(x = log_salario_mensual_hora_hat, y = res_log_salario_mensual_hora))+
  geom_point(col = "#A80000" , size = 0.9)+
  geom_hline(yintercept=0, linetype="dashed")+
  geom_smooth()+
  labs(x = "Fitted values",
       y = "Residuals",
       title = "Residual Plot for Quadratic Fit")+
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

qqnorm(base_nueva$res_log_salario_mensual_hora)

# iv) plot of the estimated edad-earning profile

summ = base_nueva %>%
  group_by(
    edad, edad_2
  ) %>%
  summarize(
    mean_log_salario_mensual_hora = mean(log_salario_mensual_hora),
    log_salario_mensual_hora_hat_reg = mean(log_salario_mensual_hora_hat), .groups = "drop"
  )

ggplot(summ) +
  geom_point(
    aes(x = edad, y = mean_log_salario_mensual_hora),
    color = "#A80000", size = 2
  ) +
  geom_line(
    aes(x = edad, y = log_salario_mensual_hora_hat_reg),
    color = "#003399", size = 1
  ) +
  labs(
    title = "Logaritmo del Salario por Edad",
    x = "edad",
    y = "logaritmo del salario"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

### Bootstrap de "peak edad"

reglsalario_mes_hora <- lm(log_salario_mensual_hora ~ edad + edad_2, data = base_nueva)

# i) Obtenemos los coeficientes de la regresion

coefs <- reglsalario_mes_hora$coefficients
coefs

# ii) Extrayendo los coeficientes como escalar

b0 <- coefs[1]
b1 <- coefs[2]
b2 <- coefs[3]

# Calculo del "peak edad"

peak_edad <- (-b1/(2*b2))
peak_edad

# Calculo de errores estandar

eta_reglsalario_mes_hora_fn <- function(data, index){
  coefs <- lm(log_salario_mensual_hora ~ edad + edad_2, data, subset = index)$coefficients
  b1 <- coefs[2]
  b2 <- coefs[3]
  peak_edad <- (-b1/(2*b2))
  return(peak_edad)
}

eta_reglsalario_mes_hora_fn(base_nueva,1:nrow(base_nueva))

# Implementamos boot

set.seed(123)

resultados <- boot(base_nueva, eta_reglsalario_mes_hora_fn, R=10000)
resultados

# Intervalo de confianza

boot.ci(resultados, type = c("norm", "basic"))


hist(resultados$t)
qqnorm(resultados$t, datax = T)

