############################## Problem Set 1 ###################################
# Autores: David Stiven Peralta M...
# Si encuentra alg?n error o tiene sugerencias por favor cont?cteme
# correo: ds.peralta@uniandes.edu.co
# fecha: 01/02/2021
################################################################################

# 1) Organizo el directorio y descargo los paquetes requeridos #################

rm(list = ls()) # Limpiar Rstudio
options(scipen = 20,  digits=3) # establezco la notacion científica y el número de decimales
require(pacman)
p_load(ggplot2, rio, tidyverse, skimr, caret, rvest, magrittr, openxlsx,
       rstudioapi, readxl, openxlsx, stargazer, boot, car, flextable) # Cargo los paquetes de interés para ejecutar el código

# Determino el directorio de trabajo
escritorio <- rstudioapi::getActiveDocumentContext()$path #accedo a la ruta del archivo
carpeta <- dirname(escritorio) # extraigo el nombre de la carpeta donde está guardado el archivo
setwd(carpeta) # Establezco el directorio de trabajo como aquel donde está guardado el archivo
getwd() # verifico el directorio
rm(carpeta, escritorio) # Limpio el ambiente

# 2) Descargo y limpio la data para el análisis ################################
#(OMITO ESTAS LÍNEAS PORQUE IMPORTÉ EL ARCHIVO YA QUE ASÍ ES MENOS PESADO. AL FINAL, VOLVERÉ A ACTIVAR ESTAS LÍNEAS)
# descargo el contenido de las 10 páginas 
#tablas_html <- vector("list", 10) # creo una lista para guardar los enlaces
#for (i in 1:10) {
#  urls <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", i, ".html")
#  tablas_html[[i]] <- read_html(urls) %>% html_table()
#} # creo un loop para descargar las 10 tablas de la página y los guardo en el vector "tablas_html"
#print(tablas_html) # Verifico que la información haya quedado bien descargada

# Concateno el contenido de las 10 tablas
#base_geih2018 <- bind_rows(tablas_html, .id = "numero_de_base")
#as.data.frame(base_geih2018)
#glimpse(base_geih2018)

# Exporto la base unida a Excel
#write.xlsx(base_geih2018, "Base_unida")

# Cargo la base
base_geih2018 <- read_excel("Base_unida")

# Determino las variables de interés y elimino las demás
GEIH <- base_geih2018 %>%
  select( "age", "sex", "maxEducLevel", "p6210s1","cuentaPropia", "pea", "wap",
         "p6240", "relab", "sizeFirm", "dsi",	"estrato1", "formal", "p6050",
         "p6426", "totalHoursWorked", "y_bonificaciones_m", "y_gananciaIndep_m",
         "y_salary_m_hu", "y_vivienda_m", "ingtot", "p7040", "cotPension",
         "regSalud", "clase", "directorio", "dominio","fex_c", "fex_dpto", "fweight",
         "depto", "secuencia_p", "orden") %>% # 33 variables seleccionadas de la base bruta

  rename(edad ="age",
         mujer = "sex", #1= hombre en la línea 78 invierto la variable
         educacion_alcanzada = "maxEducLevel", # omitimos nivel_educativo = "p6210" ya que maxEduclevel tiene la información de esta variable
         educacion_tiempo = "p6210s1",
         emprendedor = "cuentaPropia",
         poblacion_economicamente_activa = "pea",
         poblacion_edad_trabajar = "wap", # Omitimos "pet" ya que todos los valores dan 1
         ocupacion = "p6240",
         relacion_laboral = "relab",
         tamaño_empresa = "sizeFirm", # maybe funciona
         desempleado = "dsi", # 1 = desempleado
         estrato = "estrato1",
         formal_informal = "formal", # 1 = formal
         parentesco_jhogar = "p6050",
         tiempo_trabajando = "p6426", # la variable está en meses
         t_horas_trabajadas = "totalHoursWorked",
         ingreso_mensual = "y_bonificaciones_m",
         ingreso_mensual_independientes = "y_gananciaIndep_m",
         salario_real_hora = "y_salary_m_hu",
         ingreso_hogarmes_nominal = "y_vivienda_m",
         pension = "cotPension", # cotiza pensión
         salud = "regSalud", # cotiza salud
         urbano = "clase") %>% # reside en cabecera
  
  mutate(parentesco_jhogar = if_else(parentesco_jhogar == 1, 1, 0), # Dummy para jefes de hogar=1
         urbano = if_else(urbano == 1, 1, 0), # dummy para cabecera municipal =1
         mujer = ((mujer*-1)+1), #dummy que toma valor 1 para mujeres (punto 2)
         edad2 = edad*edad) %>% # creo edad al cuadrado

  filter (edad >= 18 & desempleado !=1) # filtro por mayores de edad y empleados

names(GEIH)
rm(base_geih2018)

# calcular promedio de salario por hogar para quienes trabajan
salario_promedio_hogar <- GEIH %>%
  group_by(directorio, secuencia_p) %>% # Agrupo por identificación de familia
  summarise(salario_promedio_hogar = mean(salario_real_hora, na.rm = T)) %>% # saco el promedio de salario por familia omitiendo los valores de NA
  filter(!(is.na(salario_promedio_hogar))) %>% # filtro por valores diferentes a NA en la variable salario promedio
  ungroup()

#Uno las bases con el identificador de directorio y secuencia_p
GEIH <- left_join(GEIH, salario_promedio_hogar, by = c("directorio", "secuencia_p"))
rm(salario_promedio_hogar)

# Imputo los valores promedio por familia a los valores de NA con personas que reportaron estar empleadas
GEIH <- GEIH %>% 
  mutate(salario_real_hora_imputado = ifelse(is.na(salario_real_hora), salario_promedio_hogar, salario_real_hora), # Creo una nueva variable de salario por hora imputando los valores de los salarios promedio por hogar
         log_salario_hora_imputado = log(salario_real_hora_imputado))
         
sum(is.na(GEIH$salario_real_hora_imputado)) # evalúo el total de valores con NA después de la imputación.

# Evaluó el salario_hora con valores imputados y sin valores imputados
var_salario <- GEIH [, c("salario_real_hora", "salario_real_hora_imputado")]

# Evalúo los descriptivos de comparación entre el salario sin y con imputación de valores
var_salario %>%
  skim_without_charts()

# Evalúo outliers de las variables de continuas
var_outliers <- GEIH [, c("salario_real_hora_imputado", "edad",
                          "educacion_tiempo")]

# Establecer el diseño de la ventana de gráficos
par(mfrow = c(1, 3))  # Ajusta los valores de "filas" y "columnas" según tus necesidades

# Evalúo outliers de mis variables de interés con boxplot
for (variable in colnames(var_outliers)) {
  boxplot(var_outliers[[variable]], main = variable)
}

# Evalúo valores estadísticamente atípicos mediante prueba de significancia outlierTest
for (variable in colnames(var_outliers)) {
  formula <- paste(variable, "~ 1")
  lm_outliers <- lm(formula, data = var_outliers, na.action = na.omit)
  outlier_test <- outlierTest(lm_outliers)

  cat("Variable:", variable, "\n")
  summary(lm_outliers)
  print(outlier_test)
  cat("\n")
}

# analizo los outliers para evaluar la coherencia de las observaciones
GEIH[c(6790, 7313, 21436, 9000, 14623, 1538, 1539, 6788, 6789, 4803),
     c("salario_real_hora_imputado", "edad", "educacion_alcanzada",
       "educacion_tiempo", "mujer", "emprendedor", "ocupacion",
       "formal_informal", "parentesco_jhogar", "salario_promedio_hogar")] # variable salario_real_hora_imputado

GEIH[c(9409),
     c("salario_real_hora_imputado", "edad", "educacion_alcanzada",
       "educacion_tiempo", "mujer", "emprendedor", "ocupacion",
       "formal_informal", "parentesco_jhogar", "salario_promedio_hogar")] # variable edad

GEIH[c(20957, 21792),
     c("salario_real_hora_imputado", "edad", "educacion_alcanzada",
       "educacion_tiempo", "mujer", "emprendedor", "ocupacion",
       "formal_informal", "parentesco_jhogar", "salario_promedio_hogar")] # variable educacion_tiempo

# Elimino los datos incoherentes
GEIH <- GEIH[-c(9409, 20957, 21792), , drop = FALSE]

# Elimino variables con NA en la variable de interés salario_real_hora_imputado
GEIH <- GEIH[complete.cases(GEIH$salario_real_hora_imputado), , drop = FALSE]

# Exporto la base limpia
write.xlsx(GEIH, "GEIH")

# 3) Evalúo las estadísticas descriptivas de las variables de interés #########
rm(list = ls()) # Limpiar Rstudio
# Cargo la base limpia
GEIH <- read_excel("GEIH") # Si no quiere correr todo el código, pueden correr esta línea y les importa la base límpia
names(GEIH)

# Creo los estadísticos descriptivos de las principales variables de interés
var_interes <- GEIH [, c("salario_real_hora_imputado", "log_salario_hora_imputado", "edad", "edad2", "mujer",
                         "educacion_alcanzada", "educacion_tiempo",
                         "emprendedor", "ocupacion", "formal_informal",
                         "parentesco_jhogar", "salario_promedio_hogar",
                         "relacion_laboral", "tamaño_empresa")]

descriptivos <- var_interes [, c("salario_real_hora_imputado", "edad",
                         "educacion_tiempo", "mujer", "emprendedor",
                         "formal_informal", "parentesco_jhogar")]

# Evalúo los descriptivos de las variables de interés en una tabla para exportar
tabla_estadisticas <- tibble()  # Creo una tibble vacía para almacenar los resultados
for (variable in colnames(descriptivos)) {
  estadisticas <- descriptivos %>%
    summarise(n_observaciones = sum(!is.na(.data[[variable]])),
              promedio = mean(.data[[variable]], na.rm = TRUE),
              sd = sd(.data[[variable]], na.rm = TRUE),
              mín = min(.data[[variable]], na.rm = TRUE),
              max = max(.data[[variable]], na.rm = TRUE)) %>%
    add_column(variable = variable, .before = 1)
  
  tabla_estadisticas <- bind_rows(tabla_estadisticas, estadisticas)  # Agregar las estadísticas a la tabla
}

tabla_estadisticas

# Crear la tabla en formato APA
tabla_01 <- flextable(tabla_estadisticas)
tabla_01 <- theme_booktabs(tabla_01)
tabla_01 <- autofit(tabla_01)
tabla_01

par(mfrow = c(1, 2))
# Crear gráfica de frecuencias para el salario
filtro_salario <- GEIH$salario_real_hora_imputado[
  GEIH$salario_real_hora_imputado <= mean(GEIH$salario_real_hora_imputado) +
    3 * sd(GEIH$salario_real_hora_imputado)] # omito valores superiores a 3 ds para facilitar la visualización gráfica

hist(filtro_salario,
     breaks = 50,
     main = "Frecuencias de salario por hora",
     xlab = "Salario por hora", ylab = "Frecuencia") #creo el histograma

filtro_logsalario <- GEIH$log_salario_hora_imputado[
  GEIH$log_salario_hora_imputado <= mean(GEIH$log_salario_hora_imputado) +
    3 * sd(GEIH$log_salario_hora_imputado)] # omito valores superiores a 4 ds para facilitar la visualización gráfica

hist(filtro_logsalario,
     breaks = 50,
     main = "Frecuencias de salario por hora",
     xlab = "Log de Salario por hora", ylab = "Frecuencia") #creo el histograma

# Crear gráfica de frecuencias para la edad
hist(GEIH$edad,
     breaks = 50,
     main = "Frecuencias de edad",
     xlab = "años", ylab = "Frecuencia") #creo el histograma

porcentaje_mayores <- sum(GEIH$edad > 60)/sum(GEIH$edad > 17) *100 #calculo el porcentaje de personas mayores de 60 años
porcentaje_mayores

# Crear gráfica de frecuencias para los años de educación
hist(GEIH$educacion_tiempo,
     breaks = 20,
     main = "Frecuencias de años de educación",
     xlab = "años de educación", ylab = "Frecuencia") #creo el histograma

porcentaje_mayores <- sum(GEIH$edad > 60)/sum(GEIH$edad > 17) *100 #calculo el porcentaje de personas mayores de 60 años
porcentaje_mayores

################### FIn #######################################################