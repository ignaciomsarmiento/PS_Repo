############################## Problem Set 1 ###################################
# Autores: David Stiven Peralta M...
# Si encuentra alg?n error o tiene sugerencias por favor cont?cteme
# correo: ds.peralta@uniandes.edu.co
# fecha: 01/02/2021
################################################################################

#Organizo el directorio y descargo los paquetes requeridos

rm(list = ls()) # Limpiar Rstudio
options(scipen = 20,  digits=3) # establezco la notacion científica y el número de decimales
require(pacman)
p_load(ggplot2, rio, tidyverse, skimr, caret, rvest, magrittr, openxlsx,
       rstudioapi, readxl, openxlsx, stargazer, boot) # Cargo varios paquetes al tiempo

# Determino el directorio de trabajo
escritorio <- rstudioapi::getActiveDocumentContext()$path #accedo a la ruta del archivo
carpeta <- dirname(escritorio) # extraigo el nombre de la carpeta donde está guardado el archivo
setwd(carpeta) # Establezco el directorio de trabajo como aquel donde está guardado el archivo
getwd() # verifico el directorio
rm(carpeta, escritorio) # Limpio el ambiente

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
         sexo = "sex", #1=hombre
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
         ingreso_mensual = "y_bonificaciones_m", # revisar estadísticas si son coherentes
         ingreso_mensual_independientes = "y_gananciaIndep_m", # revisar estadísticas si son coherentes
         salario_real_hora = "y_salary_m_hu", # revisar estadísticas si son coherentes
         ingreso_hogarmes_nominal = "y_vivienda_m",# revisar estadísticas si son coherentes
         pension = "cotPension", # cotiza pensión
         salud = "regSalud", # cotiza salud
         urbano = "clase") %>% # reside en cabecera
  
  mutate(parentesco_jhogar = if_else(parentesco_jhogar == 1, 1, 0), # Dummy para jefes de hogar=1
         urbano = if_else(urbano == 1, 1, 0), # dummy para cabecera municipal =1
         sexo = ((sexo*-1)+1), #dummy que toma valor 1 para mujeres (punto 2)
         edad2 = edad*edad, # creo edad al cuadrado
         log_salario_hora = ifelse(salario_real_hora == 0, NA, log(salario_real_hora))) %>% # log salario por hora con la variable creada por ignacio # REVISAR AJUSTE DE LA VARIABLE

  filter (edad >= 18) # filtro por mayores de edad

names(GEIH)
rm(base_geih2018)


# DE AQUÍ PARA ABAJO ESTÁ EN CONSTRUCCIÓN (todavía lo estoy ajustando)
# Evalúo la viabilidad de tratar los valores de NA en la variable de interés salario_real_hora
sum(GEIH$desempleado != 0 & is.na(GEIH$salario_real_hora)) #Evalúo cuántas personas están desempleadas para dejarlas sin ajustes
sum(GEIH$desempleado == 0 & is.na(GEIH$salario_real_hora)) #Evalúo cuántas personas están empleadas y reportan un salario de 0
GEIH$llave <- paste(GEIH$directorio, GEIH$secuencia_p) # creo una llave de hogar 
GEIH$llave

GEIH <- GEIH %>%
  group_by(llave) %>%
  mutate(condicion = ifelse(is.na(salario_real_hora) & any(!is.na(salario_real_hora)), 1, 0)) %>%
  ungroup()


GEIH %>%
mutate(condicion = ifelse(any(is.na(salario_real_hora)) & any(!is.na(salario_real_hora)), 1, 0))

sum(GEIH$condicion)

# Creo los estadísticos descriptivos de las principales variables de interés
var_interes <- GEIH [, c("log_salario_hora",
                         "salario_real_hora", "edad", "sexo", "educacion_alcanzada",
                         "ocupacion", "formal_informal",  "parentesco_jhogar",
                         "desempleado")] # "años_educacion", "emprendedor", 

GEIH <- na.omit(GEIH[, "log_salario_hora", drop = FALSE])


skim(var_interes) # obtengo las estadísticas descriptivas de las principales variables de interés

########### Puntos a tener en cuenta para la limpieza de datos #################
# Evaluar outliers de experiencia, educación, edad, género, por salario

table(GEIH$secuencia_p)
################### FIn #######################################################