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
p_load(ggplot2, rio, tidyverse, skimr, caret, rvest, magrittr, openxlsx, rstudioapi, readxl, openxlsx) # Cargo varios paquetes al tiempo

# Determino el directorio de trabajo
escritorio <- rstudioapi::getActiveDocumentContext()$path #accedo a la ruta del archivo
carpeta <- dirname(escritorio) # extraigo el nombre de la carpeta donde está guardado el archivo
setwd(carpeta) # Establezco el directorio de trabajo como aquel donde está guardado el archivo
rm(carpeta, escritorio) # Limpio el ambiente

# descargo el contenido de las 10 páginas (OMITO ESTAS LÍNEAS PORQUE IMPORTÉ EL ARCHIVO YA QUE ASÍ ES MENOS PESADO. AL FINAL, VOLVERÉ A ACTIVAR ESTAS LÍNEAS)
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
names(base_geih2018)
variables <- base_geih2018 %>%
  select("age", "sex", "maxEducLevel", "pea", "pet", "wap", "p6240", "relab",
          "sizeFirm", "dsi",	"estrato1", "formal", "p6050",	"p6210", 	"p6426",
            "totalHoursWorked", "y_bonificaciones_m", "y_especie_m",
              "y_gananciaIndep_m", "y_gananciaIndep_m_hu", "y_salary_m",
                "y_salary_m_hu", "y_vivienda_m", "directorio", "dominio",
                  "fex_c", "fex_dpto", "fweight", "depto", "clase",
                    "secuencia_p") %>%
  rename(edad ="age",
         sexo = "sex",
         educacion_alcanzada = "maxEducLevel",
         poblacion_economicamente_activa = "pea",
         poblacion_edad_trabajar = "pet",
         Working_age = "wap",
         ocupacion = "p6240",
         tipo_ocupacion = "relab",
         tamaño_empresa = "sizeFirm",
         empleado_desempleado = "dsi",
         estrato = "estrato1",
         formal_informal = "formal",
         parentesco_jhogar = "p6050",	
         nivel_educativo = "p6210",
         tiempo_trabajando = "p6426",
         t_horas_trabajadas = "totalHoursWorked",
         ingreso_mensual = "y_bonificaciones_m",
         ingreso_mensual_especie = "y_especie_m",
         ingreso_mensual_independientes = "y_gananciaIndep_m",
         ingreso_hora_independiente = "y_gananciaIndep_m_hu",
         salario_nominal_mensual = "y_salary_m",
         salario_real_hora = "y_salary_m_hu",
         ingreso_hogarmes_nominal = "y_vivienda_m")

glimpse(variables)

########### Puntos a tener en cuenta para la limpieza de datos #################
# Las variables de interés que he observado hasta el momento son: "numero_de_base" "...2"
#"directorio" "secuencia_p" "orden" "clase" "dominio" "mes" "estrato1" "sex" "age"

# Retirar menores de 18 años
# No limitar un máximo  
# El valor a usar para el salario será 
# Primero validar si la persona tenía más integrantes en el hogar y si es así, duplicar el valor del integrante que sí tiene valor de ingreso en los demás integrantes del hogar (aplicar en personas que tengan NA en el salario). ## Evaluar cuántos datos quedarían al final.
# Evaluar outliers de experiencia, educación, edad, género, por salario
#





## Ver una de las 10 tablas importadas en la lista tablas_html
tabla_2 <- tablas_html[[2]]
view(tabla_2)

# descargar urls individualmente
table1 <- read_html("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_1.html") %>%
  html_table()


