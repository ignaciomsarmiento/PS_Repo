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
variables <- base_geih2018[, c("age",	"clase",	"depto",	"directorio",	"dominio",
                               "dsi",	"estrato1",	"fex_c",	"fex_dpto",	"formal",
                               "fweight",	"p6050",	"p6210",	"maxEducLevel",
                               "p6240",	"p6426",	"p6630s6",	"p6630s6a1",
                               "p7310",	"p7495",	"p7500s1",	"p7500s1a1",
                               "p7500s2",	"p7500s2a1",	"p7500s3",	"p7500s3a1",
                               "p7505",	"pet",	"relab",	"secuencia_p",	"sex",
                               "sizeFirm",	"totalHoursWor~d",	"wap",
                               "y_accidentes_m",	"y_bonificacio~m",
                               "y_especie_m",	"y_gananciaInd~u",
                               "y_gananciaN~o_m",	"y_salary_m",	"y_salary_m_hu",
                               "y_vivienda_m",
)]


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


