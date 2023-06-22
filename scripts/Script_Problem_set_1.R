############################## Problem Set 1 ###################################
# Autores: David Stiven Peralta M...
# Si encuentra alg?n error o tiene sugerencias por favor cont?cteme
# correo: ds.peralta@uniandes.edu.co
# fecha: 01/02/2021
################################################################################

#Organizo el directorio y descargo los paquetes requeridos

rm(list = ls()) # Limpiar Rstudio
options(scipen = 20,  digits=3) # establezco la notacion científica y el número de decimales
require(pacman) %>%
  p_load(ggplot2, rio, tidyverse, skimr, caret, rvest, magrittr, openxlsx, rstudioapi) # Cargo varios paquetes al tiempo

# Determino el directorio de trabajo
escritorio <- rstudioapi::getActiveDocumentContext()$path
carpeta <- dirname(escritorio)
setwd(carpeta)

# descargo el contenido de las 10 páginas
tablas_html <- vector("list", 10) # creo una lista para guardar los enlaces
for (i in 1:10) {
  urls <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", i, ".html")
  tablas_html[[i]] <- read_html(urls) %>% html_table()
} # creo un loop para descargar las 10 tablas de la página y los guardo en el vector "tablas_html"
print(tablas_html) # Verifico que la información haya quedado bien descargada

# Concateno el contenido de las 10 tablas
base_geih2018 <- bind_rows(tablas_html, .id = "numero_de_base")
as.data.frame(base_geih2018)
glimpse(base_geih2018)

class(base_geih2018)











########### Puntos a tener en cuenta para la limpieza de datos #################
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

# Exportar la base unida a Excel
p_load(openxlsx)
write.xlsx(base_geih2018, "Base_unida")
