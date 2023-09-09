####################### TALLER 1 ##########################
######## PUNTO 1 #######
#### CARGUE DE BASE ####
## Creamos un vector con los diferentes links ##
link <- paste0("https://ignaciomsarmiento.github.io/GEIH2018_sample/pages/geih_page_", 1:10, ".html")


## Se hace un bucle que una las 10 bases
base <- data.frame()
for(i in link){
  print(i)
  GEIH<- read_html(i) %>% 
    html_table() %>%
    as.data.frame()
  base <- rbind(base, GEIH)
}

#### LIMPIEZA DE DATOS ####
summary(base)
str(base)
## Se realizo una tabla de datos vacios
vacios <- data.frame(apply(X = is.na(base), MARGIN =2, FUN = sum))
vacios$porcentaje_vacios <- round(vacios$apply.X...is.na.base...MARGIN...2..FUN...sum./32177,3)
colnames(vacios)[1]<- "Cantidad_vacios"

## Se seleccionan las varaibles de interés
base2 <- select(base, "estrato1",
                "sex",
                "age",
                "p6240",
                "p6426",
                "ina",
                "maxEducLevel",
                "ocu",
                "dsi")

## Se filtra la base ##
base_filtrada <- subset(base2, base2$age >= 18 & base2$ocu == 1)

## Se dispone a eliminar todos los vacios

nombres <- colnames(base)
write.csv(nombres,"Nombres varaibles.csv")
