N <- nrow(final_db)

vars_num <- c("ingreso_hora", "age", "totalHoursWorked")
vars_cat <- c("female", "household_head", "estrato1", "informal", 
              "maxEducLevel", "microEmpresa")

#Numeric

desc_num <- final_db %>%
  select(all_of(vars_num)) %>%
  skim()  

stargazer(final_db[vars_num],
          type = "html", # Puedes poner "text" si quieres ver en consola
          title = paste("Estadísticas descriptivas (N =", N, ")"),
          digits = 2,
          summary.stat = c("mean","sd","min","max","median"),
          out = "descriptivos_numericos.doc")

#Categorical 

doc <- read_docx()

for (v in vars_cat) {
  # Tabla de frecuencias
  tabla <- final_db %>%
    count(!!sym(v)) %>%
    mutate(prop = round(100 * n / sum(n), 2))
  ft <- flextable(tabla)
  doc <- doc %>%
    body_add_par(paste("Frecuencias para:", v), style = "heading 2") %>%
    body_add_flextable(ft) %>%
    body_add_par("", style = "Normal") # línea en blanco
}

print(doc, target = "frecuencias_categoricas.docx")
