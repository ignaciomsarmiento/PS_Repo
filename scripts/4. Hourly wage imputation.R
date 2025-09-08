
#####
#3. Data imputation

#SubSet <- final_db[, c("impa","impaes", "isa", "isaes", "Imputacion", "y_ingLab_m", "totalHoursWorked", "y_ingLab_m_ha", "ingreso_hora")]
#View(SubSet)

#Impa transformation

final_db <- final_db %>%
  mutate(impa = ifelse(impa == 0, NA, impa))

final_db <- final_db %>%
  mutate(impa = ifelse(is.na(impa), impaes, impa))

final_db <- final_db %>%
  mutate(impa = ifelse(is.na(impa), 0, impa))

#Isa transformation

final_db <- final_db %>%
  mutate(isa = ifelse(isa == 0, NA, isa))

final_db <- final_db %>%
  mutate(isa = ifelse(is.na(isa), isaes, isa))

final_db <- final_db %>%
  mutate(isa = ifelse(is.na(isa), 0, isa))

#Imputation variable

final_db$Imputacion <- final_db$impa + final_db$isa

sum(is.na(final_db$Imputacion))

#Impute y_ingLab_m

final_db <- final_db %>%
  mutate(y_ingLab_m = ifelse(is.na(y_ingLab_m), Imputacion, y_ingLab_m))

#Keep only salary > 0

final_db <- final_db %>%
  filter(y_ingLab_m > 0)

#Ingreso por hora

final_db$ingreso_hora <- final_db$y_ingLab_m / (final_db$totalHoursWorked * (30/7))
final_db$log_ing_hora <- log(final_db$ingreso_hora)
