
#####

#6. Predicting wages

#Split data (70% training, 30% testing)

set.seed(10101)
idx <- sample(seq_len(nrow(final_db)), size = 0.7*nrow(final_db))
train <- final_db[idx, ]
test  <- final_db[-idx, ]

#Specifications 

controls <- c("age", "age2", "household_head","estrato11", "estrato13", "estrato14", 
              "estrato15", "estrato16", "informal","totalHoursWorked", "maxEducLevel1", 
              "maxEducLevel3", "maxEducLevel5", "maxEducLevel6", "maxEducLevel7", 
              "microEmpresa")


#1. Brecha salarial por género sin controles
lm_gender1 <- lm(log(ingreso_hora) ~ female, data = train)

#2. Brecha salarial con controles (FWL directo)
fwl_model <- lm(log(ingreso_hora) ~ female + ., 
                data = train[, c("ingreso_hora", "female", controls)])

#3. Interacción female * household_head
controls_ <- setdiff(controls, "household_head")
reg_hh_gender <- as.formula(
  paste("log(ingreso_hora) ~ female + household_head + female:household_head +",
        paste(controls_, collapse = " + ")))
lm_hh_gender <- lm(reg_hh_gender, data = train)

#4. Interacción edad * female
reg_age_gender <- lm(log(ingreso_hora) ~ age * female + ., 
                     data = train[, c("ingreso_hora", "female", controls_)])

#5. Interacción female * informal
controls_noinf <- setdiff(controls, "informal")
reg_inf_gender <- lm(log(ingreso_hora) ~ female * informal + ., 
                     data = train[, c("ingreso_hora", "female", "informal", controls_noinf)])

#6. Log de horas trabajadas
mod_loghours <- lm(log(ingreso_hora) ~ log(totalHoursWorked) + female + ., 
                   data = train[, c("ingreso_hora", "female", controls)])
summary(final_db$totalHoursWorked)

#7. Interacciones educación * female
mod_edu_interac <- lm(log(ingreso_hora) ~ female * (maxEducLevel3 + maxEducLevel5 + 
                                                      maxEducLevel6 + maxEducLevel7) + ., 
                      data = train[, c("ingreso_hora", "female", controls)])

model_list <- list(
  gender_only    = lm_gender1,
  gender_fwl     = fwl_model,
  reg_hh_gender  = lm_hh_gender,
  reg_age_gender = reg_age_gender,
  reg_inf_gender = reg_inf_gender,
  mod_loghours   = mod_loghours,
  mod_edu_interac= mod_edu_interac)

stargazer(model_list,
          type = "html",
          out = "female_interacciones.doc",
          title = "Efecto de ser Mujer y sus Interacciones",
          dep.var.labels = "log(Salario por hora)",
          keep = c("female"),   # solo female y sus interacciones
          star.cutoffs = c(0.1, 0.05, 0.01),
          digits = 4,
          omit.stat = c("f", "ser")) 

#Computing RSME

rmse_results <- lapply(names(model_list), function(name) {
  model <- model_list[[name]]
  y_test <- log(test$ingreso_hora)
  yhat   <- predict(model, newdata = test)
  rmse   <- sqrt(mean((y_test - yhat)^2, na.rm = TRUE))
  data.frame(model = name, RMSE = rmse)
}) %>% bind_rows()

print(rmse_results)

ft_rmse <- flextable(rmse_results) %>%
  theme_booktabs() %>%
  autofit() %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 11, part = "all") %>%
  add_footer_lines("Nota: RMSE calculado sobre la muestra de test.")

doc <- read_docx() %>%
  body_add_par("Resultados de RMSE por Modelo", style = "heading 1") %>%
  body_add_flextable(ft_rmse)

print(doc, target = "RMSE_resultados.docx")

#Prediction error

y_test <- log(test$ingreso_hora)
yhat_best <- predict(reg_inf_gender, newdata = test)

errors <- y_test - yhat_best
summary(errors)

#Errors distribution

ggplot(data.frame(errors = errors), aes(x = errors)) +
  geom_histogram(bins = 30, fill = "lightblue", color = "white") +
  labs(title = "Distribución de errores",
       x = "Error de predicción",
       y = "Frecuencia") +
  theme_minimal()

#Identifying outliers

q <- quantile(errors, c(0.01, 0.99))
outliers <- test[errors < q[1] | errors > q[2], ]
head(outliers)


#LOOCV para mejor modelo

 #RUN THE MODEL WITH ALL OBS (Informal*gender)

full_model <- lm(reg_inf_gender,
                 data = final_db)

X<- model.matrix(full_model)
y <- model.response(model.frame(full_model))

beta_hat <- full_model$coefficients

## Calculate the inverse of  (X'X), call it G_inv

G_inv<- solve(t(X)%*%X)

## and 1/1-hi

vec<- 1/(1-hatvalues(full_model))

N <- nrow(X)  # Number of observations
LOO <- numeric(N)  # To store the errors

# Loop over each observation
for (i in 1:N) {
  # get the new beta
  new_beta<- beta_hat  - vec[i] * G_inv %*% as.vector(X[i, ]) * full_model$residuals[i]
  ## get the new error
  new_error<- (y[i]- (X[i, ] %*% new_beta))^2
  LOO[i]<-  new_error
}

looCV_error_model1 <- mean(LOO)
sqrt(looCV_error_model1)


## RUN THE MODEL WITH ALL OBS (Worked hours log transformation)

full_model <- lm(mod_loghours,
                 data = final_db)

X<- model.matrix(full_model)
y <- model.response(model.frame(full_model))

beta_hat <- full_model$coefficients

## Calculate the inverse of  (X'X), call it G_inv
G_inv<- solve(t(X)%*%X)

## and 1/1-hi
vec<- 1/(1-hatvalues(full_model))

N <- nrow(X)  # Number of observations
LOO <- numeric(N)  # To store the errors

# Loop over each observation
for (i in 1:N) {
  # get the new beta
  new_beta<- beta_hat  - vec[i] * G_inv %*% as.vector(X[i, ]) * full_model$residuals[i]
  ## get the new error
  new_error<- (y[i]- (X[i, ] %*% new_beta))^2
  LOO[i]<-  new_error
}

looCV_error_model2 <- mean(LOO)
sqrt(looCV_error_model2)

 #Results table 

rmse1 <- sqrt(looCV_error_model1)
rmse2 <- sqrt(looCV_error_model2)

loocv_results <- data.frame(
  Modelo = c("reg_inf_gender", "mod_loghours"),
  `LOOCV RMSE` = c(rmse1, rmse2)
)

ft_loo <- flextable(loocv_results) %>%
  theme_booktabs() %>%
  autofit() %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 11, part = "all") %>%
  add_footer_lines("Nota: LOOCV RMSE = raíz del error cuadrático medio bajo validación cruzada leave-one-out.")

doc <- read_docx() %>%
  body_add_par("Comparación de modelos con LOOCV", style = "heading 1") %>%
  body_add_flextable(ft_loo)

print(doc, target = "LOOCV_RMSE.docx")

