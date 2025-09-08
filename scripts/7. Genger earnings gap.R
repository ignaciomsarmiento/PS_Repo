
#####

#5. Gender earnings gap

#a. Model w/out controls 

lm_gender1<- lm(log(ingreso_hora) ~ female, data = final_db)
robust_gender1 <- sqrt(diag(vcovHC(lm_gender1, type = "HC0")))

stargazer(lm_gender1,
          type = "html",
          se = list(robust_gender1),
          dep.var.labels = "log(Salario por hora)",
          out = "Gender 1.doc")

#b. FWL with controls 

controls <- c("age", "age2", "household_head","estrato11", "estrato13", "estrato14", 
              "estrato15", "estrato16", "informal","totalHoursWorked", "maxEducLevel1", 
              "maxEducLevel3", "maxEducLevel5",  "maxEducLevel6", "maxEducLevel7", "microEmpresa")

controls_m <- model.matrix(~ ., data = final_db[, controls, drop = FALSE])

y <- log(final_db$ingreso_hora) 
d <- final_db$female  #independent variable            
n <- nrow(final_db)

#Reg y ~ controls

reg_y <- lm(y ~ ., data = final_db[, controls])
res_y <- resid(reg_y) #y residuals
res_y

#Reg female ~ controls

reg_female <- lm(d ~ ., data = final_db[, controls])
res_female <- resid(reg_female)
res_female

#FWL estimator

fwl_model <- lm(res_y ~ res_female)
robust_fwl <- sqrt(diag(vcovHC(fwl_model, type = "HC0")))

stargazer(fwl_model,
          type = "html",
          se = list(robust_fwl),
          dep.var.labels = "log(Salario por hora)",
          out = "FWL.html")

#c. FWL w/ bootstrap

fwl_boot <- function(data, indices) {
  d_boot <- data[indices, ]
  y_boot <- log(d_boot$ingreso_hora)
  dvar_boot <- d_boot$female
  
  reg_y <- lm(y_boot ~ ., data = d_boot[, controls, drop = FALSE])
  res_y <- resid(reg_y)
  reg_d <- lm(dvar_boot ~ ., data = d_boot[, controls, drop = FALSE])
  res_d <- resid(reg_d)
  fwl_modelb <- lm(res_y ~ res_d)
  robust_fwl <- sqrt(diag(vcovHC(fwl_modelb, type = "HC0")))
  
  return(coef(fwl_modelb)[2]) #res_female beta
}

set.seed(123)
boot_fwl <- boot(data = final_db, statistic = fwl_boot, R = 1000)
boot_fwl
se_fwl <- sd(boot_fwl$t)

boot.ci(boot_fwl, type = "perc")

 #Results table

get_stars <- function(pval) {
  if (pval < 0.01) return("***")
  else if (pval < 0.05) return("**")
  else if (pval < 0.1) return("*")
  else return("")
}

coef_fwl <- coef(fwl_model)["res_female"]
se_fwl_robust <- robust_fwl["res_female"]
t_fwl <- coef_fwl / se_fwl_robust
pval_fwl <- 2 * (1 - pnorm(abs(t_fwl)))
stars_fwl <- get_stars(pval_fwl)
r2_fwl <- summary(fwl_model)$r.squared

mean_fwl_boot <- mean(boot_fwl$t)                          
se_fwl_boot   <- sd(boot_fwl$t)                            
t_boot <- mean_fwl_boot / se_fwl_boot
pval_boot <- 2 * (1 - pnorm(abs(t_boot)))
stars_boot <- get_stars(pval_boot)
r2_boot <- r2_fwl

results <- data.frame(
  Variable = c("Female", " ", "R²"),
  `FWL Robusto` = c(
    paste0(round(coef_fwl, 4), stars_fwl),
    paste0("(", round(se_fwl_robust, 4), ")"),
    round(r2_fwl, 3)
  ),
  `FWL Bootstrap` = c(
    paste0(round(mean_fwl_boot, 4), stars_boot),
    paste0("(", round(se_fwl_boot, 4), ")"),
    round(r2_boot, 3)
  )
)

ft <- flextable(results) %>%
  theme_booktabs() %>%
  autofit() %>%
  bold(part = "header") %>%
  align(align = "center", part = "all") %>%
  fontsize(size = 11, part = "all") %>%
  add_footer_lines("Notas: Errores estándar entre paréntesis. *** p<0.01, ** p<0.05, * p<0.1.")

doc <- read_docx() %>%
  body_add_par("Comparación del coeficiente Female (FWL)", style = "heading 1") %>%
  body_add_flextable(ft)

print(doc, target = "FWL_female_comparacion.docx")

#Age-wage profile for gender graph

controls_ <- setdiff(controls, c("age", "age2"))

reg_age_gender <- as.formula(
  paste("log(ingreso_hora) ~ age + female + I(age^2) + age:female + I(age^2):female +",
        paste(controls_, collapse = " + ")))

lm_age_gender <- lm(reg_age_gender, data = final_db)
summary(lm_age_gender)

#Bootstrap for age gender wage profile

pred_grid <- final_db %>%
  select(age) %>%
  distinct() %>%
  arrange(age) %>%
  crossing(female = c(0, 1)) 

pred_grid$Genero <- ifelse(pred_grid$female == 0, "Hombres", "Mujeres")

for (v in controls_) {
  pred_grid[[v]] <- mean(final_db[[v]], na.rm = TRUE)
}

boot_pred_gender <- function(data, indices) {
  d <- data[indices, ]
  m <- lm(reg_age_gender, data = d)
  predict(m, newdata = pred_grid)
}

set.seed(123)
boot_curve_gender <- boot(data = final_db, statistic = boot_pred_gender, R = 1000)

pred_grid$pred <- exp(predict(lm_age_gender, newdata = pred_grid))
pred_grid$lwr  <- apply(exp(boot_curve_gender$t), 2, quantile, 0.025)
pred_grid$upr  <- apply(exp(boot_curve_gender$t), 2, quantile, 0.975)

age_peak_gender <- function(data, indices) {
  d <- data[indices, ]
  m <- lm(reg_age_gender, data = d)
  b <- coef(m)
  
  nms <- names(b)
  name_ageint  <- nms[grep("^age:female$", nms)]
  name_age2int <- nms[grep("^I\\(age\\^2\\):female$", nms)]
  
  beta1  <- b["age"]
  beta2  <- b["I(age^2)"]
  delta1 <- if(length(name_ageint))  b[name_ageint]  else 0
  delta2 <- if(length(name_age2int)) b[name_age2int] else 0
  
  peak_m <- - beta1 / (2 * beta2)
  peak_f <- - (beta1 + delta1) / (2 * (beta2 + delta2))
  
  return(c(peak_m, peak_f))
}

set.seed(123)

boot_gender_peaks <- boot(data = final_db, statistic = age_peak_gender, R = 1000)

mean_peaks <- colMeans(boot_gender_peaks$t)
mean_peaks
names(mean_peaks) <- c("Male", "Female")

male_ci   <- boot.ci(boot_gender_peaks, type = "perc", index = 1)$percent[4:5]
female_ci <- boot.ci(boot_gender_peaks, type = "perc", index = 2)$percent[4:5]

ggplot(pred_grid, aes(x = age, y = pred, color = Genero)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = Genero), alpha = 0.2, color = NA) +
  geom_line(size = 1) +
  geom_vline(xintercept = mean_peaks["Male"], color = "blue", linetype = "dashed", size = 0.8) +
  geom_vline(xintercept = male_ci, color = "blue", linetype = "dotted", size = 0.6) +
  geom_vline(xintercept = mean_peaks["Female"], color = "red", linetype = "dashed", size = 0.8) +
  geom_vline(xintercept = female_ci, color = "red", linetype = "dotted", size = 0.6) +
  labs(
    title = "Perfil salarial estimado en función de la edad",
    subtitle = paste0(
      "Pico hombres: ~", round(mean_peaks["Male"], 1), " años (IC95%: ",
      round(male_ci[1], 1), " – ", round(male_ci[2], 1), ")\n",
      "Pico mujeres: ~", round(mean_peaks["Female"], 1), " años (IC95%: ",
      round(female_ci[1], 1), " – ", round(female_ci[2], 1), ")"
    ),
    x = "Edad",
    y = "Salario por hora esperado (COP)",
    color = "Género",
    fill  = "Género"
  ) +
  scale_color_manual(values = c("Hombres" = "dodgerblue4", "Mujeres" = "firebrick")) +
  scale_fill_manual(values  = c("Hombres" = "dodgerblue4", "Mujeres" = "firebrick")) +
  theme_minimal()


controls_ <- setdiff(controls, c("age", "age2"))

reg_age_gender <- as.formula(
  paste("log(ingreso_hora) ~ age + female + I(age^2) + age:female + I(age^2):female +",
        paste(controls_, collapse = " + ")))

lm_age_gender <- lm(reg_age_gender, data = final_db)
summary(lm_age_gender)

#Bootstrap for age gender wage profile

pred_grid <- final_db %>%
  select(age) %>%
  distinct() %>%
  arrange(age) %>%
  crossing(female = c(0, 1)) 

pred_grid$Genero <- ifelse(pred_grid$female == 0, "Hombres", "Mujeres")

for (v in controls_) {
  pred_grid[[v]] <- mean(final_db[[v]], na.rm = TRUE)
}

boot_pred_gender <- function(data, indices) {
  d <- data[indices, ]
  m <- lm(reg_age_gender, data = d)
  predict(m, newdata = pred_grid)
}

set.seed(123)
boot_curve_gender <- boot(data = final_db, statistic = boot_pred_gender, R = 1000)

pred_grid$pred <- exp(predict(lm_age_gender, newdata = pred_grid))
pred_grid$lwr  <- apply(exp(boot_curve_gender$t), 2, quantile, 0.025)
pred_grid$upr  <- apply(exp(boot_curve_gender$t), 2, quantile, 0.975)

age_peak_gender <- function(data, indices) {
  d <- data[indices, ]
  m <- lm(reg_age_gender, data = d)
  b <- coef(m)
  
  nms <- names(b)
  name_ageint  <- nms[grep("^age:female$", nms)]
  name_age2int <- nms[grep("^I\\(age\\^2\\):female$", nms)]
  
  beta1  <- b["age"]
  beta2  <- b["I(age^2)"]
  delta1 <- if(length(name_ageint))  b[name_ageint]  else 0
  delta2 <- if(length(name_age2int)) b[name_age2int] else 0
  
  peak_m <- - beta1 / (2 * beta2)
  peak_f <- - (beta1 + delta1) / (2 * (beta2 + delta2))
  
  return(c(peak_m, peak_f))
}

set.seed(123)

boot_gender_peaks <- boot(data = final_db, statistic = age_peak_gender, R = 1000)

mean_peaks <- colMeans(boot_gender_peaks$t)
mean_peaks
names(mean_peaks) <- c("Male", "Female")

male_ci   <- boot.ci(boot_gender_peaks, type = "perc", index = 1)$percent[4:5]
female_ci <- boot.ci(boot_gender_peaks, type = "perc", index = 2)$percent[4:5]

ggplot(pred_grid, aes(x = age, y = pred, color = Genero)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr, fill = Genero), alpha = 0.2, color = NA) +
  geom_line(size = 1) +
  geom_vline(xintercept = mean_peaks["Male"], color = "blue", linetype = "dashed", size = 0.9) +
  geom_vline(xintercept = male_ci, color = "blue", linetype = "dotted", size = 0.8) +
  geom_vline(xintercept = mean_peaks["Female"], color = "red", linetype = "dashed", size = 0.9) +
  geom_vline(xintercept = female_ci, color = "red", linetype = "dotted", size = 0.8) +
  labs(
    title = "Perfil salarial estimado en función de la edad",
    subtitle = paste0(
      "Pico hombres: ~", round(mean_peaks["Male"], 1), " años (IC95%: ",
      round(male_ci[1], 1), " – ", round(male_ci[2], 1), ")\n",
      "Pico mujeres: ~", round(mean_peaks["Female"], 1), " años (IC95%: ",
      round(female_ci[1], 1), " – ", round(female_ci[2], 1), ")"
    ),
    x = "Edad",
    y = "Salario por hora esperado (COP)",
    color = "Género",
    fill  = "Género"
  ) +
  scale_color_manual(values = c("Hombres" = "dodgerblue4", "Mujeres" = "firebrick")) +
  scale_fill_manual(values  = c("Hombres" = "dodgerblue4", "Mujeres" = "firebrick")) +
  theme_minimal()