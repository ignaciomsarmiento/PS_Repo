
#####
#4. Age-wage profile 

#a. Linear Model

lm_age<- lm(log(ingreso_hora) ~ age + I(age^2), data = final_db)
robust_se1 <- sqrt(diag(vcovHC(lm_age, type = "HC0")))

stargazer(lm_age,
          type = "html",
          se = list(robust_se1),
          dep.var.labels = "log(Salario por hora)",
          out = "Age-wage profile.doc")

#b. Bootstrap for finding the max point 

age_peak <- function(data, indices) {
  d <- data[indices, ]
  lmage <- lm(log(ingreso_hora) ~ age + I(age^2), data = d)
  b <- coef(lmage)
  return( -b["age"] / (2 * b["I(age^2)"]) ) #calculo del pico de edad (derivada)
}

set.seed(123)

boot_ <- boot(data = final_db, statistic = age_peak, R = 1000)
boot_
mean_peak <- mean(boot_$t) #bootstrap mean
mean_peak

#c. Confidence intervals

peak_ci2 <- boot.ci(boot_, type = "perc")$percent[4:5]
peak_ci2


#d. Estimate w/ confidence intervals

pred_ic <- final_db %>%
  select(age) %>%
  distinct() %>%
  arrange(age)

boot_pred <- function(data, indices) {
  d <- data[indices, ]
  lmage <- lm(log(ingreso_hora) ~ age + I(age^2), data = d)
  pred <- predict(lmage, newdata = pred_ic)
  return(pred)
}

set.seed(123)

boot_curve <- boot(data = final_db, statistic = boot_pred, R = 1000)

pred_ic$pred <- exp(predict(lm_age, newdata = pred_ic)) #OLS curve
pred_ic$lwr  <- apply(exp(boot_curve$t), 2, quantile, 0.025)
pred_ic$upr  <- apply(exp(boot_curve$t), 2, quantile, 0.975)

#e. Graph 

ggplot(pred_ic, aes(x = age, y = pred)) +
  geom_ribbon(aes(ymin = lwr, ymax = upr), fill = "grey40", alpha = 0.3) +
  geom_line(color = "dodgerblue4", size = 1) +
  geom_vline(xintercept = mean_peak, color = "darkred", linetype = "dashed", size = 0.9) +
  geom_vline(xintercept = peak_ci2, color = "green4", linetype = "dotted", size = 0.8) +
  labs(
    title = "Perfil salarial estimado en función de la edad",
    subtitle = paste0(
      "Pico de salario a los ~", round(mean_peak, 1),
      " años (IC 95%: ", round(peak_ci2, 1), " - ", round(peak_ci2, 1), ")"
    ),
    x = "Edad",
    y = "Salario por hora esperado (unidades en COP)") +
  theme_minimal()

