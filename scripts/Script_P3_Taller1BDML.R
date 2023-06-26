
### Punto 3 ###


modelo_punto_3 <- lm(log_salario_hora_imputado~edad + edad2, data = GEIH_def)

stargazer(modelo_punto_3,type="text",title="Resultado de la Regresión Salario-Edad", keep=c("Intercept", "edad","edad2"), 
          dep.var.labels="Log(salario_hora)",covariate.labels=c("Edad","Edad2"),omit.stat=c("ser","f","adj.rsq"))

stargazer(modelo_punto_3)

### Corro lo de Jaz


reg_w_age<-lm(formula=log_salario_hora_imputado~edad+edad2, data=GEIH) #modelo general

coefs_w_age<-reg_w_age$coef
b1_w_age<-coefs_w_age[1]
b2_w_age<-coefs_w_age[2]
b3_w_age<-coefs_w_age[3]


edad_max<- (-b2_w_age/(2*b3_w_age))

summary(modelo_punto_3)

peak_age <- -0.0203207/2*(0.0001771)

peak_age2 <- -0.0203207/(2*(0.0001771))

set.seed(12345)
model_wage_age_fn<- function(data, index) {
  f<- lm(formula=log_salario_hora_imputado~edad+edad2, data, subset=index)
  
  coefs<-f$coefficients
  b2<-coefs[2]
  b3<-coefs[3]
  
  edad_max_bt<-(-b2/(2*b3))
  return(edad_max_bt)
}

model_wage_age_fn(GEIH,1:nrow(GEIH)) #para verificar que nos de el mismo peak age en el modelo general

set.seed(12345) #para que sea reproducible
p_load(boot)
err_est_wage_age<-boot(GEIH,model_wage_age_fn,R=1000)
plot(err_est_wage_age)

err_est_wage_age
model_wage_age_fn(GEIH,1:nrow(GEIH)) #para verificar que nos de el mismo peak age en el modelo general

se<- apply(err_est_wage_age$t,2,sd)[1] #grabamos el valor del error estándar en el objeto se


# Confidence Intervals ----------------------------------------------------

#Intervalos de confianza
#Cálculos de intervalos de confianza
conf_int<-boot.ci(boot.out=err_est_wage_age, type=c("norm"), conf=0.95) #cálculo de los intervalos de confianza boot
conf_int

#Extraemos los valores inferiores y superiores del intervalo de confianza
ic_sup<-conf_int$normal[3]
ic_inf<-conf_int$normal[2]
ic_sup
ic_inf

conf_int
