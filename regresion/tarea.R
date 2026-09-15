









mazda<-readxl::read_xlsx(path = "regresion/mazda2021.xlsx")

model<- lm(data= mazda, precio ~ km)
summary(model)

model$residuals
 
var_model <- var(model$residuals)

sigmasqrd_est<-sum(model$residuals^2)/(length(model$residuals))

var_model-sigmasqrd_est







diffs <- numeric(1000)

for (i in 1:1000) {
  # Crear dataset con dos variables aleatorias
  set.seed(i)
  datos_sim <- data.frame(
    x = rnorm(nrow(mazda)),
    y = rnorm(nrow(mazda))
  )
  
  # Crear modelo lineal
  modelo_sim <- lm(y ~ x, data = datos_sim)
  
  # Calcular varianza estimada (sigma al cuadrado)
  var_estimada_sesgada <- sum(modelo_sim$residuals^2) / (length(modelo_sim$residuals) )
  
  # Varianza real
  var_real <- var(datos_sim$y)
  
  # Guardar la diferencia
  diffs[i] <- var_estimada_sesgada - var_real
}
sesgo<-length(datos_sim$y)-2/length(datos_sim$y)
mean(diffs)
