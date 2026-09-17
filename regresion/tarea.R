#Espacios en blanco porque la pantalla de mi laptop esta dañada en esta parte JAJAJAJ









#Datos 
mazda<-readxl::read_xlsx(path = "regresion/mazda2021.xlsx")


#Modelo y varianza poblacional 
model<- lm(data= mazda, precio ~ km)
summary(model) 

 #varianza poblacional (just flexing atp)
sigmasqrd_est<-sum(model$residuals^2)/(length(model$residuals))



# Varianza true
sigma2_true <- sum(model$residuals^2) / (length(model$residuals) - 2)



diffs <- numeric(1000) #iniciar variables o sino R llora (Y yo también)
var_sesgada <- numeric(1000)
sigma2_insesgada <- numeric(1000)

for (i in 1:1000) {
  set.seed(i)
  idx <- sample(seq_len(nrow(mazda)), size = 100, replace = FALSE)
  muestra <- mazda[idx, c("precio", "km")]

  modelo_sim <- lm(precio ~ km, data = muestra)

  # Estimador sesgado:
  var_sesgada[i] <- sum(residuals(modelo_sim)^2) / 100


# Varianza de la muestra insesgada 
sigma2_insesgada[i] <- sum(modelo_sim$residuals^2) / (length(modelo_sim$residuals) - 2)


  # Guardar la diferencia entre estimador sesgado y  true
  diffs[i] <- var_sesgada[i] - sigma2_insesgada[i]

}

sesgo_prom <- mean(diffs)

# Sesgo teorico para comparación 
sesgo_teorico <- - (2 / 100) * sigma2_true
diffs
c(sesgo= sesgo_prom , "sesgo teorico"= sesgo_teorico)

#  pa ver si si se parecen :3 
sesgo_prom / sesgo_teorico 
