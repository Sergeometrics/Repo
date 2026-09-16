









mazda<-readxl::read_xlsx(path = "regresion/mazda2021.xlsx")

model<- lm(data= mazda, precio ~ km)
summary(model)

model$residuals
 
var_model <- var(model$residuals)

sigmasqrd_est<-sum(model$residuals^2)/(length(model$residuals))

var_model-sigmasqrd_est

n <- 100

# Varianza true
sigma2_true <- sum(model$residuals^2) / (length(model$residuals) - 2)

set.seed(2026)

diffs <- numeric(1000)

for (i in 1:1000) {
  set.seed(i)
  idx <- sample(seq_len(nrow(mazda)), size = n, replace = FALSE)
  muestra <- mazda[idx, c("precio", "km")]

  modelo_sim <- lm(precio ~ km, data = muestra)

  # Estimador sesgado:
  var_sesgada <- sum(residuals(modelo_sim)^2) / n

  # Guardar la diferencia entre estimador sesgado y  true
  diffs[i] <- var_sesgada - sigma2_true
}

sesgo <- mean(diffs)

# Sesgo teorico para comparación 
sesgo_teorico <- - (2 / n) * sigma2_true
diffs
c(sesgo= sesgo , "sesgo teorico"= sesgo_teorico)
