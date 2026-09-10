# ------------------------------









# Estimación de betas
# ------------------------------
# ------------------------------
# Ejercicio 1
# ------------------------------
beta0 = 1
beta1 = 2
n = 100
e = rnorm(n, 0, 1)
x = runif(n, 1, 5)
y = beta0 + beta1*x +e

lm(y~x)$coeff[1]
lm(y~x)$coeff[2]

# ------------------------------
# Ejercicio 2, 1000 iteraciones con varianza fija
# ------------------------------
n.sim = 1000
n = 100
beta0 = 1
beta1 = 2
sigma <- 1
beta0.est <- beta1.est <- c()
for(i in 1:n.sim){
  e = rnorm(n, 0, sigma)
  x = runif(n, 1, 5)
  y = beta0 + beta1*x +e
  beta0.est[i] <- lm(y~x)$coeff[1]
  beta1.est[i] <- lm(y~x)$coeff[2]
}

plot(beta0.est)
abline(h=beta0, col = "red")
summary(beta0.est)

plot(beta1.est)
abline(h=beta1, col = "red")
summary(beta1.est)

hist(beta0.est)
abline(v=beta0, col = "red")

hist(beta1.est)
abline(v=beta1, col = "red")


# ------------------------------
# Ejercicio 3, estimación de beta en función de la varianza
# ------------------------------
sigma <- seq(20, 1, by = -0.1)
beta0.est <- beta1.est <- c()
for(i in 1:length(sigma)){
  e = rnorm(n, 0, sigma[i])
  x = runif(n, 1, 5)
  y = beta0 + beta1*x +e
  beta0.est[i] <- lm(y~x)$coeff[1]
  beta1.est[i] <- lm(y~x)$coeff[2]
}

plot(beta0.est)
abline(h=beta0, col = "red")

plot(beta1.est)
abline(h=beta1, col = "red")

# ------------------------------
# Ejercicio 4, estimación de beta en función del tamaño muestral
# ------------------------------
n <- seq(5, 500, by = 1)
sigma <- 2
beta0.est <- beta1.est <- c()
for(i in 1:length(n)){
  e = rnorm(n[i], 0, sigma)
  x = runif(n[i], 1, 5)
  y = beta0 + beta1*x +e
  beta0.est[i] <- lm(y~x)$coeff[1]
  beta1.est[i] <- lm(y~x)$coeff[2]
}

plot(beta0.est)
abline(h=beta0, col = "red")

plot(beta1.est)
abline(h=beta1, col = "red")

# ------------------------------
# Estimación de sigma2
# ------------------------------
# ------------------------------
# Ejercicio 1
# ------------------------------
beta0 = 1
beta1 = 2
sigma = 1
n = 100
e = rnorm(n, 0, sigma)
x = runif(n, 1, 5)
y = beta0 + beta1*x +e

summary(lm(y~x))$sigma

# ------------------------------
# Ejercicio 2, 1000 iteraciones con betas fijos
# ------------------------------
n.sim = 1000
beta0 = 1
beta1 = 2
sigma = 2
sigma.est <- c()
for(i in 1:n.sim){
  e = rnorm(n, 0, sigma)
  x = runif(n, 1, 5)
  y = beta0 + beta1*x +e
  sigma.est[i] <- summary(lm(y~x))$sigma
}

plot(sigma.est)
abline(h=sigma, col = "red")
summary(sigma.est)

# ------------------------------
# Ejercicio 3, estimación de sigma en función de beta
# ------------------------------
beta0 <- seq(1, 20, by = 0.1)
beta1 <- 2
sigma = 2
sigma.est <- c()
for(i in 1:length(beta0)){
  e = rnorm(n, 0, sigma)
  x = runif(n, 1, 5)
  y = beta0[i] + beta1*x +e
  sigma.est[i] <- summary(lm(y~x))$sigma
  }

plot(sigma.est)
abline(h=sigma, col = "red")

# ------------------------------
# Ejercicio 4, estimación de sigma en función del tamaño muestral
# ------------------------------
n <- seq(5, 1000, by = 1)
beta0 = 1
beta1 = 2
sigma <- 2
sigma.est <- c()
for(i in 1:length(n)){
  e = rnorm(n[i], 0, sigma)
  x = runif(n[i], 1, 5)
  y = beta0 + beta1*x +e
  sigma.est[i] <- summary(lm(y~x))$sigma
}

plot(sigma.est^2)
abline(h=sigma^2, col = "red")
