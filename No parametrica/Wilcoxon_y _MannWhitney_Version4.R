

library(nortest)

#--------------------------------------------
# 1. Representación gráfica de las hipótesis
#    Test de Wilcoxon
#--------------------------------------------

# Hipótesis 
# F_X(x)=F_Y(x)
# F_X(x) \neq F_Y(x)

#--------------------------------------------------------
# 1.1 X distribución Normal y normal desplazada por delta
#--------------------------------------------------------

# Función de densidad

par(mfrow=c(1,2))
mu=15
sigma=3
delta =2
t=seq(mu-4*sigma, mu+4*sigma, length=1000)
plot(t, dnorm(t, mu, sigma), col=2, type="l", ylab="Densidad Normal", main=expression(X%~%Normal(15,3)))
abline(v=15, lty=2)
abline(v=17, lty=2)
lines(t, dnorm(t-delta, mu, sigma), col=4, type="l")
legend(3, 0.12,  bty="n", legend=c(expression(f[X](13)), expression(f[Y](15)==f[X](13))),      col=c(2, 4), cex=0.8)
points(15, dnorm(13, mu, sigma), pch=16, col=4)  # f_y(15)=f_x(13)
points(13, dnorm(13, mu, sigma), pch=16, col=2)
legend(8.5, 0.02,  bty="n", legend=c(expression(f[X](t)), expression(f[Y](t)==f[X](t-Delta))), lty=1, col=c(2, 4), cex=0.7)

dnorm(15,15,3)
dnorm(12, 12, 3)

# -----------------------
# Función de distribución
# -----------------------

mu=15
sigma=3
t=seq(mu-4*sigma, mu+4*sigma, length=1000)
plot(t, pnorm(t, mu, sigma), col=2, type="l", ylab="Densidad Normal", main=expression(X%~%Normal(15,3)))
delta=2
lines(t, pnorm(t-delta, mu, sigma), col=4, type="l")
legend("topleft",  bty="n", legend=c(expression(F[X](t)), expression(F[Y](t)==F[X](t-Delta))), lty=1, col=c(2, 4), cex=0.8)
points(15, pnorm(13, mu, sigma), pch=16, col=4)  # F_y(15)=F_x(13)
points(13, pnorm(13, mu, sigma), pch=16, col=2)
legend(12, 0.3,  bty="n", legend=c(expression(F[X](13)), expression(F[Y](15)==F[X](13))),       col=c(2, 4), cex=0.8)


#--------------------------------
# 1.2 Sea X exponencial (lambda=3)
#---------------------------------

# --------------
# 1.2.1 Densidad
# --------------

par(mfrow=c(1,2))
lambda=3
delta = 0.1
t=seq(0, 2, length=1000)
plot(t, dexp(t,lambda), col=2, type="l", ylab="Densidad ", main=expression(X%~%Exp(lambda==3)))
t=seq(0.1, 1.9, length=1000)
densidad_despazada=lambda*exp(-lambda*(t-delta))
lines(t, densidad_despazada, col=4, type="l")
legend(0.3, 3,  bty="n", legend=c(expression(f[X](t)), expression(f[Y](t)==f[X](t-Delta))), lty=1, col=c(2, 4), cex=0.8)
points(0.3, dexp(0.3, lambda), pch=16, col=2)  # f_y(0.4)=f_x(0.4)
points(0.4, dexp(0.3, lambda), pch=16, col=4)  # f_y(0.4)=f_x(0.4)
legend(0.65, 2.5,  bty="n", legend=c(expression(f[X](0.3)), expression(f[Y](0.4)==f[X](0.3))), col=c(2, 4), cex=0.8)

# ------------------------------------------------
# Distribución exponencial y exponencial deplazada
# ------------------------------------------------

t=seq(0, 2, length=1000)
plot(t, pexp(t,lambda), ylim=c(0,1), col=2, type="l", ylab="Distribuci??n", main=expression(X%~%Exp(lambda==3)))
delta = 0.1
t=seq(0.1, 1.9, length=1000)
lines(t, pexp(t-delta, lambda), col=4, type="l")
legend(0.8, 0.8,  bty="n", legend=c(expression(F[X](t)), expression(F[Y](t)==F[X](t-Delta))), lty=1, col=c(2, 4), cex=0.8)
points(0.3, pexp(0.3, lambda), pch=16, col=2)  # f_y(0.4)=f_x(0.4)
points(0.4, pexp(0.3, lambda), pch=16, col=4)  # f_y(0.4)=f_x(0.4)
legend(1.15, 0.7,  bty="n", legend=c(expression(F[X](0.3)), expression(F[Y](0.4)==F[X](0.3))), col=c(2, 4), cex=0.8)

# -----------------------------------------------
# 1.3. Distribución  Weibull y Weibull desplazada
# -----------------------------------------------

?dweibull
alpha=2
beta =3
x=seq(0, 10, length=100)
plot(x, dweibull(x, shape=alpha, scale = beta), col=4, type="l", ylab="Densidad", main=expression(X%~%Weibull(alpha==2,beta==3)))

# X Weibull desplazada

theta=1
t=seq(1, 11, length=100)
lines(t, dweibull(t-theta, shape=alpha, scale = beta), type="l", col=2)

#----------------------
# Ejemplo de las cabras
#----------------------

# ----------------------------------------------------------------------
# Goats <- read.csv("C:/RAMON/MIS CARPETAS/CURSOS/I-2020/NO PARAMETRICA
#                     /Talleres R/5. Test de dos Muestras Independientes
#                     /Goats.csv")
# ----------------------------------------------------------------------

Goats<- read.table (file ="No parametrica/Goats.csv", header = TRUE, sep = ",")
names(Goats)       # Leyendo en R Studio


#-------------------------
# Prueba t de dos muestras
#-------------------------

Tratamiento1=Goats[1:15,3]
Tratamiento1=as.numeric(Tratamiento1)
Tratamiento2=Goats[16:30,3]
Tratamiento2=as.numeric(Tratamiento2)
t.test(Tratamiento1, Tratamiento2, alternative = "two.sided",  mu = 0, conf.level = 0.95, var.equal=TRUE)
2*pt(-0.40203,28) 
pt(-0.40203,28) + (1-pt(0.40203,28))

# -------------------------------
# Otra forma de hacer la prueba t
# -------------------------------


Goats.df <- read.table (file ="No parametrica/Goats.csv", header = TRUE, sep = ",")
t.test(Goats.df$Judgment ~ Goats.df$Treatment, alternative = "two.sided",  mu = 0, conf.level = 0.95, var.equal=TRUE )
names(Goats.df)
Goats.df$Treatment
Goats.df$Judgment

lillie.test(Tratamiento1)
lillie.test(Tratamiento2)

var.test(Goats.df$Judgment~Goats.df$Treatment)
bartlett.test(Goats.df$Judgment~Goats.df$Treatment)


# -------------------------------
# Prueba Wilcoxon de dos muestras  
# -------------------------------

?wilcox.test
wilcox.test(Goats.df$Judgment ~ Goats.df$Treatment, 
            alternative = "two.sided",mu = 0, conf.level = 0.95)

#---------------------------------------------------------------------------
# Prueba Mann-Whitney
# R hace el test de Mann Whitney pero no lo llama U sino W:
# Comprobar calculando los valores de W y U a mano
# R1 suma los rangos de la muestra 1 y R2=suma de los rangos de la muestra 2
# En la teorma presentamos (siguiendo el libro guía de Hollander y Wolfe)
# a la estadística de Wilcoxon 
# definida como la suma de los rangos de la muestra 2. 
#----------------------------------------------------------------------------

Respuesta=Goats.df$Judgment
Trat=Goats.df$Treatment
rango=rank(Respuesta)
datos=cbind(Respuesta, Trat, rango)
datos

W1=sum(rango[1:15])   # Nota: Suma los rangos del tratamiento uno, no los del tratamiento 2
W2=sum(rango[16:30])  # Nota: Suma los rangos del tratamiento uno, no los del tratamiento 2

n=15
m=15
U1=W1-((m*(m+1))/2)  # U=W1-R1, R1: Suma de los rangos de la muestra1
U2=W2-((n*(n+1))/2)  # U=W2-R2, R2: Suma de los rangos de la muestra2

U1
U2
wilcox.test(Goats.df$Judgment ~ Goats.df$Treatment, alternative = "two.sided",  mu = 0, conf.level = 0.95)


# U es igual a 98 que es el valor de la estadística de prueba W en la salida de R    
# El estadístico U se define como el mínimo de U1 y U2.         

 

#---------------------
# Ejemplo datos reales
#---------------------

# Ejercicio de clase: Cada estudiante debe buscar una base de datos
# y realizar el test de Wilcoxon



#------------------------------------
# Estudio de Potencia Bajo Normalidad
# Hipotesis alterna mu1-mu2<0
#------------------------------------

# ----------------------------------
# Parámetros para la Ha: mu_x < mu_y
# ----------------------------------

n_x=200                                  # Tamaño de Muestra de X
n_y=200                                  # Tamaño de Muestra de Y
mu_x=10                                  # Media de X
sigma_x=3                                # Desviación de Y
tmu_y=100                                # Número de medias bajo la alternativa
mu_y=seq(10, 13, length=tmu_y)           # Medias de Y
sigma_y=3                                # Sigma de Y
prob_rechazo_ttest=NULL
prob_rechazo_wilcoxon=NULL
# tsim=7000
tsim=1000

for (j in 1: tmu_y)
     {
      valorP_ttest=NULL
      valorP_wilcoxon=NULL
      for (i in 1:tsim)
          {
           x=rnorm(n_x, mu_x, sigma_x)
           y=rnorm(n_y, mu_y[j], sigma_y)
           ttest=t.test(x, y, alternative = "less", mu = 0, var.equal = TRUE, conf.level = 0.95)
           valorP_ttest[i]=ttest$p.value
           wtest=wilcox.test(x, y, alternative = "less", mu = 0, conf.level = 0.95)
           valorP_wilcoxon[i]=wtest$p.value
           }
       prob_rechazo_ttest[j]=sum(ifelse(valorP_ttest<0.05,1,0))/tsim
       prob_rechazo_wilcoxon[j]=sum(ifelse(valorP_wilcoxon<0.05,1,0))/tsim
     }

cbind(prob_rechazo_ttest, prob_rechazo_wilcoxon)
plot(mu_y[1:40], prob_rechazo_ttest[1:40], type="l", col=2, 
     ylab="Probabilidad de rechazo", xlab="Hipótesis Alterna")
lines (mu_y[1:40], prob_rechazo_wilcoxon[1:40], type="l", col=4, 
     ylab="Probabilidad de rechazo", xlab=expression(mu))
legend("topleft",  bty="n", legend=c("T-student", "Wilcoxon"), lty=1, col=c(2, 4), cex=0.8)



# ------------------------------------
# Estudio de Potencia Bajo Normalidad
# Ha: mu1-mu2>0
# ------------------------------------

# ----------------------------------
# Parámetros para la Ha: mu_x > mu_y
# ----------------------------------

n_x=200                                   # Tamaño de muestra de X
n_y=200                                   # Tamaño de muestra de Y

mu_x=10                                  # Media de X
sigma_x=3                                # Desviación de Y

tmu_y=100                                # Número de medias bajo la alternativa
mu_y=seq(8.5, 10, length=tmu_y)          # Medias de Y
sigma_y=3                                # Sigma de Y

prob_rechazo_ttest=NULL
prob_rechazo_wilcoxon=NULL
tsim=7000

for (j in 1: tmu_y)
     {
      valorP_ttest=NULL
      valorP_wilcoxon=NULL
      for (i in 1:tsim)
          {
           x=rnorm(n_x, mu_x, sigma_x)
           y=rnorm(n_y, mu_y[j], sigma_y)
           ttest=t.test(x, y, alternative = "greater", mu = 0, var.equal = TRUE, conf.level = 0.95)
           valorP_ttest[i]=ttest$p.value
           wtest=wilcox.test(x, y, alternative = "greater", mu = 0, conf.level = 0.95)
           valorP_wilcoxon[i]=wtest$p.value
           }
       prob_rechazo_ttest[j]=sum(ifelse(valorP_ttest<0.05,1,0))/tsim
       prob_rechazo_wilcoxon[j]=sum(ifelse(valorP_wilcoxon<0.05,1,0))/tsim
     }

plot(mu_y, prob_rechazo_ttest, type="l", col=2,
     ylab="Probabilidad de rechazo", xlab="Hipótesis Alterna",
     main="")
lines (mu_y, prob_rechazo_wilcoxon, type="l", col=4)
legend("topleft",  bty="n", legend=c("T-student", "Wilcoxon"), lty=1, col=c(2, 4), cex=0.8)



#------------------------------------
# Estudio de Potencia Bajo Normalidad
# mu1 \neq mu2 
#------------------------------------

# -----------------------------------------------
# Parámetros bajo la Ha: mu_x diferente de  mu_y
# -----------------------------------------------

n_x=200                                  # Tamaño de muestra de X
n_y=200                                  # Tamaño de muestra de Y

mu_x=10                                  # Media de X
sigma_x=3                                # Desviación de Y

tmu_y=100                                # Número de medias bajo la alternativa
mu_y=seq(8.5, 11.5, length=tmu_y)        # Medias de Y
sigma_y=3                                # Desviación de Y

prob_rechazo_ttest=NULL
prob_rechazo_wilcoxon=NULL
tsim=7000

for (j in 1: tmu_y)
     {
      valorP_ttest=NULL
      valorP_wilcoxon=NULL
      for (i in 1:tsim)
          {
           x=rnorm(n_x, mu_x, sigma_x)
           y=rnorm(n_y, mu_y[j], sigma_y)
           ttest=t.test(x, y, alternative = "two.sided", mu = 0, var.equal = TRUE, conf.level = 0.95)
           valorP_ttest[i]=ttest$p.value
           wtest=wilcox.test(x, y, alternative = "two.sided", mu = 0, conf.level = 0.95)
           valorP_wilcoxon[i]=wtest$p.value
           }
       prob_rechazo_ttest[j]=sum(ifelse(valorP_ttest<0.05,1,0))/tsim
       prob_rechazo_wilcoxon[j]=sum(ifelse(valorP_wilcoxon<0.05,1,0))/tsim
     }
plot(mu_y, prob_rechazo_ttest, type="l", col=2,
     ylab="Probabilidad de rechazo", xlab="Hipótesis Alterna")
lines (mu_y, prob_rechazo_wilcoxon, type="l", col=4) 
legend(9.5,1,  bty="n", legend=c("T-student", "Wilcoxon"), lty=1, col=c(2, 4), cex=0.8)


