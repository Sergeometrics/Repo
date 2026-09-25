##Configuración 
options(digits=3)
hoteles = c("HAA", "HАВ", "HAC", "HAD", "HAE")

choose(5, 3)
muestrasMAS = t(as.matrix(combn(hoteles, 3)))
dim(muestrasMAS)
muestrasMAS




probmuestra = rep(1/10, 10)2. Variables Indicadoras de PertenenciaI(muestrasMAS == "HAA")
IHAA = rowSums(I(muestrasMAS == "HAA"))
IHAA
IHAB = rowSums(I(muestrasMAS == "HAB"))
IHAC = rowSums(I(muestrasMAS == "HAC"))
IHAD = rowSums(I(muestrasMAS == "HAD"))
IHAE = rowSums(I(muestrasMAS == "HAE"))


muestrasMAS = data.frame(muestrasMAS, probmuestra, IHAA, IHAB, IHAC, IHAD, IHAE)
muestrasMAS
muestrasMAS$ns = rowSums(muestrasMAS[,5:9])
muestrasMAS3. Probabilidades de Inclusión (1er y 2do orden)attach(muestrasMAS)
piHAA = sum(IHAA * probmuestra)
piHAA
mean(IHAA)
0.6 * 0.4
var(IHAA) * (9/10)
piHAAHAB = sum(IHAA * IHAB * probmuestra)
piHAAHAB
mean(IHAA * IHAB)
mean(ns)
var(ns)
0.6 * 5

pikl = matrix(0.3, 5, 5)
diag(pikl) = rep(0.6, 5)
pikl
sum(diag(pikl))
sum(pikl)
delta = pikl - (0.6 * 0.6)
delta


sum(delta)4. Alternativa: Muestreo Aleatorio Simple CON reemplazoexpand.grid(rep(list(hoteles), 3))5. Estadísticos y Estimadoresventas = c(150, 255, 300, 345, 90)
personal = c(20, 30, 32, 50, 20)
mean(ventas)
mean(personal)
attach(muestrasMAS)
muestrasMAS$estim1 = (IHAA*ventas[1] + IHAB*ventas[2] + IHAC*ventas[3] + IHAD*ventas[4] + IHAE*ventas[5]) / ns
muestrasMAS$estim2 = (IHAApersonal[1] + IHABpersonal[2] + IHACpersonal[3] + IHADpersonal[4] + IHAE*personal[5]) / ns
muestrasMAS

mean(estim1)
mean(estim2)
muestrasMAS$probm2 = c(6/90, 7/90, 8/90, 8/90, 9/90, 10/90, 9/90, 10/90, 11/90, 12/90)
sum(muestrasMAS$probm2)
sum(estim1 * probm2)
sum(estim2 * probm2)
var(estim1)

eeEstim1 = sqrt(var(estim1))
sesgoEstim1 = mean(estim1) - mean(ventas)
ECMEstim1 = var(estim1) + (sesgoEstim1^2)
CVEstim1 = eeEstim1 / mean(estim1)
eeEstim1
sesgoEstim1
ECMEstim1
CVEstim1

muestrasMAS$estim12 = estim1^2
muestrasMAS$estim22 = estim2^2
attach(muestrasMAS)
varEstim1 = sum(estim12 * probm2) - (sum(estim1 * probm2))^2
varEstim2 = sum(estim22 * probm2) - (sum(estim2 * probm2))^2
varEstim1
varEstim2


eeEstimid = sqrt(varEstim1)
sesgoEstim1d = sum(estim1 * probm2) - mean(ventas)
ECMEstim1d = varEstim1 + (sesgoEstim1d^2)
CVEstimid = eeEstimid / sum(estim1 * probm2)
deffEstim1d = varEstim1 / var(estim1)


eeEstimid
sesgoEstim1d
ECMEstim1d
CVEstimid
deffEstim1d