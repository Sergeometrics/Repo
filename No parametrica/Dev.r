









set.seed(10)
m1<-rnorm(40,10,2)
m2<-rnorm(37,10,2)
m3<-rnorm(42,11,5)
t.test(m1,m2,alternative = "two.sided", conflevel = 0.9)

#para  ANOVA
datos <- data.frame(valor = c(m1, m2, m3), grupo = factor(rep(c("m1", "m2", "m3"),  times = c(length(m1), length(m2), length(m3)))))


modelo <- aov(valor ~ grupo, data = datos)
summary(modelo)

#Comparacion 1 a 1
TukeyHSD(modelo)

#lsd
pairwise.t.test(datos$valor, datos$grupo, p.adjust.method = "none")
