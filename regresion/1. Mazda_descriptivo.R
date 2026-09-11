library(readxl)
mazda2021 <- read_excel("~/Library/CloudStorage/Dropbox/0. UNAL/Actividades realizadas/Clases/UNAL 2026-01/Análisis de regresión_2016360/datos/mazda2021.xlsx")

library(ggplot2)
ggplot(data = mazda2021, aes(x = km, y = precio)) + 
  geom_point()

cor(mazda2021$km, mazda2021$precio)

library(dplyr)
ggplot(data = mazda2021 %>% filter(modelo != "Mazda 2"), aes(x = km, y = precio)) + 
  geom_point() + 
  geom_smooth()

ggplot(data = mazda2021, aes(x = km, y = precio)) + 
  geom_point() + 
  geom_smooth(method = "lm")

ggplot(data = mazda2021, aes(x = año, y = precio)) + 
  geom_point() 

cor(mazda2021$precio, mazda2021$km)
cor(mazda2021$precio, mazda2021$año)

ggplot(data = mazda2021, aes(x = modelo, y = precio)) + 
  geom_boxplot() 

table(mazda2021$combu)
library(ggridges)
ggplot(data = mazda2021, aes(x = precio, y = combu, fill = combu)) + 
  geom_density_ridges() 

ggplot(data = mazda2021,aes(x = km, y = precio)) + 
  geom_point()

ggplot(data = mazda2021,aes(x = km, y = precio, col = modelo)) + 
  geom_point()

ggplot(data = mazda2021,aes(x = km, y = precio, col = modelo, shape = trans)) + 
  geom_point()

ggplot(data = mazda2021,aes(x = km, y = precio, col = modelo, shape = trans, size = año)) + 
  geom_point(alpha = 0.7)

##
lm(precio ~ km, data = mazda2021)

lm(precio ~ km, data = mazda2021, subset = (modelo == "Mazda 2"))
lm(precio ~ km, data = mazda2021, subset = (modelo == "Mazda 3"))
lm(precio ~ km, data = mazda2021, subset = (modelo == "Mazda 6"))
