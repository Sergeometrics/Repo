library(readxl)
mazda2021 <- read_excel("~/Dropbox/0. UNAL/Actividades realizadas/Clases/UNAL 2026-02/Análisis de regresión/datos/mazda2021.xlsx")

library(ggplot2)

library(dplyr)

ggplot(data = mazda2021, aes(x = trans, y = precio)) + 
  geom_boxplot() 

library(ggridges)
ggplot(data = mazda2021, aes(x = precio, y = trans)) + 
  geom_density_ridges() 

mazda2021 %>%
  group_by(trans) %>%
  summarise(min(precio), mean(precio), median(precio), max(precio))

##
lm(precio ~ trans, data = mazda2021)

# Tomar la categoría base "transmisión manual"
mazda2021$trans <- relevel(
  factor(mazda2021$trans),
  ref = "Manual"
)

lm(precio ~ trans, data = mazda2021)

lm(precio ~ modelo, data = mazda2021)