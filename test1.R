#test 1
carros<- mtcars
head(mtcars)
names(mtcars)
mtcars$mpg
plot(mtcars$mpg,mtcars$cyl)
library(esquisse)
esquisser()
ggplot(carros) +
  aes(x = cyl, y = hp, colour = vs, size = mpg, group = am) +
  geom_point() +
  scale_color_gradient() +
  theme_minimal()
library(plotly)
