library(ggplot2)
data("mtcars")
ggplot(mtcars, aes(x=wt, y=mpg)) + geom_point()

mtcars$cyl <- as.factor(mtcars$cyl)
p <-ggplot(mtcars, aes(x=wt, y=mpg, color=cyl, shape=cyl)) +
  geom_point() + 
  geom_smooth(method=lm, se=FALSE, fullrange=TRUE)
p+labs(title = "Relation between Weight and Miles per gallon", x= "Weight", y= "Miles per  gallon")

plot(mtcars)
data("mtcars")
cars <-data("mtcars")
plot(mtcars$wt,mtcars$mpg, main= "Weight vs. Miles per  gallon",
     xlab= "weight",ylab="Miles per gallon")
abline(lm(mpg ~wt, data=mtcars), col= 'red')


