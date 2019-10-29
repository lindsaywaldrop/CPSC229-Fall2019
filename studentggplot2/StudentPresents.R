library(ggplot2)

#### Sabrina, Marco, Chumi ####

# Histogram
AP<-as.data.frame(AirPassengers)
AP$x<-as.numeric(AP$x)

H<-ggplot(AP,aes(x=x))
H + geom_histogram(color="blue",bins=20)

#boxplots

data("InsectSprays")

B<-ggplot(InsectSprays,aes(spray,count))
B + geom_boxplot() + stat_summary(fun.y="mean",geom="point")

V<-ggplot(InsectSprays,aes(spray,count))
V + geom_violin()

#### Kady, Tiffany, Krista ####

# lineplots
library(ggfortify)

AP<-as.data.frame(AirPassengers)

autoplot(AirPassengers,
         main="Air Passengers")
ggfreqplot(AirPassengers)

# scatterplot
data("mtcars")

ggplot(mtcars,aes(wt,mpg))+geom_point()

#### Riya, Leo, Zaira ####
data("diamonds")

basic_graph<-ggplot(diamonds,aes(x=cut))

basic_graph+geom_bar(fill="red",color="black",width=0.75)


