
p<-ggplot(mtcars,aes(hp,mpg))

# Aesthetic properties
p + geom_point(color="red") #change color of points
p + geom_point(shape="triangle") #change shape of points
p + geom_point(size=4) # change size of points
p + geom_point(shape=21,color="black",fill="white",size=4) #large, white-inside, black outlined circles

p <- ggplot(mtcars, aes(hp,mpg,fill=cyl))
p + geom_point(aes(color=cyl))
p + geom_point(aes(color=cyl),alpha=5/10)

ggplot(mtcars,aes(hp,mpg)) + geom_point(color=mtcars$cyl)


# Mapping and guides
# scale_*_continuous() or scale_*_discrete()  stars = x, y
p + geom_point(aes(color=cyl)) + ylim(0,40) + xlim(0,400)
p + geom_point(aes(color=cyl)) + scale_y_continuous(limits=c(0,40),breaks=c(0,10,15,30,40)) # specify label locations
p + geom_point(aes(color=cyl)) + scale_y_continuous(limits=c(0,40),breaks=c(10,20,30),labels=c("OK","Good","Great")) # specify label locations and text 

#Create multi-panel plots

p + geom_point() + facet_wrap(~cyl) #Makes a multi-panel plot that divides the x,y data by cyl. 
