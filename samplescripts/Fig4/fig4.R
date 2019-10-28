#Clears workspace
rm(list=ls())

library(grid) #Required library
library(jpeg) #Required library if your pic is a jpeg
library(png)  #Required library if your pic is a png
library(MASS) #I think this is required, but can't remember

# Sets working directory
setwd("/Users/lwaldrop/Dropbox/dissertation/Ch1/publication/stats")

# Displays the current working directory.
getwd()

# Loads a CSV file into the workspace that will be used for plotting.
d <- read.csv("blue.csv")
summary(d)

setwd("/Users/lwaldrop/Dropbox/dissertation/Ch1/publication/BiolBulletin/Figures/Fig4")

image1<-readPNG("Fig4_FC.png")	#Reads image in 
image2<-readJPEG("Fig4_FS.jpg")	#Reads image in 
image3<-readJPEG("Fig4_BC.jpg") #Reads image in 
image4<-readJPEG("Fig4_BS.jpg") #Reads image in

# Establish d$Plot variable. This is only for my plot, to regroup things.
for(i in 1:72){
	if( d$Model[i]=="C" && d$Orient[i]=="B" ){
	d$Plot[i]<-3
	} else {
	}

	if( d$Model[i]=="C" && d$Orient[i]=="F" ){
	d$Plot[i]<-1
	} else {
	}
	if( d$Model[i]=="S" && d$Orient[i]=="B" ){
	d$Plot[i]<-4
	} else {
	}
	if( d$Model[i]=="S" && d$Orient[i]=="F" ){
	d$Plot[i]<-2
	} else {
	}
}

#This defines a new function which will draw your image and some text alongside it. Right now, I'm not using text, but the option is still there.
imgflick.draw <- function(label, image, x, y, size, ...) {
  logo <- rasterGrob(image = image,
    x = unit(x, "npc"), y = unit(y, "npc"),
    width = unit(size, "cm"), height = unit(size, "cm"),
    just = c("left","centre"), gp = gpar(...))
  lab <- textGrob(label = label,
    x = unit(x, "npc"), y = unit(y, "npc") - unit(0.6, "grobwidth", logo),
    just = c("left", "centre"), gp = gpar(...))
  grid.draw(logo)
  grid.draw(lab)
}

y_mean<-array(1,c(6,4))
y_std<-array(1,c(6,4))
Re<-c(0.23,0.57,0.94,1.59,1.83,2.82)
#Calculates means and stdev's.
for(i in 1:6){
	m1<-mean(d$Speed[d$Re==Re[i]&d$Orient=="F"&d$Model=="C"])
	sd1<-sd(d$Speed[d$Re==Re[i]&d$Orient=="F"&d$Model=="C"])
	m2<-mean(d$Speed[d$Re==Re[i]&d$Orient=="F"&d$Model=="S"])
	sd2<-sd(d$Speed[d$Re==Re[i]&d$Orient=="F"&d$Model=="S"])
	m3<-mean(d$Speed[d$Re==Re[i]&d$Orient=="B"&d$Model=="C"])
	sd3<-sd(d$Speed[d$Re==Re[i]&d$Orient=="B"&d$Model=="C"])
	m4<-mean(d$Speed[d$Re==Re[i]&d$Orient=="B"&d$Model=="S"])
	sd4<-sd(d$Speed[d$Re==Re[i]&d$Orient=="B"&d$Model=="S"])
	y_mean[i,]<-c(m1,m2,m3,m4)
	y_std[i,]<-c(sd1,sd2,sd3,sd4)
	}

x<-c(1,2,3,4)

lab1 = expression(paste("Mean water speed in dendrite-\ncontaining area of array,   cm ",s^-1))

#### Mean with Confidence Intervals. #####

#setEPS()   #EPS seems to not work well with the images, they come out a bit grainy. I have been using PDF instead, which looks fine.
#postscript("Fig4_R.eps",width=3.3*2,height=2.7*2) #For saving as EPS.
pdf("Fig4_R.pdf",width=3.3*2,height=2.7*2)  #For saving as PDF

par(mfrow=c(1,1), mar=c(3,5.5,1,1), oma=c(3,1,1,1)) #Bottom, Left, Top, Right. 
plot(x,y_mean[4,],ylim=c(0,4),pch=2,xlim=c(0.5,4.5),cex=1.5,
xlab=list(" "),
ylab=list(lab1,cex=1.2),xaxt="n") #xaxt ="n" turns off markings on x-axis. The numbers here are arbitrary, and I want to replace them with pictures.
Axis(side=1, labels=FALSE,tck=-0.01, at=c(1,2,3,4)) #Redraws axis with no labels but ticks at 1,2,3, and 4.
imgflick.draw(" ", image1, 0.25, 0.125, 0.5, size=1.75, fontsize = 8) #Uses new function to plot image and text. Text goes between " ", image name, x coordinate, y coordinate, etc.
imgflick.draw(" ", image2, 0.43, 0.125, 0.5, size=1.75, fontsize = 8)
imgflick.draw(" ", image3, 0.605, 0.125, 0.5, size=1.75, fontsize = 8)
imgflick.draw(" ", image4, 0.78, 0.125, 0.5, size=1.75, fontsize = 8)
#Plotting some other things.
lines(x,y_mean[3,],type="o",lty=0,pch=15,cex=1.5)  
lines(x,y_mean[2,],type="o",lty=0,pch=16,cex=1.5)
lines(x,y_mean[1,],type="o",lty=0,pch=17,cex=1.5)
lines(x,y_mean[5,],type="o",lty=0,pch=1,cex=1.5)
lines(x,y_mean[6,],type="o",lty=0,pch=0,cex=1.5)
#Adding error bars with length stdev to plots.
arrows(x, y_mean[1,]-y_std[1,], x, y_mean[1,]+y_std[1,], length=0.05, angle=90, code=3) 
arrows(x, y_mean[2,]-y_std[2,], x, y_mean[2,]+y_std[2,], length=0.05, angle=90, code=3)
arrows(x, y_mean[3,]-y_std[3,], x, y_mean[3,]+y_std[3,], length=0.05, angle=90, code=3)
arrows(x, y_mean[4,]-y_std[4,], x, y_mean[4,]+y_std[4,], length=0.05, angle=90, code=3)
arrows(x, y_mean[5,]-y_std[5,], x, y_mean[5,]+y_std[5,], length=0.05, angle=90, code=3)
arrows(x, y_mean[6,]-y_std[6,], x, y_mean[6,]+y_std[6,], length=0.05, angle=90, code=3)
#Legend.
legend("topright",legend=c("Re=2.8","Re=1.8","Re=1.6","Re=0.94","Re=0.57","Re=0.23"),
pch=c(0,1,2,15,16,17),lty=c(0,0,0,0,0,0))
#Alternative x-axis label.
mtext("Aesthetasc Splay and Direction of Movement", line=-24.5)

dev.off()