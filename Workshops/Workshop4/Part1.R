# Workshop 4, Part 1 (In class exercise)
#
# For Part 1, you will need to debug the following code so that it works to produce a plot. 
# The purpose of this exercise is to assess your understanding of the control features that 
# we learned in class in practice. The code is complicated, but no single element is 
# something that you haven't already seen! Remember, googling the error message helps a lot, 
# as does isolating each line of code within a function or loop. 
# 
# There are four errors in the code that prevent it from working properly. Correct each 
# error in the script and make a comment at the line with information about what you did
# to correct the error. Submit your correct R script as some version of "YourName-Part1.R".

rm(list=ls())

library(ggplot2)
library(cowplot)

location<-"~/Dropbox (Chapman)/courses/CPSC229/CPSC229-Fall2019/Workshops/Workshop4/"
setwd(paste(location,"set1",sep=""))

run<-c(3116,3161,3163,3118)

for(i in 1:length(run)){
  name<-paste('run',run[i],'_hermithair.csv',sep="")
  data<-read.csv(name,header=FALSE)
  assign(paste('run,run[i],sep=''),data)'
}


run3116cols<-run3116$V3/max(run3116$V3)
run3161cols<-run3161$V3/max(run3161$V3)
run3163cols<-run3163$V3/max(run3163$V3)
run3118cols<-run3118$V3/max(run3118$V3)

theme_set(theme_bw())
p1<-qplot(x=run3116$V1,y=run3116$V2,color=run3116cols,xlab="X Position",ylab="Y Position")+theme(legend.position='none') +scale_colour_gradient(high="yellow",low="blue")
p2<-qplot(x=run3161$V1,y=run3161$V2,color=run3161cols,xlab="X Position",ylab="Y Position")+theme(legend.position='none') +scale_colour_gradient(high="yellow",low="blue")
p3<-qplot(x=run3163$V1,y=run3163$V2,color=run3163cols,xlab="X Position",ylab="Y Position")+theme(legend.position='none'))+scale_colour_gradient(high="yellow",low="blue")
p4<-qplot(x=run3118$V1,y=run3118$V2,color=run3118cols,xlab="X Position",ylab="Y Position")+theme(legend.position='none') +scale_colour_gradient(high="yellow",low="blue")



pdf("hairs_hermit.eps",width=7*0.85,height=6.5*0.85) # For an PDF.

plot_grid(p1,p2,p3,p4,labels=c("A","B","C","D"),label_size=12)

dev.off()

