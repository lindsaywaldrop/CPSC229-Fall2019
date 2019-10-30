# Automation Example to be covered in class 10/31/19
#
# In this study, I have modeled bird wings from 20 species of birds and 
# saved the coefficients of drag (CD) and lift (CL) at a range of speeds
# and angle of attacks (aoa) in CSV files. Each folder has results from 
# a different bird. I've given you some starting code to start! 
# 

rm(list=ls())

extract_CLCD<-function(n,speed){
  data<-read.csv(paste("samplescripts/AutomationExample/rep",
                       n,"/new_Wing_Polar_Graph_0.csv",sep=""))
  data <- data[,colSums(is.na(data))<nrow(data)] # Removing columns with only NA's
  
  data$speeds<-seq(5,20,by=0.5)  # This creates a vector of air speeds from 5ms/ to 20m/s
  # These speeds correspond to the rows in the CSV 1-31.
  # This creates a vector of angle of attacks that correspond to each pair of columns
  aoa<-seq(0,5,by=1)
  CLCD<-rep(NA,(length(aoa)))
  for (i in 1:length(aoa)){
    CLCD[i]<-data[data$speed==speed,2*i]/data[data$speed==speed,2*i-1]
  }
  data.CLCD<-data.frame(aoa,CLCD)
  return(data.CLCD)
}

species=20
aoa.all<-rep(seq(0,5),species)
CLCD.all<-rep(NA,6*species)
species.all<-rep(NA,6*species)
for (i in 1:species){
  data.CLCD<-extract_CLCD(i,10)
  for(j in 1:nrow(data.CLCD)){
    CLCD.all[j+(i-1)*6]<-data.CLCD$CLCD[j]
    species.all[j+(i-1)*6]<-i
  }
}

final.data<-data.frame(as.factor(species.all),aoa.all,CLCD.all)
names(final.data)<-c("species","aoa","CLCD")
#Challenge: calculate the lift-to-drag ratios of all the birds at 10 m/s and plot CLCD against angle of attack. 

library(ggplot2)

ggplot(final.data,aes(aoa,CLCD,color=species))+geom_line()


