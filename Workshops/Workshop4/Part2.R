# Workshop 4, Part 2 (Out of class exercise)
#
# For Part 2, you'll have one week to take the following code and automate the sections labeled: 
# - Loads Set 1 Data (Lines 88-105)
# - Loads Set 2 Data (Lines 106-124)
# - Loads Set 3 Data (Lines 125-143)
# Each set should be identical, but you'll need to automate how you load the data, make 
# necessary calculations, and keep track of it so you can make the plot at the end. 
# Note: While you can add additional functions, I would not recommend altering the findMeans() function
# defined on line 19. 

#### Loading required packages ####
library(signal)
library(ggplot2)

rm(list=ls())

#### Functions to load ####
findMeans<-function(set1data.time,set1data.Cadj,set2data.time,set2data.Cadj,set3data.time,set3data.Cadj){
  if(length(set1data.Cadj)==length(set2data.Cadj) & length(set2data.Cadj)==length(set3data.Cadj)){		
    meanCadj<-matrix(0,length(set1data.time),1)
    SDCadj<-matrix(0,length(set1data.time),1)
    flux<-matrix(0,length(set1data.time),1)
    for (i in 1:length(set1data.time)){
      a<-c(set1data.Cadj[i],set2data.Cadj[i],set3data.Cadj[i])
      meanCadj[i]<-mean(a)
      SDCadj[i]<-sd(a)
    }
    for(i in 2:length(flux)){
      flux[i] = abs(meanCadj[i+1]-meanCadj[i])/abs(set1data.time[i+1]-set1data.time[i])
    }
    dataresult<-data.frame(set1data.time,set1data.Cadj,set2data.Cadj,set3data.Cadj,meanCadj,SDCadj,flux)
    names(dataresult)<-c("time","set1","set2","set3","mean","SD","flux")
    return(dataresult)
    
  } else{
    if(length(set1data.Cadj)<=length(set2data.Cadj) & length(set1data.Cadj)<=length(set3data.Cadj)){
      timeint<-set1data.time
    } else if (length(set2data.Cadj)<=length(set1data.Cadj) & length(set2data.Cadj)<=length(set3data.Cadj)) {
      timeint<-set2data.time
    } else if(length(set3data.Cadj)<=length(set1data.Cadj) & length(set3data.Cadj)<=length(set2data.Cadj)) {
      timeint<-set3data.time	
    } else {
      timeint<-set1data.time
    }
    set1dataint.Cadj<-interp1(set1data.time,set1data.Cadj,timeint,method=c("linear"),extrap=FALSE)
    set2dataint.Cadj<-interp1(set2data.time,set2data.Cadj,timeint,method=c("linear"),extrap=FALSE)
    set3dataint.Cadj<-interp1(set3data.time,set3data.Cadj,timeint,method=c("linear"),extrap=FALSE)
    
    meanCadj<-matrix(0,length(timeint),1)		
    SDCadj<-matrix(0,length(timeint),1)
    flux<-matrix(0,length(timeint),1)
    for (i in 1:length(timeint)){
      a<-c(set1dataint.Cadj[i],set2dataint.Cadj[i],set3dataint.Cadj[i])
      meanCadj[i]<-mean(a)
      SDCadj[i]<-sd(a)
    }
    for(i in 2:length(flux)){
      flux[i] = abs(meanCadj[i+1]-meanCadj[i])/abs(timeint[i+1]-timeint[i])
    }
    
    
    dataresult<-data.frame(timeint,set1dataint.Cadj,set2dataint.Cadj,set3dataint.Cadj,meanCadj,SDCadj,flux)
    names(dataresult)<-c("time","set1","set2","set3","mean","SD","flux")
    return(dataresult)
  }
}


#### Defines necessary parameters ####

n<-3

Dair<-6.02e-6
Dwater<-7.84e-10

# Values of Cinf for each case. 
cinf1.marinewater<-0.052328952493219  # Blue crab in water, Set 1
cinf1.hermitair<- 0.529629241195228	 # Terrestrial hermit crab in air, Set 1
cinf2.marinewater<-0.052328952493219  #Blue crab in water, Set 2
cinf2.hermitair<- 0.512951477711897 #Terrestrial crab in air, Set 2
cinf3.marinewater<-0.053844453778029  #Blue crab in water, Set 3
cinf3.hermitair<- 0.526007400705351	 #Terrestrial crab in air, Set 3


location<-"~/Dropbox (Chapman)/courses/CPSC229/CPSC229-Fall2019/Workshops/Workshop4"

##### Loads Set 1 Data #####
setwd(paste(location,"/set1",sep=""))

# Loads data, Blue crab in water
marinewater1.perstepdata<-read.csv("pertimestep_3300.csv",header=FALSE)
summary(marinewater1.perstepdata)

# Some calculations
marinewater1.perstepdata$C<-marinewater1.perstepdata$V2/cinf1.marinewater # Creates C/Cinf for per time step data

# Loads data, Terrestrial crab in air
hermitair1.perstepdata<-read.csv("pertimestep_3116.csv",header=FALSE)
summary(hermitair1.perstepdata)

# Some calculations
hermitair1.perstepdata$C<-hermitair1.perstepdata$V2/cinf1.hermitair # Creates C/Cinf for per time step data


##### Loads Set 2 Data #####

setwd(paste(location,"/set2",sep=""))

# Loads data, Blue crab in water
marinewater2.perstepdata<-read.csv("pertimestep_3301.csv",header=FALSE)
summary(marinewater2.perstepdata)

# Some calculations
marinewater2.perstepdata$C<-marinewater2.perstepdata$V2/cinf2.marinewater # Creates C/Cinf for per time step data

# Loads data, Terrestrial crab in air
hermitair2.perstepdata<-read.csv("pertimestep_3167.csv",header=FALSE)
summary(hermitair2.perstepdata)

# Some calculations
hermitair2.perstepdata$C<-hermitair2.perstepdata$V2/cinf2.hermitair # Creates C/Cinf for per time step data


##### Loads Set 3 Data #####

setwd(paste(location,"/set3",sep=""))

# Loads data, Blue crab in water
marinewater3.perstepdata<-read.csv("pertimestep_3302.csv",header=FALSE)
summary(marinewater3.perstepdata)

# Some calculations
marinewater3.perstepdata$C<-marinewater3.perstepdata$V2/cinf3.marinewater # Creates C/Cinf for per time step data

# Loads data, Terrestrial crab in air
hermitair3.perstepdata<-read.csv("pertimestep_3175.csv",header=FALSE)
summary(hermitair3.perstepdata)

# Some calculations
hermitair3.perstepdata$C<-hermitair3.perstepdata$V2/cinf3.hermitair # Creates C/Cinf for per time step data


#### Find means of data sets 1-3 ####

marinewater<-findMeans(marinewater1.perstepdata$V1,
                       marinewater1.perstepdata$C,
                       marinewater2.perstepdata$V1,
                       marinewater2.perstepdata$C,
                       marinewater3.perstepdata$V1,
                       marinewater3.perstepdata$C)

hermitair<-findMeans(hermitair1.perstepdata$V1,
                     hermitair1.perstepdata$C,
                     hermitair2.perstepdata$V1,
                     hermitair2.perstepdata$C,
                     hermitair3.perstepdata$V1,
                     hermitair3.perstepdata$C)



#### Set up Data Frame for Plot ####

#Sets working directory for plot
setwd(location)

condition<-c(rep("marinewater",nrow(marinewater)),rep("hermitair",nrow(hermitair)))
mean<-c(marinewater$mean,hermitair$mean)
sd<-c(marinewater$SD,hermitair$SD)
time<-c(marinewater$time,hermitair$time)

plot1<-data.frame(time,condition,mean,sd)

#### PLOT #################

pdf("Combined_bothconditions.pdf",width=5,height=4) 

ggplot(plot1,aes(x=time,y=mean,color=condition)) + geom_line() +
  geom_ribbon(aes(
    ymin=mean-qt(0.975,df=2)*sd,
    ymax=mean+qt(0.975,df=2)*sd,
    fill=condition),alpha=0.2,color=NA) +
  ylab(expression(C/C[infinity])) + 
  xlab('Time (s)') + 
  ylim(0,max(mean)) + 
  xlim(0,max(time[plot1$condition=="hermitair"]))

dev.off()
