# Plotting a single PIV with a heatmap color background. 
library(reshape2)
library(ggplot2)
library(viridis)
library(ggquiver)

# Construct a function for calculating vector magnitudes and replacing NaN's with 0's
magnitude <- function(u,v) {
  it<-sqrt(u^2+v^2)
  #it <- replace(it, is.na(it), 0)
}

# Reads in the x,y coordinates and the u,v components of velocity, 
# calculates magnitude, melts into data frame.
read.PIV <- function(n){
  x<-read.csv(paste("x",n,".csv",sep=""),header=FALSE)  # Reads in x coordinates
  y<-read.csv(paste("y",n,".csv",sep=""),header=FALSE)  # Reads in y coordinates
  u<-read.csv(paste("u",n,".csv",sep=""),header=FALSE)  # Reads in u (x component of velocity)
  v<-read.csv(paste("v",n,".csv",sep=""),header=FALSE)  # Reads in v (y component of velocity)
  w<-magnitude(u,v)  # Calculates magnitude of velocity
  x<-melt(x)   # Melts data into single column
  y<-melt(y)
  u<-melt(u)
  v<-melt(v)
  w<-melt(w)
  data.PIV<-data.frame(x$value,y$value,
                       u$value,v$value,w$value)  # Writes everything into data frame
  names(data.PIV)<-c("x","y","u","v","w")   # Names each column
  return(data.PIV)
}

b<-10  # step factor for arrows
n<-210 # number of PIV run to use (in CSV file name)

data.PIV<-read.PIV(n) # Creates PIV data frame

# Plots colored background (scaled with magnitude) and white velocity arrows
ggplot(data.PIV,aes(x=x,y=y,u=u,v=v,color=w)) + 
  geom_point(size=2,shape=15) + 
  scale_color_viridis(na.value="#FFFFFF00") + 
  geom_quiver(data=data.PIV[seq(1,nrow(data.PIV),by=b),], aes(x=x,y=y,u=u,v=v),
              vecsize=b*100,color="white")




