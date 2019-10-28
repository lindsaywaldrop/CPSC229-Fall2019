# Load Required Packages
library(grid) 
require(tiff)

# Clear Workspace
rm(list=ls())

# Move to working directory
setwd("/Users/Spectre/Dropbox/microPIV/prelimdata/subset1")

k = 2       # Total number of image pairs
scale = 0.02   # Scale of the length of vector arrows
angle = 25     # Angle between the arrow head and body
length = 0.03  # Length of the arrow head sides

start=40       # Start number of image sequence
end=240        # End number of image sequence
imageseq<-seq(start,end) # Constructs vector with image sequence for later reference

# Construct a function for creating the color map for vectors
myColorRamp <- function(colors, values) {
  v <- (values - min(values))/diff(range(values))
  x <- colorRamp(colors)(v)
  rgb(x[,1], x[,2], x[,3], maxColorValue = 255)
}

# Construct a function for calculating vector magnitudes and replacing NaN's with 0's
magnitude <- function(u,v) {
	it<-sqrt(u^2+v^2)
	it <- replace(it, is.na(it), 0)
}

# This section reads in files for pair 31, which had the highest magnitudes of any other pair, used to scale color on all pairs
u1<-read.csv("u31.csv",header=FALSE)
u31<-as.matrix(u1)
v1<-read.csv("v31.csv",header=FALSE)
v31<-as.matrix(v1)
w<-magnitude(u31,v31)						#Calculates magnitudes
cols <- myColorRamp(c("black", "red"), w)	#Constructs color map

# Main loop 
for (i in 1:k){
	
	# Constructs file names
	namex<-paste("x",i,".csv",sep="")
	namey<-paste("y",i,".csv",sep="")
	nameu<-paste("u",i,".csv",sep="")
	namev<-paste("v",i,".csv",sep="")

	# Constructs image file name
	if (imageseq[i]<100) {
		nametiff<-paste("tunicate_PIV_07_14_C001H001S0001_images0000",imageseq[i],".tif",sep="")
	} else {
		nametiff<-paste("tunicate_PIV_07_14_C001H001S0001_images000",imageseq[i],".tif",sep="")
	}
		
	# Loads individual files
	x1<-read.csv(namex,header=FALSE)
	x<-as.matrix(x1)
	y1<-read.csv(namey,header=FALSE)
	y<-as.matrix(y1)
	u1<-read.csv(nameu,header=FALSE)
	u<-as.matrix(u1)
	v1<-read.csv(namev,header=FALSE)
	v<-as.matrix(v1)
	img<-readTIFF(nametiff)
	
	# Calculates velocity magnitudes
	w<-magnitude(u,v)
	
	# Plots and Saves Image and PIV data
	filename=paste("Rplots/lPIV",i,".png",sep="")
	png(file=filename,6.5,9.5,units="in",res=600) # Saves file
	par(mar=c(0,0,0,0)) # Sets margins of plot in frame
	plot(c(min(x),max(x)),c(min(y),max(y)),pch=".",xlim=c(0,10),ylim=c(-9,5),xlab=c(" "),ylab=c(" ")) # Sets inital plot
	rasterImage(img,min(x),min(y),max(x),max(y))  # Draws background image
	arrows(x, y, x+scale*u, y+scale*v, angle=10, length=length, col=cols, code=1)  #Draws PIV arrows
	dev.off()  #Closes saving device
	
}
