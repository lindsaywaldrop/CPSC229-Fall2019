#Line Plots Using Base Graphics
library(datasets)
#Load the data into R
data(AirPassengers)
#Assign the dataset to a variable
AP <- AirPassengers
class(AP) #class is ts meaning this dataset is a time-series; fit for a line plot
#the graph will plot as a line graph
plot(AP, xlab="Year", type = "o", pch = 20,   #add labels and line type
     ylab = "Passenger numbers (1000's)",
     main="Air Passenger numbers from 1949 to 1961")
#abline will add a line of best fit to help with trend
abline(reg=lm(AirPassengers~time(AirPassengers)), col = "red")
#aggregate displays a year on year trend
plot(aggregate(AirPassengers,FUN=mean), 
     main="Air Passengers from 1949 to 1961",
     xlab="Year", ylab = "Passenger numbers (1000's)",
     col = "red")

#Line Plots Using ggPlot2
library(ggplot2)
autoplot(AirPassengers) #simple way to graph, however ggfortify provides more customizations
install.packages(                    #Use ggfortify, an extension of ggplot2 for timeseries
  c("ggfortify", "changepoint",      
    "strucchange", "ggpmisc"))
library(ggfortify) 
autoplot(AirPassengers,                              #ggfortify tells ggplot2 how to plot time series graphs
         main = "Air Passengers from 1949 to 1961", 
         xlab = "Year", 
         ylab = "Passengers")
autoplot(AirPassengers,
         main = "Air Passengers from 1949 to 1961", 
         xlab = "Year", 
         ylab = "Passengers",
         ts.colour = 'red', ts.linetype = 'dashed') #change line color and line shape
#Another way to add titles would be to assign the autoplot to a variable and use + ggtitle
ggfreqplot(AirPassengers) #shows the data on a monthly basis
library(changepoint) #Another extension of ggplot2 to define changepoints of data
autoplot(cpt.meanvar(AirPassengers), 
         main = "Air Passengers from 1949 to 1961", 
         xlab = "Year", 
         ylab = "Passengers",)
autoplot(cpt.meanvar(AirPassengers),
         main = "Air Passengers from 1949 to 1961", 
         xlab = "Year", 
         ylab = "Passengers",
         cpt.colour = 'blue', cpt.linetype = 'solid') #change color and shape of line
