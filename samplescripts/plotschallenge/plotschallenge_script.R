## Plots challenge script
# Code to complete the plots challenge 10/15/19 - L. Waldrop

library(ggplot2)   # Loads ggplot2

data("airquality") # Loads airquality data set

p<-ggplot(airquality,aes(x=Temp,y=Ozone))+geom_point()  # Sets up base plot to which red points will be added.

for(i in seq(min(airquality$Month),max(airquality$Month))){  #For loop that cycles from 5 (min value of Month) through 9 (max value of Month)
  for(j in seq(min(airquality$Day),max(airquality$Day))){  #For loop that cycles from 1 (min value of Day) through 31 (max value of Day)
    # The next line is a control feature, it breaks the day loop if you are in a month with only 30 days (6 and 9), 
    # so it doesn't try to plot values it can't find.
    if(isTRUE(all.equal(airquality$Ozone[airquality$Month==i & airquality$Day==j],integer(0)))) break 
    message(paste("month:",i," day:",j))  # Displays the current month and day
    message(paste("Ozone: ",airquality$Ozone[airquality$Month==i & airquality$Day==j]))  # Displays the current Ozone
    tiff(file=paste("plot",i,"-",j,".tif",sep=""),width=500,height=450)   # Saves an individual file as a tiff image
      print(       # This is required for working inside loops, it will print to file.
        p+geom_point(aes(  # The following code calls the based plot p and then adds the red point and annotation text
            x=airquality$Temp[airquality$Month==i & airquality$Day==j],
            y=airquality$Ozone[airquality$Month==i & airquality$Day==j]),
            color="red") +
          annotate(geom="text",x=60,y=165,label=paste("Month: ",i,", Day: ",j,sep=""))
      )  # Closes print() command
    dev.off()    #shuts off plotting device
  }  # Ends days loop
}   # Ends months loop

# This is only ONE WAY of accomplishing this task. There are many other ways to do the same
# thing, so if your code is different, then that's fine! 

