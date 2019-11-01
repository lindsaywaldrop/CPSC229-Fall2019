# Automation Example to be covered in class 10/31/19
#
# In this study, I have modeled bird wings from 20 species of birds and 
# saved the coefficients of drag (CD) and lift (CL) at a range of speeds
# and angle of attacks (aoa) in CSV files. Each folder has results from 
# a different bird. I've given you some starting code to start! 
# 

# Loading a single file: 
birbs<-function(j){
  data<-read.csv("rep1/new_Wing_Polar_Graph_0.csv")
}


data <- data[,colSums(is.na(data))<nrow(data)] # Removing columns with only NA's

speeds<-seq(5,20,by=0.5)  # This creates a vector of air speeds from 5ms/ to 20m/s
# These speeds correspond to the rows in the CSV 1-31.
data$speeds<-speeds
# This creates a vector of angle of attacks that correspond to each pair of columns
aoa<-seq(0,5,by=1)  

# If you load a single CSV file as "data", you can calculate the lift-to-drag ratio like this:
CLCD<-rep(NA,length(aoa))

for (i in 1:length(aoa)){
  message("column 1: ",2*i,", column 2: ",2*i-1)
  CLCD[i]<-data[data$speeds==10,2*i]/data[data$speeds==10,2*i-1] #This is lift-to-drag 
}

#Challenge: calculate the lift-to-drag ratios of all the birds at 10 m/s and plot CLCD against angle of attack. 
