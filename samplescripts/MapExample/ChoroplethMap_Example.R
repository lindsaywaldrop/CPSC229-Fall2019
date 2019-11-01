# required packages
library(maps) # for the state map data
library(ggplot2) # required for plotting, map_data() 
library(dplyr)  # required for arrange()
library(stringr)

states_map<-map_data("state") #extracts data from the states map

# Make a data set of US crime data from the USArrests data set
crimes<-data.frame(state=tolower(rownames(USArrests)),USArrests)
crime_map<-merge(states_map,crimes,by.x="region",by.y="state")
crime_map<-arrange(crime_map,group,order)
head(crime_map)

# Plot make of murder rate. You can select any column in that 
ggplot(crime_map,aes(x=long,y=lat,group=group,fill=Murder)) +
  geom_polygon() +
  coord_map("polyconic") 


# Write the "favorite" state food (from the StateFood.csv file) on each state.
states_map<-map_data("state") #extracts data from the states map

statefoods<-read.csv("StateFood.csv",row.names = 1)
foods<-data.frame(state=tolower(rownames(statefoods)),statefoods)
foods<-arrange(foods,state)
foods_map<-merge(states_map,foods,by.x="region",by.y="state")
foods_map<-arrange(foods_map,group,order)
cnamefood<-aggregate(cbind(long,lat)~Food,data=foods_map,FUN=function(x)median(range(x)))

# Plots foods on each state, on a white and black US map
ggplot(foods_map,aes(x=long,y=lat,group=group)) + 
  geom_polygon(color="black",fill="white") + 
  geom_text(data=cnamefood,aes(x=long,y=lat,group=Food,label=str_wrap(Food,width=10)))

# Adding a random set of numbers to get a fill on the above map
statefoods<-read.csv("StateFood.csv",row.names = 1)
statefoods$Grossness<-runif(nrow(statefoods),0,1)
foods<-data.frame(state=tolower(rownames(statefoods)),statefoods)
foods<-arrange(foods,state)
foods_map<-merge(states_map,foods,by.x="region",by.y="state")
foods_map<-arrange(foods_map,group,order)
cnamefood<-aggregate(cbind(long,lat)~Food,data=foods_map,FUN=function(x)median(range(x)))

# Plots foods on each state on a map with Grossness scaled between light blue and light gray
ggplot(foods_map,aes(x=long,y=lat,group=group,fill=Grossness)) + 
  geom_polygon(color="black") + 
  scale_fill_gradient2(low="lightblue",high="gray90",midpoint=mean(foods_map$Grossness))+
  geom_text(data=cnamefood,aes(x=long,y=lat,group=Food,label=str_wrap(Food,width=10)),inherit.aes=FALSE)

