# 17 - Cleaning & Transforming Data 

# Strings: informal name for data class character, or a subset of characters
# either double quotes or single quotes
"This is a string"
'This is a string'
'This is a "string"'

strs<-c("This is a ","string.")

# for loop
for(i in 1:100){
  print(paste("This is the number i",i,".","No really it is ",i,"!",sep=""))
}

#while loop
x<-1
while(x<=100){
  print(paste("This is the number ",x,"."," No really it is ",x,"!",sep=""))
  x<-x+1
}

#repeat loop
#???
str<-c("How much wood could a woodchuck chuck?")
strsplit(str," ",fixed=TRUE)

# Manipulating and cleaning data frames
anolis<-read.csv("AnolisData2.csv")
anolis2<-anolis[complete.cases(anolis),]
anolis3<-na.omit(anolis) #na.omit() does the same as line 30!
isFALSE(anolis2==anolis3) #Checks to see if the two methods are equivalent (they are)

# Adding columns in a data frame
anolis$coolness<-anolis$awesomeness-anolis$hostility
anolis$Cool<-with(anolis,attitude-hostility)

data("deer_endocranial_volume",package="learningr") #You must have the learningr package installed
deer_wide<-na.omit(deer_endocranial_volume)

library(reshape2)  #You must have the reshape2 package installed
# Reshaping data frames into long format
deer_long<-melt(deer_wide,id.vars = "SkullID")
deer_long<-melt(deer_wide,measure.vars=
                  c("VolCT","VolBead","VolLWH","VolFinarelli","VolCT2",
                    "VolBead2","VolLWH2"))
library(ggplot2)
ggplot(deer_long,aes(x=variable,y=value))+
  geom_bar(stat="summary",fun.y="mean",fill="blue")

# Sorting data
x<-c(12,8,498,1,9)
sort(x)
sort(x,decreasing=TRUE)

order(x)
x[order(x)] #This does the same as sort(x)

ggplot(anolis2,aes(x=reorder(ecomorph,SVL,mean),y=SVL))+
  geom_bar(stat="summary",fun.y="mean")
