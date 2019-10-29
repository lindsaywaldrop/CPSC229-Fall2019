#Importing data from packages
data()
# data("datasetname",package="packagename")

data("kidney",package="survival") #Pull data set from specific package

# Importing data from text files
# read.csv() already done
# read.table() general function
# read.table("file",sep = "") <- specifies the character that deliminates 
# the text within the file
# sep = "," (CSV), "\t" (Tab-SV, TSV), sep = " ", space
data<-read.table("blue.csv",sep=",") # read a CSV
data<-read.csv("blue.csv")  #equivalent to above line!!
data<-read.table("babycrab2.tsv",sep="\t",skip=1) # reads TSV file, skips first line

read.csv2 # For European files

# Note: colbycol, sqldf packages will read in partial CSV files, in case you need that
# readLines() reads unstructed text files. 
data<-readLines("file.txt")

# Importing Binary files
# Excel files 
library(xlsx) # requires rJava
library(readxl) # does not require rJava
data<-read_xlsx("tunicatespecies.xlsx") # Imports Excel files!

library(R.matlab) # for importing MATLAB files (.mat)
data<-readMat("randomnumbers.mat")

# Importing Image files
library(jpeg) # Works for .jpeg, .jpg
library(png)   # Works for .png
library(tiff) # Works for .tif, .tiff

image<-readJPEG("Raleigh.jpg") # Imports image into R
plot(c(0,1),c(0,1))  # Sets up initial plot
rasterImage(image,0,0,1,1) # Plots image



