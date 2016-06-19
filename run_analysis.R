## Please read the adjoining readme files to understand how the code works and how to run it on the data

## calling reqd librariies
library(httr)

## setting parameters for download
url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
file <- "dataSet.zip"

## downloading the file
if(!file.exists("dataSet.zip")){
  print("Downloading File")
  download.file(url, file, method = "curl")
}

## creating folders and extracting the .zip file for use
dataFolder <- "UCI HAR Dataset"
resultsFolder <- "results"
if(!file.exists(dataFolder)){
  print("Extracting Files")
  unzip(file, list = FALSE, overwrite = TRUE)
}
if(!file.exists(resultsFolder)){
  print("Creating Results Folder")
  dir.create(resultsFolder)
}

## reading .txt files and converting in to data frame for use
getTables <- function(filename, cols = NULL){
  print(paste("Getting Table:", filename))
  a <- paste(dataFolder, filename, sep = "/")
  data <- data.frame()
  if(is.null(cols)){
    data <- read.table(a,sep = "", stringsAsFactors = F)
  }
  else{
    data <- read.table(a, sep = "", stringsAsFactors = F, col.names= cols)
  }
  data
}

## running getTables on features.txt
features <- getTables("features.txt")

## reading rest of the files and building the databse
getData <- function(type, features){
  print(paste("Getting Data", type))
  subjectData <- getTables(paste(type,"/","subject_",type,".txt", sep = ""),"id")
  yData <- getTables(paste(type, "/", "y_",type,".txt",sep = ""),"activity")
  xData <- getTables(paste(type,"/","x_",type,".txt",sep = ""), features$v2)
  return (cbind(subjectData, yData, xData))
}

## -----------------------------------------------------------------------

## running and checking getData
test <- getData("test", features)
train <- getData("train", features)

## saving results in the results folder
saveResults <- function(data, name){
  print(paste("Saving Results", name))
  file <- paste(resultsFolder, "/", name, ".csv", sep = "")
  write.csv(data,file)
}

### required activities ###

#1) Merges the training and the test sets to create one data set.
library(plyr)
data <- rbind(train, test)
data <- arrange(data, id)

#2) Extracts only the measurements on the mean and standard deviation for each measurement. 
meanStd <- data[,c(1,2,grep("std", colnames(data)), grep("mean", colnames(data)))]
saveResults(meanStd,"meanStd")

#3) Uses descriptive activity names to name the activities in the data set
activity_labels <- getTables("activity_labels.txt")

#4) Appropriately labels the data set with descriptive variable names. 
data$activity <- factor(data$activity, levels=activity_labels$V1, labels=activity_labels$V2)

#5) Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 
tidyDataset <- ddply(meanStd, .(id, activity), .fun=function(x){ colMeans(x[,-c(1:2)]) })
colnames(tidyDataset)[-c(1:2)] <- paste(colnames(tidyDataset)[-c(1:2)], "_mean", sep="")
saveResults(tidyDataset,"tidyDataset")
