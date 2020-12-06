#clear work space
rm(list=ls())

#install pacman to check for required packages
if (!require('pacman')) install.packages('pacman'); library(pacman) 

# load (install if required) packages from CRAN
p_load("dplyr", "tidyverse",  "data.table")


#Check if a directory exists, and if not, Create one
#Directory is GCD (Getting and Cleaning Data)
if(!file.exists("GCD")){
  dir.create("GCD")
}

#the data to be used for the project will be in a zipped file, downloaded from a website
#assign location of data (URL) to object
zip_url <- 'https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip'

#download zipped folder
download.file(zip_url, destfile = "./GCD.zip")
zipfile <- "./GCD.zip"
unzip(zipfile, exdir = "./GCD", unzip = "internal")
list.files("./GCD")

#read required files to r objects with corresponding file/object names

#_test objects contain 2947 observations related to testing
x_test <- read.table("./GCD/UCI HAR Dataset/test/X_test.txt", sep = "")
y_test <- read.table("./GCD/UCI HAR Dataset/test/y_test.txt", sep = "")
s_test <- read.table("./GCD/UCI HAR Dataset/test/subject_test.txt", sep = "")

#_train objects contain 7352 observations related to training
x_train <- read.table("./GCD/UCI HAR Dataset/train/X_train.txt", sep = "")
y_train <- read.table("./GCD/UCI HAR Dataset/train/y_train.txt", sep = "")
s_train <- read.table("./GCD/UCI HAR Dataset/train/subject_train.txt", sep = "")

#activity contains the numbered codes for each activity and the corresponding 
#activity description
activity <- read.table("./GCD/UCI HAR Dataset/activity_labels.txt")

#the features file contains the names of the variables for the objects labeled x
features <- read.table("./GCD/UCI HAR Dataset/features.txt")

#pull the feature names and add features as column names to x data objects
var_name <- pull(features, 'V2')

colnames(x_test) <- var_name
colnames(x_train) <- var_name

#label columns for other objects
#the s_ files contain subject identifiers; label the columns id
colnames(s_test)  <- c("id")
colnames(s_train) <- c("id")

#the y_ files contain the numbered code for each physical activity recorded
colnames(y_test) <- c("code")
colnames(y_train) <- c("code")

colnames(activity) <- c('code', 'activity')

#join subject id with x and y test/train
test <- cbind(s_test, y_test, x_test)
train <- cbind(s_train, y_train, x_train)

#join the created test and train objects together
test_train <- rbind(test, train)

#select only the required variables (mean and std)
 test_train <- test_train%>% 
  select(id, code, contains("mean"), contains("std"))

#make activity_code factor in both data frames so that they will match for join 
activity$code <- as.factor(activity$code) 
test_train$code <- as.factor(test_train$code)

#match activity code and replace with activity description
test_train$code <- activity$activity[match(test_train$code, activity$code )]

#create more descriptive variable names
colnames(test_train)<-gsub("Acc", "Accelerometer", colnames(test_train))
colnames(test_train)<-gsub("Gyro", "Gyroscope", colnames(test_train))
colnames(test_train)<-gsub("BodyBody", "Body", colnames(test_train))
colnames(test_train)<-gsub("Mag", "Magnitude", colnames(test_train))
colnames(test_train)<-gsub("^t", "Time", colnames(test_train))
colnames(test_train)<-gsub("^f", "Frequency", colnames(test_train))
colnames(test_train)<-gsub("tBody", "TimeBody", colnames(test_train))
colnames(test_train)<-gsub("mean", "Mean", colnames(test_train), ignore.case = TRUE)
colnames(test_train)<-gsub("std", "STD", colnames(test_train), ignore.case = TRUE)
colnames(test_train)<-gsub("freq", "Frequency", colnames(test_train), ignore.case = TRUE)
colnames(test_train)<-gsub("angle", "Angle", colnames(test_train))
colnames(test_train)<-gsub("gravity", "Gravity", colnames(test_train))
colnames(test_train)<-gsub("\\()", "", colnames(test_train))
colnames(test_train)<-gsub("code", "ActivityDescription", colnames(test_train))

#test_train is now a tidy data set, ready to be summarized

summary_test_train <- test_train %>%
  group_by(id, ActivityDescription) %>% 
  summarise_all(funs(mean))  

#write to a .txt file
write.table(summary_test_train, "./GCD/UCI HAR Dataset/summary_test_train.txt", 
            row.name=FALSE, col.names = TRUE)
  
#sample code to read the summary back in
tidy_data <- read.table( "./GCD/UCI HAR Dataset/summary_test_train.txt", header = TRUE)
