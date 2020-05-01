# Assignment: Getting and Cleaning Data Course Project
# 
# Instructions
# 
# The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set.
# Review criterialess 
# The submitted data set is tidy.
# The Github repo contains the required scripts.
# GitHub contains a code book that modifies and updates the available codebooks with the data to indicate all the variables and summaries calculated, along with units, and any other relevant information.
# The README that explains the analysis files is clear and understandable.
# The work submitted for this project is the work of the student who submitted it.
# Getting and Cleaning Data Course Projectless 
# The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.
#
# One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:
# 
#   http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
#
# Here are the data for the project:
# 
#   https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip
#
# You should create one R script called run_analysis.R that does the following.
#
# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement.
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names.
# From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
# Good luck!


loadAndMergeData <- function () {
  # Load libraries
  library(reshape2)
  library(dplyr)
  
  # init variables about subdirectory, zipfilename and zipfilepath
  subdirectory              <- "./data/"
  zipfilename               <- "UCI HAR Dataset.zip"
  zipfilepath               <- paste0(subdirectory, zipfilename)
  
  # If subdirectory "data" doesn't exists, then create this.
  if (!file.exists("data")) {
    dir.create("data")
  }
  
  # If zip file isn't stored in subdirectory data, it will be downloaded.
  if (!file.exists(zipfilepath)){
    fileURL                 <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
    download.file(fileURL, zipfilepath, method="curl")
  }  
  
  # Upzip downloaded zip file
  if (!file.exists("UCI HAR Dataset")) { 
    unzip(zipfilepath, exdir = subdirectory) 
  }
  
  # Load "activity labels" & "features" and convert them into character.
  activityLabels            <- read.table(paste0(subdirectory, 
                                                 "UCI HAR Dataset/activity_labels.txt"), 
                                                  col.names=c("activityId", "activity"))
  activityLabels[,2]        <- as.character(activityLabels[,2])
  features                  <- read.table(paste0(subdirectory, 
                                                 "UCI HAR Dataset/features.txt"), 
                                                  col.names=c("featureId", "featureLabel"))
  features[,2]              <- as.character(features[,2])
  
  # Extract only the data about mean and standard deviation
  features.MeanAndStd       <- grep(".*mean.*|.*std.*", features[,2])
  features.MeanAndStd.names <- features[features.MeanAndStd,2]
  
  # Adjust labels into CamelCase notation and without dashs
  features.MeanAndStd.names = gsub('-mean', 'Mean', features.MeanAndStd.names)
  features.MeanAndStd.names = gsub('-std', 'Std', features.MeanAndStd.names)
  features.MeanAndStd.names <- gsub('[-()]', '', features.MeanAndStd.names)
  
  # Load test data
  set.test                  <- read.table(paste0(subdirectory, "UCI HAR Dataset/test/X_test.txt"))
  activities.test           <- read.table(paste0(subdirectory, "UCI HAR Dataset/test/Y_test.txt"))
  subjects.test             <- read.table(paste0(subdirectory, "UCI HAR Dataset/test/subject_test.txt"))
  
  # Load training data
  set.training              <- read.table(paste0(subdirectory, "UCI HAR Dataset/train/X_train.txt"))
  activities.training       <- read.table(paste0(subdirectory, "UCI HAR Dataset/train/Y_train.txt"))
  subjects.training         <- read.table(paste0(subdirectory, "UCI HAR Dataset/train/subject_train.txt"))

  # Merge "subject data" from training and test
  subject.merged            <- rbind(subjects.test, subjects.training)
  # Rename subject identifier column in "subjectId"
  names(subject.merged)     <- "subjectId"
  
  # Merge "set data" from test and training data and select only mean and standard deviation values
  set.merged                <- rbind(set.test, set.training)
  set.merged                <- set.merged [, features.MeanAndStd]
  # Rename column name from selected feature labels
  names(set.merged)         <- features.MeanAndStd.names
  
  # Merge "activity data" from test and training data
  activities.merged         <- rbind(activities.test, activities.training)
  # Rename activity identifier column in "activityId"
  names(activities.merged)  = "activityId"
  # Merge activity data and activity labels
  activities.merged         <- merge(activities.merged, 
                                     activityLabels, 
                                     all.x = TRUE,
                                     by="activityId")
  
  # Merge merged subject data, merged activity data and merged set data
  data.merged               <- cbind(subject.merged, activity = activities.merged$activity, set.merged)
  # Write merged data into the file "tidy_data.txt"
  write.table(data.merged, "merged_data.txt")
  
  # Calculte the average of each variable grouped by activity and subject.
  data.merged <- tbl_df(data.merged)
  data.tidy <- data.merged %>% group_by(subjectId, activity) %>% summarise_each(funs(mean))
  write.table(data.tidy, "tidy_data.txt")
}
