# README.md
Describtion
This R-code downloads data about "Human Activity Recognition Using Smartphones Data Set" (http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones), fetchs only mean and standard deviation values and merges this set of data with activity and feature labels. All labels are corresponded to camelcase notation. At the end the result is stored in the file tidy.txt.

Assignment Submission Files
run_analysis.R
CodeBook.md
README.md
merged_data.txt
tidy_data.txt
run_analysis.R - Steps in detail
Download data from https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip and stores in the subdirectory "./data/".
Unzip downloaded zip file in "./data".
Load "activity labels"
Load "features" and select only labels about mean and standard deviation labels
Adjust lables into CamelCase notation and without dashs
Load test data
Load training data
Merge "subject data" from training and test
Rename subject identifier column in "subjectId"
Merge "set data" from test and training data and select only mean and standard deviation values
Rename column name from selected feature labels
Merge "activity data" from test and training data
Rename activity identifier column in "activityId"
Merge activity data and activity labels
Merge merged subject data, merged activity data and merged set data
Write merged data into the file "merged_data.txt"
Calculte the average of each variable grouped by activity and subject.
Write the result into the file "tidy_data.txt"
Instructions of "Assignment: Getting and Cleaning Data Course Project""
The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set.

Review criterialess
The submitted data set is tidy.
The Github repo contains the required scripts.
GitHub contains a code book that modifies and updates the available codebooks with the data to indicate all the variables and summaries calculated, along with units, and any other relevant information.
The README that explains the analysis files is clear and understandable.
The work submitted for this project is the work of the student who submitted it.
Getting and Cleaning Data Course Projectless
The purpose of this project is to demonstrate your ability to collect, work with, and clean a data set. The goal is to prepare tidy data that can be used for later analysis. You will be graded by your peers on a series of yes/no questions related to the project. You will be required to submit: 1) a tidy data set as described below, 2) a link to a Github repository with your script for performing the analysis, and 3) a code book that describes the variables, the data, and any transformations or work that you performed to clean up the data called CodeBook.md. You should also include a README.md in the repo with your scripts. This repo explains how all of the scripts work and how they are connected.

One of the most exciting areas in all of data science right now is wearable computing - see for example this article . Companies like Fitbit, Nike, and Jawbone Up are racing to develop the most advanced algorithms to attract new users. The data linked to from the course website represent data collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained:

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones

Here are the data for the project:

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

You should create one R script called run_analysis.R that does the following.

Merges the training and the test sets to create one data set.
Extracts only the measurements on the mean and standard deviation for each measurement.
Uses descriptive activity names to name the activities in the data set
Appropriately labels the data set with descriptive variable names.
From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject. Good luck!
