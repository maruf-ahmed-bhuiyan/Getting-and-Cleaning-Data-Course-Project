# Getting and Cleaning Data Project
# Author: Maruf Ahmed Bhuiyan

# Merges the training and the test sets to create one data set.
# Extracts only the measurements on the mean and standard deviation for each measurement.
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names.
# From the data set in step 4, creates a second, independent tidy data 
# set with the average of each variable for each activity and each subject.

# loadin the required packages
library(data.table)
library(reshape2)

# i have already downloaded and uzipped the file in the working directory.
path <- getwd()

activityLabels <- fread(file.path(path, "UCI HAR Dataset/activity_labels.txt")
                        , col.names = c("classLabels", "activityName"))
View(activityLabels)

features <- fread(file.path(path, "UCI HAR Dataset/features.txt")
                  , col.names = c("index", "featureNames"))
View(features)

# extractng only the measurements on the mean and 
# standard deviation from featurenames by use of grep() [greeple function]

featuresWanted <- grep("(mean|std)\\(\\)", features[, featureNames])
measurements <- features[featuresWanted, featureNames]

# substituing parehthesises() with blank using gsub()
measurements <- gsub('[()]', '', measurements)
measurements

# loading the train datasets
train <- fread(file.path(path, "UCI HAR Dataset/train/X_train.txt"))[, featuresWanted, with = FALSE]
setnames(train, colnames(train), measurements)

trainActivities <- fread(file.path(path, "UCI HAR Dataset/train/Y_train.txt")
                         , col.names = c("Activity"))
trainSubjects <- fread(file.path(path, "UCI HAR Dataset/train/subject_train.txt")
                       , col.names = c("SubjectNum"))
train <- cbind(trainSubjects, trainActivities, train)

# loading the test datasets
test <- fread(file.path(path, "UCI HAR Dataset/test/X_test.txt"))[, featuresWanted, with = FALSE]
setnames(test, colnames(test), measurements)

testActivities <- fread(file.path(path, "UCI HAR Dataset/test/Y_test.txt")
                        , col.names = c("Activity"))
testSubjects <- fread(file.path(path, "UCI HAR Dataset/test/subject_test.txt")
                      , col.names = c("SubjectNum"))

test <- cbind(testSubjects, testActivities, test)

# merging the both datasets
combined <- rbind(train, test)
View(combined)
colnames(combined)
str(combined$Activity)

# assigning descriptive names to the variables
combined[["Activity"]] <- factor(combined[, Activity]
                                 , levels = activityLabels[["classLabels"]]
                                 , labels = activityLabels[["activityName"]])

combined[["SubjectNum"]] <- as.factor(combined[, SubjectNum])
View(combined)

combined <- melt(data = combined, id = c("SubjectNum", "Activity"))
combined <- dcast(data = combined, SubjectNum + Activity ~ variable, 
                  fun.aggregate = mean)

write.table(x = combined, file = "tidydata.txt", row.name=FALSE)
