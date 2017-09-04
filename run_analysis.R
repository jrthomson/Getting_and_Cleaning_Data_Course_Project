library(dplyr)

######## 
#  
# 1. Get data
# 
########

# 1.1 Download the zip file if it hasn't already been downloaded

zipUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
zipFile <- "UCI HAR Dataset.zip"

if (!file.exists(zipFile)) {
	download.file(zipUrl, zipFile,)
}

# 1.2 Unzip zip file containing data if data directory doesn't already exist

dataPath <- "UCI HAR Dataset"
if (!file.exists(dataPath)) {
  	unzip(zipFile)
}

########
#
# 2. Read data
#
########

# 2.1 Read training data

trainingSubjects <- read.table(file.path(dataPath, "train", "subject_train.txt"))
trainingValues <- read.table(file.path(dataPath, "train", "X_train.txt"))
trainingActivity <- read.table(file.path(dataPath, "train", "y_train.txt"))

# 2.2 Read test data

testSubjects <- read.table(file.path(dataPath, "test", "subject_test.txt"))
testValues <- read.table(file.path(dataPath, "test", "X_test.txt"))
testActivity <- read.table(file.path(dataPath, "test", "y_test.txt"))

# 2.3 Read features

features <- read.table(file.path(dataPath, "features.txt"), as.is = TRUE)

# 2.4 Read activity

activities <- read.table(file.path(dataPath, "activity_labels.txt"))
colnames(activities) <- c("activityId", "activityLabel")

########
#
# 3. Merge the training and test sets to make one data set
#
########

# 3.1 Make single data table

humanActivity <- rbind(
	cbind(trainingSubjects, trainingValues, trainingActivity),
	cbind(testSubjects, testValues, testActivity)
)

# 3.2 Remove unnecessary data

rm(trainingSubjects, trainingValues, trainingActivity, 
  	testSubjects, testValues, testActivity)

# 3.3 Use descriptive names

colnames(humanActivity) <- c("subject", features[, 2], "activity")

########
# 
# 4. Extract only the measurements on the mean and standard deviation for each measurement. 
#          
########

# 4.1 determine columns of data set to keep based on column name...

columnsKeep <- grepl("subject|activity|mean|std", colnames(humanActivity))
humanActivity <- humanActivity[, columnsKeep]

########
# 
# 5. Use descriptive activity names to name the activities in the data set
#          
########

# 5.1 replace activity values with named factors

humanActivity$activity <- factor(humanActivity$activity, 
	levels = activities[, 1], labels = activities[, 2])

########
#
# 6. Label the data set with descriptive variable names
#
########

# 6.1 Get column names

humanActivityCols <- colnames(humanActivity)

# 6.2 Remove special characters

humanActivityCols <- gsub("[\\(\\)-]", "", humanActivityCols)

# 6.3 Clean up names

humanActivityCols <- gsub("^f", "frequencyDomain", humanActivityCols)
humanActivityCols <- gsub("^t", "timeDomain", humanActivityCols)
humanActivityCols <- gsub("Acc", "Accelerometer", humanActivityCols)
humanActivityCols <- gsub("Gyro", "Gyroscope", humanActivityCols)
humanActivityCols <- gsub("Mag", "Magnitude", humanActivityCols)
humanActivityCols <- gsub("Freq", "Frequency", humanActivityCols)
humanActivityCols <- gsub("mean", "Mean", humanActivityCols)
humanActivityCols <- gsub("std", "StandardDeviation", humanActivityCols)

# 6.4 Correct error

humanActivityCols <- gsub("BodyBody", "Body", humanActivityCols)

# 6.5 use new labels as column names

colnames(humanActivity) <- humanActivityCols

########
#
# 7. Create a tidy set with the average of each variable for each activity and each subject
#
########

# 7.1 Group by subject and activity - summarize using mean
humanActivityMeans <- humanActivity  
	group_by(subject, activity) 
	summarize_each(funs(mean))

# 7.2 output to file "tidy_data.txt"
write.table(humanActivityMeans, "tidy_data.txt", row.names = FALSE, 
            quote = FALSE)
