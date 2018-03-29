filename <- "UCI HAR Dataset.zip"

## Download and unzip the dataset:
if (!file.exists(filename)){
        fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
        download.file(fileURL, filename, method = "curl")
}  
if (!file.exists("UCI HAR Dataset")) { 
        unzip(filename) 
}

## Importing "dply" library for Data Manipulation
library(dplyr)

## Reading training data
train_subjects <- read.table("UCI HAR Dataset/train/subject_train.txt")
train_values <- read.table("UCI HAR Dataset/train/X_train.txt")
train_activity <- read.table("UCI HAR Dataset/train/y_train.txt")

## Reading test data
test_subjects <- read.table("UCI HAR Dataset/test/subject_test.txt")
test_values <- read.table("UCI HAR Dataset/test/X_test.txt")
test_activity <- read.table("UCI HAR Dataset/test/y_test.txt")

#### Reading features and activities
all_features <- read.table("UCI HAR Dataset/features.txt")
activities <- read.table("UCI HAR Dataset/activity_labels.txt")

#### Step 1 - Merge the training and the test sets to create one data set

## Column binding all the traing and testing data
train_data <- cbind(train_subjects, train_activity, train_values)
test_data <- cbind(test_subjects, test_activity, test_values)

## Merging individual data tables(taining and testing) to make single dataset
mergedData<- rbind(train_data, test_data)

#### Step 2 - Extract only the measurements on the mean and standard deviation
####          for each measurement

colnames(mergedData) <- c("Subject", "Activity", as.character(all_features[,2]))
required_features_with_meanFreq <- grep("Subject|Activity|mean|std", names(mergedData), value = TRUE)
meanFreq <- c(grep("meanFreq", names(mergedData), value = TRUE))
mergedData <- mergedData[, required_features_with_meanFreq]
mergedData <- mergedData[, !(names(mergedData) %in% meanFreq)]

#### Step 3 - Use descriptive activity names to name the activities in the
####          data set                                                    

## Replacing Activity and Subject values with named factor levels
mergedData$Activity <- factor(mergedData$Activity, levels = activities[, 1], labels = activities[,2])
mergedData$Subject <- as.factor(mergedData$Subject)

#### Step 4 - Appropriately label the data set with descriptive variable names 

## Getting column names
colNames_mergedData <- colnames(mergedData)

## Removing special characters
colNames_mergedData <- gsub("[\\(\\)-]", "", colNames_mergedData)

## Correcting typo of "BodyBody" with "Body"
colNames_mergedData <- gsub("BodyBody", "Body", colNames_mergedData)

## Expand abbreviations
colNames_mergedData <- gsub("^f", "FrequencyDomain", colNames_mergedData)
colNames_mergedData <- gsub("^t", "TimeDomain", colNames_mergedData)
colNames_mergedData <- gsub("Acc", "Accelerometer", colNames_mergedData)
colNames_mergedData <- gsub("Gyro", "Gyroscope", colNames_mergedData)
colNames_mergedData <- gsub("Mag", "Magnitude", colNames_mergedData)
colNames_mergedData <- gsub("mean", "Mean", colNames_mergedData)
colNames_mergedData <- gsub("std", "StandardDeviation", colNames_mergedData)

## Using the new labels as column names
colnames(mergedData) <- colNames_mergedData

#### Step 5 - Create a second, independent tidy set with the average of each
####          variable for each activity and each subject

## Grouping by Subject and Activity and using calculating mean using chaining
mergedDataMeans <- mergedData %>% 
        group_by(Subject, Activity) %>%
        summarize_all(funs(mean))

# Writing output to file "tidy_data.txt"
write.table(mergedDataMeans, "tidy_data.txt", row.names = FALSE, quote = FALSE)