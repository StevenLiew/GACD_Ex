library(plyr)
library(reshape2)

## Read 'feature.txt' and rename column name
feature <- read.table("UCI HAR Dataset/features.txt", header = FALSE, col.names=c("id","name")) 
reqfeature <- grep("-mean\\(\\)|-std\\(\\)", feature$name)
feature$name <- gsub("tBody", "Time.Body", feature$name)
feature$name <- gsub("tGravity", "Time.Gravity", feature$name)
feature$name <- gsub("fBody", "FFT.Body", feature$name)
feature$name <- gsub("fGravity", "FFT.Gravity", feature$name)
feature$name <- gsub("\\-mean\\(\\)\\-", ".Mean.", feature$name)
feature$name <- gsub("\\-std\\(\\)\\-", ".Std.", feature$name)
feature$name <- gsub("\\-mean\\(\\)", ".Mean", feature$name)
feature$name <- gsub("\\-std\\(\\)", ".Std", feature$name)

## Read 'activity_labels.txt'
activity <- read.table("UCI HAR Dataset/activity_labels.txt", header = FALSE, col.names=c("id","activity"))

## Read TEST file
## 4. Appropriately labels the data set with descriptive variable names. 
test_sub <- read.table("UCI HAR Dataset/test/subject_test.txt",header = FALSE, col.names=c("subject"))
test_set <- read.table("UCI HAR Dataset/test/x_test.txt",header = FALSE, col.names=feature[,2])
test_id <- read.table("UCI HAR Dataset/test/y_test.txt",header = FALSE, col.names=c("activityid"))

## Read TRAIN file
## 4. Appropriately labels the data set with descriptive variable names. 
train_sub <- read.table("UCI HAR Dataset/train/subject_train.txt",header = FALSE, col.names=c("subject"))
train_set <- read.table("UCI HAR Dataset/train/x_train.txt",header = FALSE, col.names=feature[,2])
train_id <- read.table("UCI HAR Dataset/train/y_train.txt",header = FALSE, col.names=c("activityid"))

## 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
test_set <- test_set[,reqfeature]
train_set <- train_set[,reqfeature]

## 1. Merges the training and the test sets to create one data set.
train_data <- cbind(train_sub, train_id, train_set )
test_data <- cbind(test_sub, test_id, test_set )
tidy <- rbind(test_data,train_data)

## 3. Uses descriptive activity names to name the activities in the data set.
tidy <- merge(tidy, activity, by.x="activityid", by.y="id" )

## 5: Creates a second, independent tidy data set with the average of each variable for each activity and each subject.
idLabels   <- c("subject", "activityid", "activity")
dataLabels <- setdiff(colnames(tidy), idLabels)
meltData <- melt(tidy, id = idLabels, measure.vars = dataLabels)
tidyMean <- dcast(meltData, subject + activityid + activity ~ variable, mean)

## Write output into a file
write.table(tidyMean, file="./tidy_mean.txt", sep="\t", row.names=FALSE)
write.table(tidy, file="./tidy.txt", sep="\t", row.names=FALSE)