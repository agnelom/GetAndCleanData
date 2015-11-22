## This run script is created for creating a tidy data-set by merging test and training data sets with limited
## measurement values leading mean() and std() of the larger data set. It also provides descriptive lables
## to the measurement values

## Load the dplyr library
library(dplyr)

## the required zip file is pre-downladed and extracted in current working directory. The extracted files are loacted 
## in the "UCI HAR Dataset", which is the top level folder for the all the used by this script

fPath <- file.path("UCI HAR Dataset")

fPath_test <- file.path(fPath,"test")
fPath_train <- file.path(fPath,"train")

## Read the feature and activity labels file from the main directory
activityLabels <- read.table(file.path(fPath,"activity_labels.txt"))
featureVarNames <- read.table(file.path(fPath,"features.txt"))

## Read Activity data for test and train from the respective folders
testActivityData <- read.table(file.path(fPath_test,"y_test.txt"))
trainActivityData <- read.table(file.path(fPath_train,"y_train.txt"))

## Read Subject data for test and train from the respective folders
testSubjectData <- read.table(file.path(fPath_test,"subject_test.txt"))
trainSubjectData <- read.table(file.path(fPath_train,"subject_train.txt"))

## Read feature data for test and train from the respective folders
testFeaturesData <- read.table(file.path(fPath_test,"X_test.txt"))
trainFeaturesData <- read.table(file.path(fPath_train,"X_train.txt"))

## GOAL 1 ---------- Mearging the Data

## Merge the the Subject data for test and train using rbind to merge the rows
subjectData <- rbind(testSubjectData,trainSubjectData)

## Merge the the Activity data for test and train using rbind to merge the rows
activtyData <- rbind(testActivityData,trainActivityData)

## Merge the the Features data for test and train using rbind to merge the rows
featureData <- rbind(testFeaturesData, trainFeaturesData)

## Assign appropriate column names to Subject and Activity
names(subjectData) <- c("Subject")
names(activtyData) <- c("Activity")

## Assigne column names to features data using the values from the features.txt
names(featureData) <- featureVarNames$V2

## Merger data from the multiple data farme
Data <- cbind(subjectData,activtyData,featureData)



## Create a data frame table, in order to be able to use select()
dataDF <- tbl_df(Data)
## remove the "Data" data frame since it is not required any more
rm("Data")


## GOAL2 ---------- Extract measurments only for mean() and std()

## getting rid od duplicate column names as they could pose a problem during select()
nonDupCols <- dataDF[!duplicated(names(dataDF))]

## using special functions "contains()" along with slect to only select measurements for 'mean()' and 'std()' and
## on the way merging the output of both select commands into subData
subData <- cbind(select(nonDupCols, Subject, Activity, contains("std()")),select(nonDupCols, contains("mean()")))


## ---------------- GOAL 3 : - Uses descriptive activity names to name the activities in the data set

## using factorization for setting appropriate activity lables for each activty code for the test & train data
subData$Activity <- factor(subData$Activity, labels = activityLabels$V2)

## ---------------- GOAL 4 : - Appropriately labels the data set with descriptive variable names.
## using gsub to achive the task

names(subData) <- gsub("^t","Time", names(subData))
names(subData) <- gsub("^f","Frequency", names(subData))
names(subData) <- gsub("Acc","Accelerometer", names(subData))
names(subData) <- gsub("Gyro","Gyroscope", names(subData))
names(subData) <- gsub("Mag","Magnitude", names(subData))

## ---------------- GOAL 5 : - From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
## Using chaining to aggregate, arrange and create the tidyData.txt file
subData2 <- aggregate(. ~Subject + Activity, subData, mean)%>%
            arrange(Subject,Activity)%>%
            write.table(file="tidyData.txt", row.name = FALSE)


