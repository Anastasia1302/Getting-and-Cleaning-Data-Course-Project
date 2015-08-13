# Creation of run_analysis.R

# Clean up workspace
rm(list=ls())

# 0. Get the data

#set working directory
setwd("~/Dropbox/Coursera/getdata/project")

# downlad the data
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl,destfile="./Dataset.zip",method="curl")

# unzip the file
unzip(zipfile="./Dataset.zip",exdir="./")

# get list of files 
path_rf <- file.path("./", "UCI HAR Dataset")
files<-list.files(path_rf, recursive=TRUE)
files

# The files that will be used to load data For the purposes of this project are listed as follows:

# features.txt     
# activity_labels.txt 
# test/subject_test.txt
# test/X_test.txt
# test/y_test.txt
# train/subject_train.txt
# train/X_train.txt
# train/y_train.txt


# 1. Merge the training and the test sets to create one data set.

#set working directory to the location where the UCI HAR Dataset was unzipped
setwd("~/Dropbox/Coursera/getdata/project/UCI HAR Dataset/");

# Read in train data
features     = read.table('./features.txt',header=FALSE); #imports features.txt
activityType = read.table('./activity_labels.txt',header=FALSE); #imports activity_labels.txt
subjectTrain = read.table('./train/subject_train.txt',header=FALSE); #imports subject_train.txt
xTrain       = read.table('./train/x_train.txt',header=FALSE); #imports x_train.txt
yTrain       = read.table('./train/y_train.txt',header=FALSE); #imports y_train.txt


# Assigin column names to the data imported above
colnames(activityType)  = c('activityId','activityType');
colnames(subjectTrain)  = "subjectId";
colnames(xTrain)        = features[,2]; 
colnames(yTrain)        = "activityId"


# cCreate the final training set by merging yTrain, subjectTrain, and xTrain
trainingData = cbind(yTrain,subjectTrain,xTrain);


# Read in the test data
subjectTest = read.table('./test/subject_test.txt',header=FALSE); #imports subject_test.txt
xTest       = read.table('./test/x_test.txt',header=FALSE); #imports x_test.txt
yTest       = read.table('./test/y_test.txt',header=FALSE); #imports y_test.txt


# Assign column names to the test data imported above
colnames(subjectTest) = "subjectId";
colnames(xTest)       = features[,2]; 
colnames(yTest)       = "activityId"


# Create the final test set by merging the xTest, yTest and subjectTest data
testData = cbind(yTest,subjectTest,xTest);


# Combine training and test data to create a final data set
finalData = rbind(trainingData,testData);

# Create a vector for the column names from the finalData, which will be used
# to select the desired mean() & stddev() columns
colNames  = colnames(finalData); 


# 2. Extract only the measurements on the mean and standard deviation, 
# i.e taken Names of Features with “mean()” or “std()”

# Create a logicalVector that contains names of ID variables
logicalVector = (grepl("activity..",colNames) | grepl("subject..",colNames) )
colNames1 = colNames[logicalVector==TRUE]

# Create a vector of names that contains means and stddev columns 
colNames2<- grep("mean\\(\\)|std\\(\\)",colNames,value=TRUE)
colNames3<- grep("activity..",colNames | grep("subject..",colNames))

# Create a vector of names that contains names of ID variables and the names of columns with means and stddev variables
colNames3<- c(colNames1, colNames2)

# Subset finalData table based on the logicalVector to keep only desired columns
finalData2 = finalData[colNames3];
str(finalData2)


# 3. Use descriptive activity names to name the activities in the data set

# Merge the finalData set with the acitivityType table to include descriptive activity names
finalData3 = merge(finalData2,activityType,by='activityId',all.x=TRUE);

names(finalData3)

# prefix t is replaced by time
# Acc is replaced by Accelerometer
# Gyro is replaced by Gyroscope
# prefix f is replaced by frequency
# Mag is replaced by Magnitude
# BodyBody is replaced by Body

names(finalData3)<-gsub("^t", "time", names(finalData3))
names(finalData3)<-gsub("^f", "frequency", names(finalData3))
names(finalData3)<-gsub("Acc", "Accelerometer", names(finalData3))
names(finalData3)<-gsub("Gyro", "Gyroscope", names(finalData3))
names(finalData3)<-gsub("Mag", "Magnitude", names(finalData3))
names(finalData3)<-gsub("BodyBody", "Body", names(finalData3))

# Reassigning the new descriptive column names to the finalData set
colNames  = colnames(finalData3)

# 5. Create second, independent tidy data set with the average of each variable for each activity and each subject. 

# Create a new table, finalDataNoActivityType without the activityType column
finalDataNoActivityType  = finalData3[,names(finalData3) != 'activityType'];

head(finalDataNoActivityType)

# Summarizing the finalDataNoActivityType table to include just the mean of each variable for each activity and each subject
tidyData    = aggregate(finalDataNoActivityType[,names(finalDataNoActivityType) != c('activityId','subjectId')],by=list(activityId=finalDataNoActivityType$activityId, subjectId = finalDataNoActivityType$subjectId),mean);

# Merging the tidyData with activityType to include descriptive acitvity names
tidyData    = merge(tidyData,activityType,by='activityId',all.x=TRUE);

# Export the tidyData set 
setwd("~/Dropbox/Coursera/getdata/project")
write.table(tidyData, './tidyData.txt',row.names=FALSE,sep='\t')

names(tidyData)

