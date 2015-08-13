# CODEBOOK.md

Getting and Cleaning Data Course Project
----------------------------------------------
----------------------------------------------

Author: **Anastasia**

**Instructions for project:**

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
From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.


**Data Set Information**

The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING UPSTAIRS, WALKING DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data.

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

**Attribute Information**

For each record in the dataset it is provided:

- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration.
- Triaxial Angular velocity from the gyroscope.
-  A 561-feature vector with time and frequency domain variables.
-  Its activity label.
-  An identifier of the subject who carried out the experiment.


The files that will be used to load data For the purposes of this project are listed as follows:

- features.txt     
- activity_labels.txt 
- test/subject_test.txt
- test/X_test.txt
- test/y_test.txt
- train/subject_train.txt
- train/X_train.txt
- train/y_train.txt


**Section 1. Merge the training and the test sets to create one data set**

``` 

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

``` 


**Section 2. Extract only the measurements on the mean and standard deviation for each measurement.**

```

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

```


**Section 3. Use descriptive activity names to name the activities in the data set.**

```
finalData3 = merge(finalData2,activityType,by='activityId',all.x=TRUE);

names(finalData3)
```

**Section 4. Appropriately label the data set with descriptive activity names. **

```
# prefix t is replaced by time
# Acc is replaced by Accelerometer
# Gyro is replaced by Gyroscope
# prefix f is replaced by frequency
# Mag is replaced by Magnitude
# BodyBody is replaced by Body


# Use gsub function for pattern replacement to clean up the data labels.

names(finalData3)<-gsub("^t", "time", names(finalData3))
names(finalData3)<-gsub("^f", "frequency", names(finalData3))
names(finalData3)<-gsub("Acc", "Accelerometer", names(finalData3))
names(finalData3)<-gsub("Gyro", "Gyroscope", names(finalData3))
names(finalData3)<-gsub("Mag", "Magnitude", names(finalData3))
names(finalData3)<-gsub("BodyBody", "Body", names(finalData3))

# Reassigning the new descriptive column names to the finalData set
colNames  = colnames(finalData3)

```

**Section 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject.**


``` 
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

```

The produced tidy data set tidyData.txt a set of variables for each activity and each subject. 10299 instances are split into 180 groups (30 subjects and 6 activities) and 66 mean and standard deviation features are averaged for each group. The resulting data table has 180 rows and 69 columns – 33 Mean variables + 33 Standard deviation variables + 1 Subject( 1 of of the 30 test subjects) + ActivityType + ActivityId . The tidy data set’s first row is the header containing the names for each column.
