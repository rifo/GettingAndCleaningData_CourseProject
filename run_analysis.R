#########################
# This script contains the function run_analysis
# It performs all the steps required by Course Assignment
# The output are:
# - a txt file named "tidyTable.txt" saved in working directory
# - the requested dataSet as console print
#########################
run_analysis <- function() {
  
  #We will use 2 packages. Check if they are installed.
  packagePlyr <- "plyr"
  if (!require(packagePlyr,character.only=TRUE)) {
      install.packages(packagePlyr)
  }
  require(packagePlyr,character.only=TRUE)  
  
  
  packageReshape2 <- "reshape2"
  if (!require(packageReshape2,character.only=TRUE)) {
      install.packages(packageReshape2)
  }
  require(packageReshape2,character.only=TRUE)  
  
  
  #########################
  #STEP 1: 
  #Merges the training and the test sets to create one data set.
  #########################
  testData <- read.table("UCI HAR Dataset/test/X_test.txt")
  trainData <- read.table("UCI HAR Dataset/train/X_train.txt")
  allData <- rbind(testData, trainData)
  
  #Load features Names and rename column names of merged dataset
  featuresNames <- read.table("UCI HAR Dataset/features.txt",stringsAsFactors=FALSE)[[2]]
  colnames(allData) <- featuresNames
  
  #########################
  #STEP 2: 
  #Extracts only the measurements on the mean and standard deviation for each measurement
  #########################
  selectedData <- allData[,grep("mean|std",featuresNames)]
  
  #########################
  #STEP 3: 
  #Uses descriptive activity names to name the activities in the data set
  #########################
  activityNames <- read.table("UCI HAR Dataset/activity_labels.txt",stringsAsFactors=FALSE)
  colnames(activityNames) <- c("ACTIVITY_ID","ACTIVITY_NAME")
  
  testActivities <- read.table("UCI HAR Dataset/test/y_test.txt",stringsAsFactors=FALSE)
  trainActivities <- read.table("UCI HAR Dataset/train/y_train.txt",stringsAsFactors=FALSE)
  allActivities <- rbind(testActivities, trainActivities)
  
  colnames(allActivities)[1] <- "ACTIVITY_ID"
  
  allActivitiesWithNames <- join(allActivities,activityNames,by="ACTIVITY_ID")
  
  
  #########################
  #STEP 4: 
  #Appropriately labels the data set with descriptive variable names. 
  #########################
  tmpColumnNames = names(selectedData)
  tmpColumnNames <- gsub(pattern="^t",replacement="TIME_",x=tmpColumnNames)
  tmpColumnNames <- gsub(pattern="^f",replacement="FREQUENCY_",x=tmpColumnNames)
  tmpColumnNames <- gsub(pattern="?Gravity?",replacement="GRAVITY",x=tmpColumnNames)
  tmpColumnNames <- gsub(pattern="?Gyro?",replacement="GYROSCOPE",x=tmpColumnNames)
  tmpColumnNames <- gsub(pattern="?Acc?",replacement="ACCELLERATION",x=tmpColumnNames)
  tmpColumnNames <- gsub(pattern="?Mag?",replacement="MAGNITUTE",x=tmpColumnNames)
  tmpColumnNames <- gsub(pattern="?Jerk?",replacement="JERK",x=tmpColumnNames)
  tmpColumnNames <- gsub(pattern="?meanFreq?",replacement="_MEANFREQUENCY_",x=tmpColumnNames)
  tmpColumnNames <- gsub(pattern="?mean?",replacement="_MEAN_",x=tmpColumnNames)
  tmpColumnNames <- gsub(pattern="?std?",replacement="_STANDARDDEVIATION_",x=tmpColumnNames)
  tmpColumnNames <- gsub(pattern="BodyBody",replacement="_BODY_",x=tmpColumnNames)
  tmpColumnNames <- gsub(pattern="Body",replacement="_BODY_",x=tmpColumnNames)
  tmpColumnNames <- gsub(pattern="[()]",replacement="",x=tmpColumnNames)
  tmpColumnNames <- gsub(pattern="[-]",replacement="",x=tmpColumnNames)
  names(selectedData) <- tmpColumnNames
  
  #add column of allActivitiesWithNames (found in STEP 3) to the selectedData dataset
  totDataset <- cbind(ACTIVITY=allActivitiesWithNames[,"ACTIVITY_NAME"],selectedData)
  
  
  
  #########################
  #STEP 5: 
  #From the data set in step 4, creates a second, independent tidy data set 
  #with the average of each variable for each activity and each subject. 
  #########################
  
  #We need to use subject test and train data 
  testSubjects <- read.table("UCI HAR Dataset/test/subject_test.txt",stringsAsFactors=FALSE)
  trainSubjects <- read.table("UCI HAR Dataset/train/subject_train.txt",stringsAsFactors=FALSE)
  allSubjects <- rbind(testSubjects,trainSubjects)
  
  colnames(allSubjects)[1] <- "SUBJECT"
  totDataset <- cbind(allSubjects, totDataset)
  
  sortedDataSet <- totDataset[order(totDataset$SUBJECT,totDataset$ACTIVITY),]
  
  #create a long shaped dataset from a wide shaped dataset
  meltDataSet <- melt(sortedDataSet,id.vars= c("SUBJECT","ACTIVITY"))
  #transformit back into a wide shaped dataset, 
  #aggregating on SUBJECT and ACTIVITY using the mean function
  castDataSet <- dcast(meltDataSet, SUBJECT+ACTIVITY ~ variable, fun.aggregate=mean)
  
  #Save text file (as requested by program assignment)
  write.table(castDataSet,file="tidyTable.txt",row.name=FALSE)

  #Finally, return the totDataset (as requested by program assignment)
  totDataset
}

