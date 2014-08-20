#Script containing the analysis for Course project Getting and Cleaning Data
run_analysis <- function() {
  #1: Merge Data
  data <- readAndMergeTrainingTestData()
  
  # Read features and make the feature names better suited for R
  features = read.csv("data/UCI HAR Dataset/features.txt", sep="", header=FALSE)
  features[,2] = gsub('-mean', 'Mean', features[,2])
  features[,2] = gsub('-std', 'Std', features[,2])
  features[,2] = gsub('[-()]', '', features[,2])
  
  # Read activityLabels
  activityLabels = read.csv("data/UCI HAR Dataset/activity_labels.txt", sep="", header=FALSE)
  
  #2: Get only the data on mean and std. dev.
  colsMeanStd <- grep(".*Mean.*|.*Std.*", features[,2])
  # Reduce the features table
  features <- features[colsMeanStd,]
  # Now add the last two columns (activity and subject)
  colsMeanStd <- c(colsMeanStd, 562, 563)
  # And select only the cols we are intersted in
  data <- data[,colsMeanStd]
  
  # Add the column names (features) to data
  colnames(data) <- c(features$V2, "Activity", "Subject")
  colnames(data) <- tolower(colnames(data))
  currentActivity = 1
  for (currentActivityLabel in activityLabels$V2) {
    data$activity <- gsub(currentActivity, currentActivityLabel, data$activity)
    currentActivity <- currentActivity + 1
  }
  data$activity <- as.factor(data$activity)
  data$subject <- as.factor(data$subject)
  tidyDataSet = aggregate(data, by=list(activity = data$activity, subject=data$subject), mean)
  # Remove the activity and subject column, since a mean of those has no use
  tidyDataSet <- tidyDataSet[,1:88]
  write.table(tidyDataSet, "data/UCI HAR Dataset/tidy.txt", sep="\t", row.name=FALSE)
}

#Read's the training and test data files and merges them
readAndMergeTrainingTestData <- function(){
  trainingdata = read.csv("data/UCI HAR Dataset/train/X_train.txt", sep="", header=FALSE)
  columnCount = dim(trainingdata)[2]
  trainingdata[,columnCount+1] = read.csv("data/UCI HAR Dataset/train/y_train.txt", sep="", header=FALSE)
  trainingdata[,columnCount+2] = read.csv("data/UCI HAR Dataset/train/subject_train.txt", sep="", header=FALSE)
  testingdata = read.csv("data/UCI HAR Dataset/test/X_test.txt", sep="", header=FALSE)
  columnCount2 = dim(testingdata)[2]
  testingdata[,columnCount2+1] = read.csv("data/UCI HAR Dataset/test/y_test.txt", sep="", header=FALSE)
  testingdata[,columnCount2+2] = read.csv("data/UCI HAR Dataset/test/subject_test.txt", sep="", header=FALSE)
  # Merge training and test sets if columnCounts are equal!
  if (columnCount == columnCount2){
    data = rbind(trainingdata, testingdata)
    data
  }
  else{
    print("column count from training and test does not match")
  }
}
