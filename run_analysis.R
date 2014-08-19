
run_analysis <- function() {
  #1: Merge Data
  data <- mergeData()
  
  # Read features and make the feature names better suited for R
  features = read.csv("data/UCI HAR Dataset/features.txt", sep="", header=FALSE)
  features[,2] = gsub('-mean', 'Mean', features[,2])
  features[,2] = gsub('-std', 'Std', features[,2])
  features[,2] = gsub('[-()]', '', features[,2])
  
  #2: Get only the data on mean and std. dev.
  colsWeWant <- grep(".*Mean.*|.*Std.*", features[,2])
  # First reduce the features table to what we want
  features <- features[colsWeWant,]
  # Now add the last two columns (subject and activity)
  colsWeWant <- c(colsWeWant, 562, 563)
  # And remove the unwanted columns from allData
  data <- data[,colsWeWant]
  
  # Add the column names (features) to allData
  colnames(data) <- c(features$V2, "Activity", "Subject")
  colnames(data) <- tolower(colnames(data))
  activityLabels = read.csv("data/UCI HAR Dataset/activity_labels.txt", sep="", header=FALSE)
  currentActivity = 1
  for (currentActivityLabel in activityLabels$V2) {
    data$activity <- gsub(currentActivity, currentActivityLabel, data$activity)
    currentActivity <- currentActivity + 1
  }
  data$activity <- as.factor(data$activity)
  data$subject <- as.factor(data$subject)
  tidy = aggregate(data, by=list(activity = data$activity, subject=data$subject), mean)
  # Remove the subject and activity column, since a mean of those has no use
  tidy[,90] = NULL
  tidy[,89] = NULL
  write.table(tidy, "tidy.txt", sep="\t")
}

mergeData <- function(){
  trainingdata = read.csv("data/UCI HAR Dataset/train/X_train.txt", sep="", header=FALSE)
  trainingdata[,562] = read.csv("data/UCI HAR Dataset/train/y_train.txt", sep="", header=FALSE)
  trainingdata[,563] = read.csv("data/UCI HAR Dataset/train/subject_train.txt", sep="", header=FALSE)
  testingdata = read.csv("data/UCI HAR Dataset/test/X_test.txt", sep="", header=FALSE)
  testingdata[,562] = read.csv("data/UCI HAR Dataset/test/y_test.txt", sep="", header=FALSE)
  testingdata[,563] = read.csv("data/UCI HAR Dataset/test/subject_test.txt", sep="", header=FALSE)
  # Merge training and test sets
  allData = rbind(trainingdata, testingdata)
  allData
}
