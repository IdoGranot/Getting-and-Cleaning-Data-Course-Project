run_analysis <- function () { 
 
    ## read features data
  
    features <- read.table("features.txt")
    
    ## read test data
    
    y_test <- read.table("test/y_test.txt")
    x_test <- read.table("test/x_test.txt")
    subject_test <- read.table("test/subject_test.txt")
    
    ## read train data
  
    y_train <- read.table("train/y_train.txt")
    x_train <- read.table("train/x_train.txt")
    subject_train <- read.table("train/subject_train.txt") 
    
    ## labels the data set with descriptive variable names
    
    names(x_test) <- features[,2]
    names(y_test) <- ("activity")
    names(subject_test) <- ("subject")
    
    ## bind the data
    
    test_data_bind <- cbind(y_test, subject_test, x_test)
    train_data_bind <- cbind(y_train, subject_train, x_train)
    names(train_data_bind) <- names(test_data_bind) ## names for the train data
    joinedData <- rbind(test_data_bind, train_data_bind)
    
    ## Extracts only the measurements on the mean and standard deviation & 2 first columns
    
    dataRelevant <- joinedData[,grepl("activity|subject|-mean|-std",colnames(joinedData))]
    
    ## Uses descriptive activity names to name the activities in the data set
    
    dataRelevant$action[dataRelevant$activity == "1"] <- "WALKING"
    dataRelevant$action[dataRelevant$activity == "2"] <- "WALKING_UPSTAIRS"
    dataRelevant$action[dataRelevant$activity == "3"] <- "WALKING_DOWNSTAIRS"
    dataRelevant$action[dataRelevant$activity == "4"] <- "SITTING"
    dataRelevant$action[dataRelevant$activity == "5"] <- "STANDING"
    dataRelevant$action[dataRelevant$activity == "6"] <- "LAYING"
    
    dataRelevant <- dataRelevant[,-1] ## delete activity column
    
    dataMelt <- melt(dataRelevant, id = c("action", "subject"))
    
    ##creates a second, independent tidy data set with the average of each variable for each activity and each subject.
  
    dataSummery <- dcast(dataMelt, subject + action ~ variable, mean)
    write.table(dataSummery, "dataSummery.txt",row.name=FALSE)
}