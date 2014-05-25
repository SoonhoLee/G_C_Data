# 1. Merges the training and the test sets to create one data set.

# setwd("~/Users/¼øÈ£/Documents/GitHub/getting_cleaning_data")

trainData <- read.table("train/X_train.txt")
testData <-read.table("test/X_test.txt")
wholeData <- rbind(trainData, testData)

# Check for Dimension
dim(wholeData) # 10299*561 is true value

# Check for headline data
head(wholeData)

trainLabel <- read.table("train/y_train.txt")
testLabel <-read.table("test/y_test.txt")
wholeLabel <- rbind(trainLabel, testLabel)

# Check for Dimension
dim(wholeLabel) # 10299*1 is true value

trainsubject <- read.table("train/subject_train.txt")
testsubject <-read.table("test/subject_test.txt")
wholesubject <- rbind(trainsubject, testsubject)

# Check for Dimension
dim(wholesubject) # 10299*1 is true value

# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 

features <- read.table("features.txt")
index <- grep("mean\\(\\)|std\\(\\)", features[, 2])
wholeData <- wholeData[,index]

# 3. Uses descriptive activity names to name the activities in the data set

activity <- read.table("activity_labels.txt")
activityname <- activity[wholeLabel[,1],2]
wholeLabel[,1] <- activityname


# 4. Appropriately labels the data set with descriptive activity names. 

names(wholesubject) <- "subject"
names(wholeLabel) <- "activity"
AllData <- cbind(wholesubject, wholeLabel, wholeData)
write.table(AllData, "mergedData.txt") 

# 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject. 

subjectLen <- length(table(wholesubject)) 
activityLen <- dim(activity)[1] 
columnLen <- dim(AllData)[2]

result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 
result <- as.data.frame(result)

colnames(result) <- colnames(AllData)

row <- 1

for(i in 1:subjectLen) {
    for(j in 1:activityLen) {

        result[row, 1] <- sort(unique(wholesubject)[, 1])[i]
        result[row, 2] <- activity[j, 2]

        bool1 <- i == AllData$subject

        bool2 <- activity[j, 2] == AllData$activity

        result[row, 3:columnLen] <- colMeans(AllData[bool1&bool2, 3:columnLen])

        row <- row + 1
    }
}

write.table(result, "result.txt")


