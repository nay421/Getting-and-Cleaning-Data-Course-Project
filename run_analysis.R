#1. Merges the training and the test sets to create one data set.

#setwd("~/Coursera/Getting & cleaning data/Assignment/UCI HAR Dataset")

trainData <- read.table ('./train/X_train.txt')

trainLabel <- read.table ('./train/y_train.txt')

trainSubject <- read.table ('./train/subject_train.txt')

testData <- read.table ('./test/X_test.txt')

testLabel <- read.table ('./test/y_test.txt')

testSubject <- read.table ('./test/subject_test.txt')

mergeddata <- rbind(trainData, testData)

mergedlabel <- rbind(trainLabel, testLabel)

mergedsubject <- rbind(trainSubject, testSubject)

#2. Extracts only the measurements on the mean and standard deviation for each measurement.

measurements <- read.table ('./features.txt')

meansd <- grep('mean\\(\\)|std\\(\\)', measurements[,2])

mergeddata <- mergeddata[,meansd]

names(mergeddata) <- gsub ('\\(\\)', '', measurements[meansd,2])
names (mergeddata) <- gsub ('mean', 'Mean', names(mergeddata))
names (mergeddata) <- gsub ('std', 'SD', names(mergeddata))
names(mergeddata) <- gsub ('-', '', names(mergeddata))

#3. Uses descriptive activity names to name the activities in the data set

activities <- read.table('./activity_labels.txt')

activities [,2]<- tolower(gsub('_','',activities[,2]))

substr(activities[2, 2], 8, 8) <- toupper(substr(activities[2, 2], 8, 8))
substr(activities[3, 2], 8, 8) <- toupper(substr(activities[3, 2], 8, 8))
activityLabel <- activities[mergedlabel[, 1], 2]
mergedlabel[, 1] <- activityLabel
names(mergedlabel) <- "activities"


#4. Appropriately labels the data set with descriptive variable names.

names(mergedsubject) <- "subject"
cleandata <- cbind(mergedsubject, mergedlabel, mergeddata)
write.table(cleandata, "merged_data.txt") 

#5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

subjectLen <- length(table(mergedsubject))
activityLen <- dim(activities)[1] 
columnLen <- dim(cleandata)[2]
result <- matrix(NA, nrow=subjectLen*activityLen, ncol=columnLen) 
result <- as.data.frame(result)
colnames(result) <- colnames(cleandata)
row <- 1
for(i in 1:subjectLen) {
    for(j in 1:activityLen) {
        result[row, 1] <- sort(unique(mergedsubject)[, 1])[i]
        result[row, 2] <- activities[j, 2]
        bool1 <- i == cleandata$subject
        bool2 <- activities[j, 2] == cleandata$activities
        result[row, 3:columnLen] <- colMeans(cleandata[bool1&bool2, 3:columnLen])
        row <- row + 1
    }
}

write.table(result, "tidy_means.txt")

