# NEEDED PACKAGES
library(dplyr)

##################################################################
# GETTING THE DATA
# Download file
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
download.file(fileUrl, "file.zip")
unzip("file.zip")
# Data form test
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt") # 2946 measurements
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt") # activity labels for each measurement
subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt") # subject for each measurement (9 of 30)
# Data form train
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt") # 7351 measurements
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt") # activity labels for each measurement
subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt") # subject for each measurement (21 of 30)
# Accessory info
act_labels <- read.table("./UCI HAR Dataset/activity_labels.txt") # 
features <- read.table("./UCI HAR Dataset/features.txt")

##################################################################
# 1. Merges the training and the test sets to create one data set.
# Merge test+train
X_tot <- rbind(X_test,X_train) # all measurements
y_tot <- rbind(y_test,y_train) # all activity labels
subject_tot <- rbind(subject_test,subject_train) # all subject labels

##################################################################
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# followed instructions to the letter: this includes columns with FreqMean, never followed by relative std column 
statindices <- grep("mean()|std()",features$V2)
X_stat <- X_tot[,grep("mean()|std()",features$V2)]
# in the measurement table X_tot, should add one column for subject and one for activity
X_full <- cbind(subject_tot, y_tot, X_stat)

##################################################################
# 3. Uses descriptive activity names to name the activities in the data set
X_full[,1] <- as.factor(X_full[,1]) # change the subject values to factor (just for tidyness)
X_full[,2] <- as.factor(X_full[,2]) # change the activity values to factor
levels(X_full[,2]) <- as.factor(act_labels[,2]) # change activity levels from 1..6 to the descriptive ones as by file

##################################################################
# 4. Appropriately labels the data set with descriptive variable names.
# Will set the names to the ones from "features.txt", but they must be tidied first
# Removing commas, parenthesis and minuses
tidyindices <- features[statindices,2]
tidyindices %<>%
  gsub("[)]", "",.) %>%
  gsub("[$(]", "",.) %>%
  gsub("[(]", "_",.) %>%
  gsub("[,]", "_",.) %>%
  gsub("[-]", "_",.)
names(X_full)[3:dim(X_full)[2]] <- tidyindices # changing measurement variable names to the tidied "features.txt"
names(X_full)[1:2] <- c("subject", "activity") # names for the 2 new columns tidied

##################################################################
# 5. From the data set in step 4, creates a second, independent tidy data set 
#    with the average of each variable for each activity and each subject.
# grouping by subject and activity and using the function summarise_at to average all the other columns
FinalTable <- X_full %>%
  group_by(subject,activity) %>%
  summarise_at(tidyindices,mean) # passed the tidyindices vector containing the names of all the variables (apart from subject and activity)

# Saving the table on a txt file
write.table(FinalTable,"AssignmentTable.txt", row.name=FALSE)

# Reading the table
# data <- read.table("AssignmentTable.txt", header = TRUE) 
# View(data)