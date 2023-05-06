### please check the following in comments


if(!dir.exists('./data')) {
  dir.create('./data')
}
### course following - 1. Merges the training and the test sets to create one data set.

data_header <- read.table('./data/UCI HAR Dataset/features.txt', sep = ' ')$V2 #Read the header data for X_dataset
#data_header <- gsub("[-()]", "", data_header) # 헤더에 - 제거

x_test <- read.table('./data/UCI HAR Dataset/test/X_test.txt') # Read X_test_data
x_train <- read.table('./data/UCI HAR Dataset/train/X_train.txt') # x Read X_train_data

x_dataset <- rbind(x_test, x_train) # Merge X_test_data and X_train_data

### course following - 2. Extracts only the measurements on the mean and standard deviation for each measurement.

colnames(x_dataset) <- data_header # Change the table header to data_header(lines:9)

# Extract a dataset consisting of columns with headers including only "mean" and "std" from a table.
extracted_column <- grep("mean\\(|std\\(", colnames(x_dataset))
# Remove strings within table header containing () or -
colnames(x_dataset) <- gsub("[-()]", "", colnames(x_dataset))
# mean_std_dataset is a table consisting of columns composed of mean and standard deviation.
mean_std_dataset<- x_dataset[, extracted_column]

### course following - 3. Uses descriptive activity names to name the activities in the data set
activity_labels <- read.table('./data/UCI HAR Dataset/activity_labels.txt')$V2 # Read index for y_dataset. 

y_test <- read.table('./data/UCI HAR Dataset/test/y_test.txt')$V1 # Read y_test_dataset
y_train <- read.table('./data/UCI HAR Dataset/train/y_train.txt')$V1 # Read y_train_dataset
# merge y_test_dataset and y_train_dataset
y_dataset <- c(y_test, y_train)

#Extract the y dataset with activity_labels values.
y_dataset <- lapply(y_dataset, function(x) {activity_labels[x]}) # y_dataset(ranged 1 to 6) changed to activity_labels

### course following - 4. Appropriately labels the data set with descriptive variable names. 

mean_std_dataset$activityName <- unlist(y_dataset) # activityName column was created in the mean_std_dataset table, and the y_dataset was added to it.

subject_test_data <- read.table('./data/UCI HAR Dataset/test/subject_test.txt')$V1 # Read x_test_set row id
subject_train_data <- read.table('./data/UCI HAR Dataset/train/subject_train.txt')$V1 # Read x_train_set row id 
subject_dataset <- c(subject_test_data, subject_train_data) # Merge two dataset.
mean_std_dataset$subjectId <- factor(subject_dataset) # Make a column "subjectId" upper code of subject dataset and change the factor variables for grouping

# so, i`m ready to group activity and subject. Check the table
View(mean_std_dataset)

### course following - 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.

library(dplyr)
# Group subject, mean each column and save to  subject_group_mean.
subject_group_mean <- mean_std_dataset%>%group_by(subjectId)%>%summarise_all(mean)
View(subject_group_mean)
write.table(subject_group_mean, file='./subject_group_mean.txt', row.names = FALSE)

#also Group activityName, mean each column and save to  activity_group_mean.
activity_group_mean <- aggregate(x = .~activityName,data = mean_std_dataset, FUN = mean)
View(activity_group_mean)
write.table(activity_group_mean, file='./activity_group_mean.txt', row.names = FALSE)
