# You should create one R script called run_analysis.R that does the following. 
# 1. Merges the training and the test sets to create one data set.
# 2. Extracts only the measurements on the mean and standard deviation for each measurement. 
# 3. Uses descriptive activity names to name the activities in the data set
# 4. Appropriately labels the data set with descriptive variable names. 
# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.



if (!require("data.table")) {
    install.packages("data.table")
}

if (!require("reshape2")) {
    install.packages("reshape2")
}

require("data.table")
require("reshape2")

#Load Raw data Files

activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")[,2]
features <- read.table("./UCI HAR Dataset/features.txt")[,2]

#Load Test Data
X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("./UCI HAR Dataset/test/y_test.txt")

subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")

# Load Training data 
X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("./UCI HAR Dataset/train/y_train.txt")

subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")

# Apply names from features
names(X_test) = features
names(X_train) = features

# Extract only mean and standard deviation columns.
extract_features <- grepl("mean|std", features)

X_test = X_test[,extract_features]
X_train = X_train[,extract_features]


# Apply activity lables

y_test[,2] = activity_labels[y_test[,1]]
names(y_test) = c("Activity_ID", "Activity")
names(subject_test) = "Subject"

y_train[,2] = activity_labels[y_train[,1]]
names(y_train) = c("Activity_ID", "Activity")
names(subject_train) = "Subject"


# Merge each test and training columns
test_data <- cbind(as.data.table(subject_test), y_test, X_test)

train_data <- cbind(as.data.table(subject_train), y_train, X_train)


# Merge test and train data together
data = rbind(test_data, train_data)


id_labels   = c("Subject", "Activity_ID", "Activity")
data_labels = setdiff(colnames(data), id_labels)
melt_data      = melt(data, id = id_labels, measure.vars = data_labels)



# Apply mean function 
tidy_data   = dcast(melt_data, Subject + Activity ~ variable, mean)
    

## Better column names

col_names <- colnames(tidy_data)
col_names <- gsub("-mean","Mean",col_names)
col_names <- gsub("-std","Std",col_names)
col_names <- gsub("BodyBody","Body",col_names)
col_names <-  gsub("\\(\\)","",col_names)   
col_names <- sub("^t","MeanT",col_names)
col_names <- sub("^f","MeanF",col_names)

colnames(tidy_data) <- col_names
    

## write the output  file
write.table(tidy_data, file = "./tidydata.txt",sep="\t")





