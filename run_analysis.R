# Getting the dataset and extracting it
if(!file.exists("./data")){dir.create(("./data"))}
if(!file.exists("./data.zip")){
  fileURL <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
  download.file(fileURL, destfile = "./data.zip")
  unzip("./data.zip", exdir = "./data")
}
# Understanding the data
# ======================
# From './data/UCI HAR Dataset/README.txt' lines 17-25w we know what each record should contain, and
# from lines 26-53 we know where to get it from for each dataset.
# For training data is in the following files
# - 'train/X_train.txt' - Training set of a 561-feature vector(mapped in features.txt)
# - 'train/y_train.txt' - Training labels (mapped in activity_labels.txt) for each record
# - 'train/subject_train.txt' - An identifier of the subject for each record (1-30)
# - 'train/Inertial Signals/total_acc_{x,y,z}_train.txt' - total accelerometer data (128 samples)
# - 'train/Inertial Signals/body_acc_{x,y,z}_train.txt' - body accelerometer data (128 samples)
# - 'train/Inertial Signals/body_gyro_{x,y,z}_train.txt' - gyroscope (128 samples)
#
# We only need the data from the first three files, total 7352 records.Each record will have:
# 1: identity of the subject
# 2: activity label
# [3-563]: feature vector (column names from 'feature.txt')

# Load the data files to a usable 
# -------------------------------
# Meta data:
activity_labels <- read.table("data/UCI HAR Dataset/activity_labels.txt")
features <- read.table("data/UCI HAR Dataset/features.txt")
features <- features$V2 # Only the labels
features <- gsub("\\(|\\)", '', features)
features <- gsub("-|,", '_', features) 

# Train set load:
x_train <- read.table("data/UCI HAR Dataset/train/X_train.txt")
y_train <- read.table("data/UCI HAR Dataset/train/y_train.txt")
subject_train <- read.table("data/UCI HAR Dataset/train/subject_train.txt")

# Tidying:
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
colnames(x_train) <- features
x_train <- x_train[,grep("meanFreq", grep("Mean|mean|std", features, value = T), invert = T, value = T)]

# 3. Uses descriptive activity names to name the activities in the data set
y_train_labels <- merge(activity_labels, y_train)
# 4. Appropriately labels the data set with descriptive variable names
train_set <- data.frame( label=y_train_labels$V2, subject=subject_train$V1, id=sub("^", "train", c(1:nrow(x_train))), row.names = 'id')
train_set <- cbind(train_set, x_train)

# Test set load:
x_test <- read.table("data/UCI HAR Dataset/test/X_test.txt")
y_test <- read.table("data/UCI HAR Dataset/test/y_test.txt")
subject_test <- read.table("data/UCI HAR Dataset/test/subject_test.txt")

# Tidying:
# 2. Extracts only the measurements on the mean and standard deviation for each measurement.
colnames(x_test) <- features
x_test <- x_test[,grep("meanFreq", grep("Mean|mean|std", features, value = T), invert = T, value = T)]


# 3. Uses descriptive activity names to name the activities in the data set
y_test_labels <- merge(activity_labels, y_test)
# 4. Appropriately labels the data set with descriptive variable names
test_set <- data.frame( label=y_test_labels$V2, subject=subject_test$V1, id=sub("^", "test", c(1:nrow(x_test))), row.names = 'id')
test_set <- cbind(test_set, x_test)

# 1. Merges the training and the test sets to create one data set
dataset <- rbind(train_set, test_set)
dataset <- dataset[ , !(names(dataset) %in% c('id'))]

# 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
tidy <- aggregate(dataset[-1], by=list(dataset$label, dataset$subject), FUN=mean)
tidy <- tidy[ , !(names(dataset) %in% c('subject'))]
names(tidy)[names(tidy)=="Group.1"] <- "activity"
names(tidy)[names(tidy)=="Group.2"] <- "subject"
write.csv(tidy, file = "tidy.csv")
