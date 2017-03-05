library (dplyr)

## Create one R script called run_analysis.R that does the following:
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive activity names.
## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.


##Define these variables on the parent environment; they will be set by the function runAnalysis
training_set <- NULL
test_set <- NULL

##It is recommended to set the working directory prior to run this function!
download_data <- function() {
   ## This is the directory the zip file creates and places its contents
   data_dir <- "UCI HAR Dataset"
   
   url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
   file <- paste(getwd(), "dataset.zip",sep="/")
   ## Download zip file if it do not exist before
   if(!file.exists(file)) {
      print(paste("Downloading zip file to",file))
      download.file(url, file, method = "curl")
   }
   ## if the directory does not exist, then extract the zip file
   if(!file.exists(data_dir)) {
      print(paste("Extracting zip file to",paste(getwd(),sep="/")))
      unzip(file, exdir = ".")
   }
   data_dir <- paste(getwd(), data_dir,sep="/")
   return(data_dir)
}

#Perform the initial download and unzip
data_path <- download_data()

read_dataset <- function(data_path,file) {
   return(read.table(paste(data_path, file,sep="/")))
}

## Here we will load the data sets to the parent environment (hence the double "<<" assignment)
## This way this data will be available for other functions in the parent environment
## Be nice and print some messages while data is being loaded
if (is.null(training_set)) {
   print("Reading training sets...")
   training_set <- read_dataset(data_path,"train/X_train.txt")
   training_set_activity <- read_dataset(data_path,"train/y_train.txt")
   training_set_subjects <- read_dataset(data_path,"train/subject_train.txt")
   names(training_set_subjects) <- "subjects"
   print("Reading training sets complete.")
}
if (is.null(test_set)) {
   print("Reading test sets...")
   test_set <- read_dataset(data_path,"test/X_test.txt")
   test_set_activity <- read_dataset(data_path,"test/y_test.txt")
   test_set_subjects <- read_dataset(data_path,"test/subject_test.txt")
   names(test_set_subjects) <- "subjects"
   print("Reading test sets complete.")
}

## Load activity names
activity_labels <- as.data.frame(read_dataset(data_path,"activity_labels.txt"))

## Read all features (data field labels)
features <- read.table(paste(data_path,"features.txt",sep="/"))[,2]

## 2, focus only on mean and std features, ie mean() and std() - these will be renamed later
focus_features <- as.character(features[grepl("mean\\(\\)|std\\(\\)", features)])

## 4 assign appropriate data labels - will need to select columns too
names(test_set)<-features
names(training_set)<-features

#trim test & trainings datasets to the focus features (mean,std)
test_set <- test_set[,focus_features]
training_set <- training_set[,focus_features]
n_features <- length(focus_features)

## Add new descriptive columns to training and test datasets: activity, subject and data_source
## activity will be loaded from the correspondent activity name from activity_labels.x
## subject is merged (cbind) from subject_[training|test].txt
## data_source is set to "training set" or "test set" (if data is coming from the training_set/test_set)
## be sure to set the names of the columns accordingly, for easy reading

training_set <- cbind(training_set,training_set_activity,training_set_subjects)
#,"training set")
names(training_set)[n_features+1] <- "activity"
names(training_set)[n_features+2] <- "subject"
#names(training_set)[n_features+3] <- "data_source"

test_set <- cbind(test_set,test_set_activity, test_set_subjects)
#,"test set")
names(test_set)[n_features+1] <- "activity"
names(test_set)[n_features+2] <- "subject"
#names(test_set)[n_features+3] <- "data_source"

## Merge both sets
merged_dataset <- rbind(training_set,test_set)

## Remove some large variables we don't need anymore to free up memory
#rm (training_set,training_set_activity,training_set_subjects,test_set,test_set_activity,test_set_subjects)

## Replace activity codes with activity names
for (i in 1:nrow(activity_labels)) {
   merged_dataset$activity[merged_dataset$activity == i] <- as.character(activity_labels[i,2])
}

## 4 We need appropriate names the fields - so let's do some renaming!
names(merged_dataset) <- gsub("^t", "Time", names(merged_dataset))
names(merged_dataset) <- gsub("^f", "Frequency", names(merged_dataset))
names(merged_dataset) <- gsub("BodyBody", "Body", names(merged_dataset)) #typo in the original work??
names(merged_dataset) <- gsub("-mean\\(\\)", "Mean", names(merged_dataset)) # remove thos nasty "()"
names(merged_dataset) <- gsub("-std\\(\\)", "StdDev", names(merged_dataset)) # remove thos nasty "()"

## 5 - create a second tidy data set and save it in the folder
tidy_dataset <- aggregate(. ~subject + activity, merged_dataset, mean)
print("Writing tidy data set...")   
write.table(tidy_dataset, "tidy_dataset.txt",row.names=FALSE)
print("Writing tidy data set complete.") 