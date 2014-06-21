# Mark Ava project on Cursera : June 21st, 2014

library(plyr)   #plyr first
library(dplyr)
library(reshape2)
library(stringr)

# get data, metadata, train and test destinations

# IMPORTANT! place unzipped folder "UCI HAR Dataset" into "wd/../data"
# or use your own path for metadata_src, train_src, test_src, please.

metadata_src <- "../data/UCI HAR Dataset/"

train_src <- "../data/UCI HAR Dataset/train/"

test_src <- "../data/UCI HAR Dataset/test/"

# activity labels and features

# activity labels
activity_labels <- 
  read.table(str_c(metadata_src, "activity_labels.txt"),
             header = FALSE,
             colClasses = c("factor", "character")) %.%
  rename(c("V1" = "activity", "V2" = "activity_descr"))

# features: labels of x_test and x_train variables  
features <-
  read.table(str_c(metadata_src, "features.txt"), 
             header = FALSE,
             colClasses = "factor") %.%
  rename(c("V1" = "feature", "V2" = "feature_descr"))

#--------- train datasets -------------------------------

# individuals
subject_train <- 
  read.table(str_c(train_src, "subject_train.txt"),
             header = FALSE,
             colClasses = "factor") %.%
  rename(c("V1" = "subject"))

# measurements
x_train <- 
  read.table(str_c(train_src, "X_train.txt"),
             header = FALSE,
             colClasses = "numeric") 

# activities
y_train <- 
  read.table(str_c(train_src, "y_train.txt"),
             header = FALSE,
             colClasses = "factor") %.%
  rename(c("V1" = "activity"))

# process train data

# renaming x_train variables
names(x_train) <- features$feature_descr

# y_train labels

y_labels_train <- y_train

#Extracts only the measurements on the mean 
#and standard deviation for each measurement. 

var_select_train <- 
  str_detect(colnames(x_train), fixed("mean()")) |
  str_detect(colnames(x_train), fixed("std()")) 

filtered_x_train <- x_train[, var_select_train]

# column binding subject_train, y_labels_train, filtered_x_train
# and adding column that defines the sample type
subject_y_labels_filtered_x_train <- 
  cbind(subject_train, 
        y_labels_train, 
        filtered_x_train) %.%
  mutate(sample_type = rep("01_train"))

#------------ test datasets ---------------------------

#individuals
subject_test <- 
  read.table(str_c(test_src, "subject_test.txt"),
             header = FALSE,
             colClasses = "factor") %.%
  rename(c("V1" = "subject"))

#measurements
x_test <- 
  read.table(str_c(test_src, "X_test.txt"),
             header = FALSE,
             colClasses = "numeric")

#activities
y_test <- 
  read.table(str_c(test_src, "y_test.txt"),
             header = FALSE,
             colClasses = "factor") %.%
  rename(c("V1" = "activity"))

# process test data

# renaming x_test variables 
names(x_test) <- features$feature_descr

# y_test labels

y_labels_test <- y_test 

# Extracts only the measurements on the mean 
# and standard deviation for each measurement. 

var_select_test <- 
  str_detect(colnames(x_test), fixed("mean()")) | 
  str_detect(colnames(x_test), fixed("std()")) 

filtered_x_test <- x_test[, var_select_test]

# bind columns subject_test, y_labels_test, filtered_x_test
# and adding column that defines the sample type

subject_y_labels_filtered_x_test <- 
  cbind(subject_test, 
        y_labels_test, 
        filtered_x_test) %.%
  mutate(sample_type = rep("02_test")) 

#-------- merging train and test data into tidy_df --------------------
# tidy_df complies to:
# Extracts only the measurements on the mean and standard deviation for 
# each measurement. 
# Uses descriptive activity names to name the activities in the data set
# Appropriately labels the data set with descriptive variable names

tidy_df <- 
  rbind.fill(subject_y_labels_filtered_x_train, 
             subject_y_labels_filtered_x_test) %.%
  merge(activity_labels) %.%
  arrange(sample_type, activity, subject) 

#---- Create the second, independent tidy data set --------------------
# with the average of each variable for each activity and each subject

second_tidy_df <-
  rbind.fill(subject_y_labels_filtered_x_train, 
             subject_y_labels_filtered_x_test) %.%
  melt(id = c("sample_type", "subject", "activity"), variable.name = "feature") %.%
  group_by(sample_type, subject, activity, feature) %.%
  summarise(value = mean(value, na.rm = TRUE)) %.%
  dcast(sample_type + subject + activity ~ feature) %.%
  arrange(sample_type, activity, subject) %.%
  merge(activity_labels) %.%
  melt(id = c("sample_type", "subject", "activity", "activity_descr"), variable.name = "feature") %.%
  dcast(sample_type + subject + activity + activity_descr ~ feature) %.%
  arrange(sample_type, activity, subject)


#----- write the tidy datasets on disk ---------------------------------

write.table(tidy_df, 
            file = "tidy_df.txt")

write.table(second_tidy_df,
            file = "second_tidy_df.txt")
