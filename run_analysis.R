# This script was written to do the following exercise about cleaning data:
## To merge the training and the test sets to create one data set.
## To extract only the measurements on the mean and standard deviation for each 
### measurement.
## To use descriptive activity names to name the activities in the data set
## To Appropriately label the data set with descriptive variable names.
## To create a second, independent tidy data set with the average of each 
### variable for each activity and each subject, From the data set in last step

## Reading the training and the test sets.
library(dplyr)
library(readr)
library(reshape2)
home_path <- "UCI HAR Dataset/"
train_path <- paste0(home_path,"train/")
X_train <- read_table2(paste0(train_path,"X_train.txt"), col_names = F)
nrowstrain<-nrow(X_train)
ncolstrain<-ncol(X_train)
y_train <- read_table2(paste0(train_path,"y_train.txt"), col_names = F)#activity
subject_train <- read_table2(paste0(train_path,"subject_train.txt"), col_names = F)#subject
X_train <- X_train %>%
    mutate(y=y_train$X1, subject=subject_train$X1)

test_path<-paste0(home_path,"test/")
X_test <- read_table2(paste0(test_path,"X_test.txt"), col_names = F)
nrowstest<-nrow(X_test)
y_test <- read_table2(paste0(test_path,"y_test.txt"), col_names = F)
subject_test <- read_table2(paste0(test_path,"subject_test.txt"), col_names = F)
X_test <- X_test %>%
    mutate(y=y_test$X1, subject=subject_test$X1)

## Merging the training and the test sets to create one data set.

X_all <- full_join(X_train,X_test)
#X_all <- X_all %>% mutate(test_set=c(rep(0,nrowstrain),rep(1,nrowstest)))

## Extracting only the measurements on the mean and standard deviation.

features <- read_table2(paste0(home_path,"features.txt"), col_names = F)
myVectorOfStrings <- c("mean()", "std()")
matchExpression <- paste(myVectorOfStrings, collapse = "|")
featuressubset<-filter(features,grepl(matchExpression,X2))
featuressubset <- featuressubset %>%
    filter(!grepl("meanFreq()",X2))
nvars<-nrow(featuressubset)
varnames<-c(featuressubset$X2)
varnames<-gsub("-","_",varnames)#gsub since it can be >1 time
varnames<-gsub("()","",varnames)#sub since it is only one time
varnames<-sub("\\(","",varnames)#sub since it is only one time
varnames<-gsub(")","",varnames)#sub since it is only one time
varnames<-gsub(",","",varnames)#sub since it is only one time
featuressubset$X2<-varnames
X_all_subset <- X_all %>%
    select(c(featuressubset$X1,(ncolstrain+1):ncol(X_all))) 

## Using descriptive activity names to name the activities in the data set

activity_labels <- read_table2(paste0(home_path,"activity_labels.txt"), col_names = F)
X_all_subset$y<-factor(X_all_subset$y,labels=activity_labels$X2)

## Labelling appropriately the data set with descriptive variable names.

colnames(X_all_subset)[1:nvars]<-featuressubset$X2
X_all_subset <- X_all_subset %>%
    rename(activity = y)
    
## Creating a second, independent tidy data set with the average of each 
### variable for each activity and each subject, From the data set in last step

melted <- melt(X_all_subset, id.vars=c("activity", "subject"))

X_average <- melted %>%
    group_by(activity, subject, variable) %>%
    summarise(mean=mean(value))

# Output
write.table(X_average,"X_average.txt",sep=" ",dec=".", col.names=F, na="", row.names=F)

# Cleaning workspace
rm(list=ls())