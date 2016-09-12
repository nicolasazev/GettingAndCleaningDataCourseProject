library("dplyr")

##### 1ST STEP #####
# load datasets
dataset_train = read.table("UCI HAR Dataset/train/X_train.txt")
label_train = read.table("UCI HAR Dataset/train/y_train.txt")
volunteer_train = read.table("UCI HAR Dataset/train/subject_train.txt")
dataset_test = read.table("UCI HAR Dataset/test/X_test.txt")
label_test = read.table("UCI HAR Dataset/test/y_test.txt")
volunteer_test = read.table("UCI HAR Dataset/test/subject_test.txt")
features = read.table("UCI HAR Dataset/features.txt")
activity_labels = read.table("UCI HAR Dataset/activity_labels.txt")

### add the activity and volunteer column in the datasets
dataset_train$activities = label_train
dataset_train$volunteers = volunteer_train
dataset_test$activities = label_test
dataset_test$volunteers = volunteer_test

### convert the datasets into date frame
df_dataset_train = data.frame(
  matrix(unlist(dataset_train), ncol=length(dataset_train)), 
  stringsAsFactors=F)
df_dataset_test = data.frame(
  matrix(unlist(dataset_test), ncol=length(dataset_test)),
  stringsAsFactors=F)

### merge the train and test datasets by row
data = rbind(df_dataset_train, df_dataset_test)

##### 2ND STEP #####
### get relevant features 
all_features = as.character(features$V2)
all_features[length(all_features)+1] = "activities"
all_features[length(all_features)+1] = "volunteers"

### extract features related with mean, std_dev and activities
relevant_idx_features = grep("mean|std|activities|volunteers", all_features)
relevant_names_features = grep("mean|std|activities|volunteers", all_features, value=T)
tidy_data = select(data, relevant_idx_features)

##### 3RD STEP #####
### change colnames with features name
colnames(tidy_data) = relevant_names_features

##### 4TH STEP #####
## change ids from activities column to descriptive name from activity_labels
tidy_data$activities = sapply(1:length(tidy_data$activities), function(i)
  as.character(c(as.data.frame(t(activity_labels))[2,])[[tidy_data$activities[i]]])
  )

##### 5TH STEP #####
### create another tidy data with the average of each variable for each activity and each subject
# split by volunteers
volunteers_data = split(tidy_data, tidy_data$volunteers)
# split by activities
activities_data = sapply(1:length(volunteers_data), function(i)
  split(volunteers_data[[i]], volunteers_data[[i]]$activities)
  )
# get the means
avg_tidy_data = t(sapply(1:length(activities_data), function(i){
    vars_mean = as.numeric(colMeans(activities_data[[i]][,1:79]))
    vars_mean[length(vars_mean)+1] = activities_data[[i]][1,80]
    vars_mean[length(vars_mean)+1] = activities_data[[i]][1,81]
    vars_mean
  }
))
# changes colnames with features name
colnames(avg_tidy_data) = relevant_names_features
# converts into a dataframe & numeric type
avg_tidy_data = as.data.frame(avg_tidy_data, stringsAsFactors=F)
avg_tidy_data[1:79] = sapply(avg_tidy_data[1:79], as.numeric)
avg_tidy_data[81] = sapply(avg_tidy_data[81], as.numeric)

avg_tidy_data