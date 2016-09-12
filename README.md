Welcome to the Getting and Cleaning Data Course Project!
===================

First of all, you need to download the data in order to run the script. You can find the data in this repo with the directory named "UCI HAR Dataset" or you can download in the link below:

>https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

For more information about the data, see:
>http://archive.ics.uci.edu/ml/datasets/Smartphone-Based+Recognition+of+Human+Activities+and+Postural+Transitions

----------
The run_analysis.R script works in the following manner: 

 1. **Step**:
		- Loads all files used in the script (training/test dataset, training/test labels, training/test volunteers, activity labels, and the list of all features)
		- Adds the volunteers and activities info as a column in the train/test dataset
		- Converts both datasets into a dataframe
		- Merges the train and the test dataset by row into one dataset
 2. **Step**:
        - From the features.txt file, get all measurements and adds the activities and volunteers labels
        - From this list of measurements, extract only those measurements on the mean and standard deviation, besides the activities and volunteers. So filter the data according with those extracted measurements to get a tidy data 
 3. **Step**:
         - Changes the colnames from the tidy dataset with a descriptive variable names from the features.txt file 
 4. **Step**:
         - Uses descriptive activity names from the activity_labels.txt, to name the activities in the tidy dataset
 5. **Step**: 
         - Split the tidy dataset by the volunteers column, to get the measurements separated by them, and stores in a variable called volunteers_data
         - Split the volunteers_data by the activities,  to get each measurement for each activity and each volunteer
         - Then, calculates the mean for each measurement for each activity and each volunteer, and stores in a new variable called avg_tidy_data
         - Changes the colnames from the avg_tidy_data with a descriptive variable names from the features.txt file 
         - Converts the dataset into a dataframe

For more information about the variables generated by the avg_tidy_data see the codebook.md. 
