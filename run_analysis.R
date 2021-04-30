################################################################ 
# Data Analysis for Mac. R and data specifications
################################################################ 

# I am using a Macbook Pro
# R version 4.0.5
# Rstudio version 1.2.1335

# The data is from a UCI Machine Learning Repository dataset
# Entitled the "Human Activity Recognition Using Smartphones Data Set":
# http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
# 
# Data:
# https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip  
#  
# I will be using Rstudio for this data analysis. 





################################################################ 
# Uploading and preparing the data for manipulating
################################################################ 

# There are a few methods available to to try to unzip the file directly from the 
# url using R. I have tried to store the url in an object and then use the 
# "unzip" function. For this you might need to install the package "zip":
# install.packages(zip)

zip_url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
unzip(zip_url)

# For me, I got the error message below:
# 
#       Error in unzip(zip_url) : 
#       zip error: `Cannot open zip file `https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip` for reading` in file `zip.c:136`
#       In addition: Warning message:
#       In normalizePath(zipfile) :
#       path[1]="https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip": No such file or directory
#
# That's no good. After several more tries, I just unzipped the file using 
# 'The Unarchiver' on my mac and continued to place the unzipped folder 
# "UCI HAR Dataset" into my working directory where I was able to access it. 
# 
# The folder "UCI HAR Dataset" contains the following .txt data files:,
#                   - UCI HAR Dataset/activity_labels.txt
#                   - UCI HAR Dataset/features.txt
#                   - UCI HAR Dataset/test/subject_test.txt
#                   - UCI HAR Dataset/test/X_test.txt
#                   - UCI HAR Dataset/test/y_test.txt
#                   - UCI HAR Dataset/train/subject_train.txt
#                   - UCI HAR Dataset/train/X_train.txt
#                   - UCI HAR Dataset/train/y_train.txt
#
# Along with this there are README documents that are important to review 
# before beginning. 




################################################################ 
# Working with the pre-unzipped folder in the working directory
################################################################ 

# The document features_info.txt describes these observations as:
#       "The features selected for this database come from the accelerometer 
#       and gyroscope 3-axial raw signals tAcc-XYZ and tGyro-XYZ. These time 
#       domain signals (prefix 't' to denote time) were captured at a constant 
#       rate of 50 Hz."
# There are two columns in feature.txt, so we will name the first "n" for number
# and "signal" for the type of observation, and store it on an object called
# "features". 

features <- read.table("/Applications/Rstudio/Rfiles/UCI HAR Dataset/features.txt", 
                       col.names = c("n","functions"))

# Test the 'features' object to make sure that it works 

features

# Great! It works. 
# Now we can do this with every file in our folder, giving them descriptive names

library(dplyr)

activity <- read.table("/Applications/Rstudio/Rfiles/UCI HAR Dataset/activity_labels.txt", 
                        col.names = c("code", "activity"))
subject_test <- read.table("/Applications/Rstudio/Rfiles/UCI HAR Dataset/test/subject_test.txt", 
                        col.names = "subject")
x_test <- read.table("/Applications/Rstudio/Rfiles/UCI HAR Dataset/test/X_test.txt", 
                        col.names = features$functions)
y_test <- read.table("/Applications/Rstudio/Rfiles/UCI HAR Dataset/test/y_test.txt", 
                        col.names = "code")
subject_train <- read.table("/Applications/Rstudio/Rfiles/UCI HAR Dataset/train/subject_train.txt", 
                        col.names = "subject")
x_train <- read.table("/Applications/Rstudio/Rfiles/UCI HAR Dataset/train/X_train.txt", 
                        col.names = features$functions)
y_train <- read.table("/Applications/Rstudio/Rfiles/UCI HAR Dataset/train/y_train.txt", 
                        col.names = "code")

# Now that all the data are loaded and given proper column names, 
# We can move on to combining them in a way that will help us more. 





################################################################ 
# Step 1: Merging the training and test datasets to create one dataset
################################################################ 

# The datasets are divided into two separate groups for the trainging group
# and the test group. We want to combine them into one dataset, so we will 
# use rbind and cbind to combine them. 
#
# First, we want to combine the x and the y training and test sets using rbind. 
# Then, we want to combine the subject training and test sets using rbind. 
# The y_train and y_test sets contain activity codes that should
# correspond to the observations that are given in x_train, x_test, 
# First let's combine the observation and activity codes into 
# one column.

X_combined <- rbind(x_train, x_test)
Y_combined <- rbind(y_train, y_test)

# Now that all the observations and activities are together, we need 
# to combine them to do the same with the corresponding subject numbers 
# in subject_train and subject_text.

Subjects_combined <- rbind(subject_train, subject_test)

# Now let's add the subject numbers, activity codes, and observations all together

Base_Merged <- cbind(Subjects_combined, Y_combined, X_combined)

# Base_Merged should have a column for the subject, code, and then for 
# each type of feature/signal. 





################################################################ 
# Step 2: Extracting only the measurements on the mean and standard deviation 
# for each measurement
################################################################ 

# For this function, we will need to make sure dplyr is loaded into our system.
# This will allow you to use the '%>%' and 'select' function. 

library(dplyr)

# Now that dplyr is loaded, we can extract the measurements (or observations)
# from Base_Merged by using select() to select the columns we want. We 
# Will select subject and code for clarity. The columns that contain the 
# mean and standard deviation observation have the word "mean" for mean and
# "std" for standard deviation.
# 
# Right now we have 563 columns, that's quite a bit. Extracting only the
# columns with mean or the std will greatly lower our column count. 
# 
# Therefore, we will use contains() arguement in the select() function to
# find all the columns containing the word "mean" and the abbreviation 
# "std". 

Tidy_Data <- Base_Merged %>%
        select (subject, code, contains("mean"), contains("std"))

# Checking Tidy_Data, we have 88 variables now, that's much better. 





################################################################ 
# Step 3: Using descriptive activity names to name the activites in the dataset
################################################################ 

# Let's use the following function to apply the "activity" dataset, which links
# the activity code with the activity name, and use the $ operator to apply 
# the names down the second column. 

Tidy_Data$code <- activity[Tidy_Data$code, 2]





################################################################ 
# Step 4: Appropriately label the dataset with descriptive variable names
################################################################ 

# We want to use more descriptive names for the column headers. 
# The rename() and names() functions will allow us to rename the column headers.
# 
# To rename the second column header from "code" to "activity, simply 
# use rename(data, new = old)


rename(Tidy_Data, "activity" = "code")

# Since there are 88 columnes, using something like rename() will get very
# tedious, so we can rename 

names(Tidy_Data) %<>%
        gsub(pattern = "Acc", replacement = "Accelerometer", names(Tidy_Data)) %>% 
        gsub(pattern = "Gyro", replacement = "Gyroscope", names(Tidy_Data)) %>% 
        gsub(pattern = "BodyBody", replacement = "Body", names(Tidy_Data)) %>%
        gsub(pattern = "^f", replacement = "Frequency", names(Tidy_Data)) %>%
        gsub(pattern = "^t", replacement = "Time", names(Tidy_Data))

# Check the new column names using colnames(Tidy_Data). They should be more 
# descriptive now. 





################################################################  
# Step 5: From the dataset in step 4, creating a second, independent tidy 
# dataset with the average of each variable for each activity and each subject
################################################################ 

# Finally, we can group all the lines by subject and activity using group_by()
# and using the summarise_all() function to calculate the mean of each subject's
# activity. 

Final_Data <- Tidy_Data %>%
        group_by(subject, activity) %>%
        summarise_all(funs(mean))

# Convert to dataframe to write a CSV file.

df <- data.frame(Final_Data)
write.csv(df, "FinalData.csv", row.names = FALSE)


########### END ############
