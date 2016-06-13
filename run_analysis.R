## Assignment: Getting and Cleaning Data Course Project

library(plyr)
## Assign Working Directory
setwd("C:/R programming/Coursera/Getting and cleaning data/Course Project/UCI HAR Dataset")

## Prepare files to process
Link_to_Features_Set <- ("./features.txt")
Link_to_Activity_Labels_Set <- ("./activity_labels.txt")
Link_to_X_Train_Set <- ("./train/X_train.txt")
Link_to_Y_Train_Set <- ("./train/y_train.txt")
Link_to_Subject_Train_Set <- ("./train/subject_train.txt")
Link_to_X_Test_Set  <- ("./test/X_test.txt")
Link_to_Y_Test_Set  <- ("./test/y_test.txt")
Link_to_Subject_Test_Set <- ("./test/subject_test.txt")

# Load Files to start processing
features <- read.table(Link_to_Features_Set, colClasses = c("character"))
activity_labels <- read.table(Link_to_Activity_Labels_Set, col.names = c("ActivityId", "Activity"))
x_train <- read.table(Link_to_X_Train_Set)
y_train <- read.table(Link_to_Y_Train_Set)
subject_train <- read.table(Link_to_Subject_Train_Set)
x_test <- read.table(Link_to_X_Test_Set)
y_test <- read.table(Link_to_Y_Test_Set)
subject_test <- read.table(Link_to_Subject_Test_Set)

## Question 1/ Merge the training and test sets to create one data set

Training_Data <- cbind(cbind(x_train, subject_train), y_train)
Test_Data <- cbind(cbind(x_test, subject_test), y_test)
Sensor_Data_Merge <- rbind(Training_Data, Test_Data)

# Label columns
names(Sensor_Data_Merge) <- rbind(rbind(features, c(562, "Subject")), c(563, "ActivityId"))[,2]
 
## Question 2/ Extracts only the measurements on the mean and standard deviation for each meansurement

Sensor_Data_Mean_Std <- Sensor_Data_Merge[,grepl("mean|std|Subject|ActivityId", names(Sensor_Data_Merge))]

## Question 3/ Uses descriptive activity names to name the activities in the data set
Sensor_Data_Mean_Std <- join(Sensor_Data_Mean_Std, activity_labels, by = "ActivityId", match = "first")
Sensor_Data_Mean_Std <- Sensor_Data_Mean_Std[,-1]

## Question 4/ Appropriately labels the data set with descriptive variable names.

# Remove parentheses
names(Sensor_Data_Mean_Std) <- gsub('\\(|\\)',"",names(Sensor_Data_Mean_Std), perl = TRUE)
# Make syntactically valid names
names(Sensor_Data_Mean_Std) <- make.names(names(Sensor_Data_Mean_Std))

# More descriptive naming convention
names(Sensor_Data_Mean_Std)

names(Sensor_Data_Mean_Std) <- gsub('Acc',"Acceleration",names(Sensor_Data_Mean_Std))
names(Sensor_Data_Mean_Std) <- gsub('GyroJerk',"AngularAcceleration",names(Sensor_Data_Mean_Std))
names(Sensor_Data_Mean_Std) <- gsub('Gyro',"AngularSpeed",names(Sensor_Data_Mean_Std))
names(Sensor_Data_Mean_Std) <- gsub('Mag',"Magnitude",names(Sensor_Data_Mean_Std))
names(Sensor_Data_Mean_Std) <- gsub('^t',"TimeDomain.",names(Sensor_Data_Mean_Std))
names(Sensor_Data_Mean_Std) <- gsub('^f',"FrequencyDomain.",names(Sensor_Data_Mean_Std))


## Question 5/ From the data set in step 4, creates a second, independent tidy data set with the 
## average of each variable for each activity and each subject.

sensor_avg = ddply(Sensor_Data_Mean_Std, c("Subject","Activity"), numcolwise(mean))

write.table(sensor_avg, file = "sensor_avg.txt")

