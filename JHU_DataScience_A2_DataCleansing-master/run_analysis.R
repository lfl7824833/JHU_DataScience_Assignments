featureName<-read.table("./UCI HAR Dataset/features.txt",
                        colClasses = c("character"))

activityName <- read.table("./UCI HAR Dataset/activity_labels.txt",
                           col.names = c("ActivityId", "Activity"))

x_test <-read.table("./UCI HAR Dataset/test/X_test.txt")
y_test <-read.table("./UCI HAR Dataset/test/y_test.txt")
subject_test <-read.table("./UCI HAR Dataset/test/subject_test.txt")

x_train <-read.table("./UCI HAR Dataset/train/X_train.txt")
y_train <-read.table("./UCI HAR Dataset/train/y_train.txt")
subject_train <-read.table("./UCI HAR Dataset/train/subject_train.txt")



# Q1. Merges the training and the test sets to create one data set.
training_sensor_data <- cbind(cbind(x_train, subject_train), y_train)
test_sensor_data <- cbind(cbind(x_test, subject_test), y_test)
sensor_data <- rbind(training_sensor_data, test_sensor_data)

sensor_labels <- rbind(rbind(featureName, c(562, "Subject")), c(563, "ActivityId"))[,2]
names(sensor_data) <- sensor_labels




# Q2. Extracts only the measurements on the mean and standard deviation for each measurement.
sensor_data_mean_std <- sensor_data[,grepl("mean|std|Subject|ActivityId", names(sensor_data))]




# Q3. Uses descriptive activity names to name the activities in the data set
sensor_data_mean_std <- join(sensor_data_mean_std, activityName, by = "ActivityId", match = "first")
sensor_data_mean_std <- sensor_data_mean_std[,-1]



# Q4. Appropriately labels the data set with descriptive names.

names(sensor_data_mean_std) <- gsub('\\(|\\)',"",names(sensor_data_mean_std), perl = TRUE)
names(sensor_data_mean_std) <- make.names(names(sensor_data_mean_std))

names(sensor_data_mean_std) <- gsub('Acc',"Acceleration",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('GyroJerk',"AngularAcceleration",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('Gyro',"AngularSpeed",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('Mag',"Magnitude",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('^t',"TimeDomain.",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('^f',"FrequencyDomain.",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('\\.mean',".Mean",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('\\.std',".StandardDeviation",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('Freq\\.',"Frequency.",names(sensor_data_mean_std))
names(sensor_data_mean_std) <- gsub('Freq$',"Frequency",names(sensor_data_mean_std))


# Q5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

sensor_avg_by_act_sub = ddply(sensor_data_mean_std, c("Subject","ActivityId"), numcolwise(mean))
write.table(sensor_avg_by_act_sub, file = "Tidy data set with the average.txt")
