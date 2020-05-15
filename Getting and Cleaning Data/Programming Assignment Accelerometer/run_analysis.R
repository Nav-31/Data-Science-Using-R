#placing required files in the same directoriy as the .R file

#loading the activity data
activity_test<-read.table("Y_test.txt",header=FALSE)
activity_train<-read.table("Y_train.txt",header=FALSE)

#loading the subject data
sub_train<-read.table("subject_train.txt",header = FALSE)
sub_test<-read.table("subject_test.txt",header = FALSE)

#loading features data
features_test<-read.table("X_test.txt",header = FALSE)
features_train<-read.table("X_train.txt",header = FALSE)

#merging test and train data by row using rbind()
subject_merged<-rbind(sub_train,sub_test)
activity_merged<-rbind(activity_train,activity_test)
features_merged<-rbind(features_train,features_test)

#setting names
names(subject_merged)<-c("subject")
names(activity_merged)<-c("activity")

features_name<-read.table("features.txt",header = FALSE)
names(features_merged)<- features_name$V2

#merge all the data
data_combine<-cbind(subject_merged,activity_merged)
complete_data<-cbind(data_combine,features_merged)

#filter by names for mean and standard deviation columns
subdataFeaturesNames<-features_name$V2[grep("mean\\(\\)|std\\(\\)", features_name$V2)]
selectedNames<-c(as.character(subdataFeaturesNames), "subject", "activity" )
filtered_data<-subset(complete_data,select=selectedNames)

#labeling activities 
filtered_data$activity <- factor(filtered_data$activity, labels= c("WALKING", "WALKING_UPSTAIRS", "WALKING_DOWNSTAIRS", "SITTING", "STANDING", "LAYING"))

#setting descreptive variable names
names(filtered_data)<-gsub("^t", "time", names(filtered_data))
names(filtered_data)<-gsub("^f", "frequency", names(filtered_data))
names(filtered_data)<-gsub("Acc", "Accelerometer", names(filtered_data))
names(filtered_data)<-gsub("Gyro", "Gyroscope", names(filtered_data))
names(filtered_data)<-gsub("Mag", "Magnitude", names(filtered_data))
names(filtered_data)<-gsub("BodyBody", "Body", names(filtered_data))

#str(filtered_data)

library(plyr)
#forming independent dataset to get aggregate of each subject during each activity
Data_agg<-aggregate(. ~subject + activity, filtered_data, mean)
Data_agg<-Data_agg[order(Data_agg$subject,Data_agg$activity),]

#storing results in a textfile
write.table(Data_agg, file = "tidydata.txt",row.name=FALSE)










