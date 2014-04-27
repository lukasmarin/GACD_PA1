print("===========UCI HAR Dataset tidy dataset generator===========")

if(!file.exists("./UCI HAR Dataset")){
    print(" Please, ensure that UCI HAR Dataset files are placed in a")
    print(" folder called 'UCI HAR Dataset' in the current directory")
    stop
}

print("Reading activity labels...")
al <- read.table("./UCI HAR Dataset/activity_labels.txt")
#

print("Reading features...")
f <- read.table("./UCI HAR Dataset/features.txt")
f$V2<-as.character(f$V2)

print("Reading test data...")
#Read test subject identifiers
test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
#Read test activities
test <- cbind(test,read.table("./UCI HAR Dataset/test/y_test.txt"))
#Read test measure values
test <- cbind(test,read.table("./UCI HAR Dataset/test/X_test.txt"))

print("Reading train data...")
#Read train subject identifiers
train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
#Read train activities
train <- cbind(train,read.table("./UCI HAR Dataset/train/y_train.txt"))
#Read train measure values
train <- cbind(train,read.table("./UCI HAR Dataset/train/X_train.txt"))

print("Generating tidy data set...")
#Get feature names
fs <- f[,2]
#Get indexes of the mean and std features
i <- grep('-mean\\(\\)|-std\\(\\)',fs)

#Store only identifier, mean and std features data
test <- test[,c(1,2,i+2)]
train <- train[,c(1,2,i+2)]

#Merge data and add column names
data <- rbind(test,train)
names(data) <- c("Subject","Activity",fs[i])
#Delete () from variable names
names(data) <- gsub("\\(\\)","",names(data))

#Add activity labels
data<-(merge(data,al,by.x="Activity",by.y="V1"))

#Clarify activity names and delete redundant column
data$Activity <- gsub("_"," ",paste(toupper(substr(data$V2, 1, 1)), tolower(substr(data$V2, 2, 100000)), sep=""))
data <- data[,!(names(data)=='V2')]
write.table(data,"tidy_data.txt",row.names=FALSE)
print("Generating aggregate (with mean) data set...")
aggs <- aggregate(. ~ Activity + Subject, data = data, mean)
write.table(aggs,"aggregated_data.txt",row.names=FALSE)
