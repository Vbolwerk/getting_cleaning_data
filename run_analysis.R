## Clear all and set working directory
rm(list = ls())
setwd(
  "C:/Users/VBolwerk/Documents/sourcetree/datasciencecoursera/getting_cleaning_data/UCI HAR Dataset"
)

install.packages("dplyr")
require("dplyr")

## Load activity labels
actLabels <-  read.table("activity_labels.txt")

## Load features and keep only mean and standard deviation features
features <- read.table("features.txt")
featuresMeanSD <-
  features[grep(".*-mean().*|.*-std().*", features[, 2]), ]
featuresMeanSD$V2 <- as.character(featuresMeanSD$V2)

## Clean feature names - remove special characters
featuresMeanSD$V2 <- gsub("[\\(\\)-]", "", featuresMeanSD$V2)

## Clean feature names - abbreviations
featuresMeanSD$V2 <- gsub("^f", "frequency", featuresMeanSD$V2)
featuresMeanSD$V2 <- gsub("^t", "time", featuresMeanSD$V2)
featuresMeanSD$V2 <- gsub("Acc", "Accelerometer", featuresMeanSD$V2)
featuresMeanSD$V2 <- gsub("Gyro", "Gyroscope", featuresMeanSD$V2)
featuresMeanSD$V2 <- gsub("Mag", "Magnitude", featuresMeanSD$V2)
featuresMeanSD$V2 <- gsub("Freq", "Frequency", featuresMeanSD$V2)
featuresMeanSD$V2 <- gsub("mean", "Mean", featuresMeanSD$V2)
featuresMeanSD$V2 <- gsub("std", "StandardDeviation", featuresMeanSD$V2)


## Function for loading train and test set with the required subselection of features
loadBindFeatures <- function(train_test, features) {
  if (train_test != "train" & train_test != "test") {
    print("Error: not specified whether to read train or test")
    break
  }
  X <- read.table(paste0(train_test,"/X_",train_test,".txt"))[features]
  Y <- read.table(paste0(train_test,"/Y_",train_test,".txt"))
  subject <- read.table(paste0(train_test,"/subject_",train_test,".txt"))
  combined <-cbind(subject, Y, X)
  combined
}

## Combine train and test into a single dataframe called data
train <-loadBindFeatures("train", featuresMeanSD$V1)
test <- loadBindFeatures("test", featuresMeanSD$V1)
data <-rbind(train, test)

## Add column names to data
colnames(data) <- c("subject", "activity", featuresMeanSD$V2)

## Turn activities & subjects into factors
data$activity <- factor(data$activity, levels = actLabels[,1], labels = actLabels[,2])
data$subject <- as.factor(data$subject)

## Create tidy data
tidy_data <- data %>% 
  group_by(subject, activity) %>%
  summarise_each(funs(mean))

write.table(tidy_data, "../tidy_data.txt", row.names = FALSE, quote = FALSE)