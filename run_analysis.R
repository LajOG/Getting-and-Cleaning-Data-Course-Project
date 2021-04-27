# runanalyis.R
# created to make tidy table of means and standard deviations out of
# test and train dataset provded for the exercise.  Source Dataset is referenced
# README.md and Codebook.md

# check if data folder exists, if not, creates it

if (!dir.exists("./data")){ 
        dir.create("./data")
}

# downlads and unzips the source files in the data directory 
setwd("./data")
fileUrl <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
temp <- tempfile()
download.file(fileUrl,temp)
unzip(temp)
unlink(temp)
setwd("..")


# reads data from test and train files for measures(x), activities(y) and subjects (S)
xtest <- read.table("./data/UCI HAR Dataset/test/X_test.txt")
xtrain<- read.table("./data/UCI HAR Dataset/train/X_train.txt")
ytest <- read.table("./data/UCI HAR Dataset/test/y_test.txt")
ytrain <-read.table("./data/UCI HAR Dataset/train/y_train.txt")
stest <- read.table("./data/UCI HAR Dataset/test/Subject_test.txt")
strain <- read.table("./data/UCI HAR Dataset/train/Subject_train.txt")
features <- read.table("./data/UCI HAR Dataset/features.txt")
activity_labels <- read.table("./data/UCI HAR Dataset/activity_labels.txt")



#merge test and train data for the measures and label the columns
datax <- rbind(xtest,xtrain)
names(datax) <-features[,2]

#keep the mean and std() - I think they come in pairs, so I only kept the activities whose 
#means had a corresponding std() after looking through the list of 
#activities and ignored the meanFreq() etc
datax <- datax[which(grepl("mean[()]",names(datax))|grepl("std[()]",names(datax)))]

#append information on subject and activity
datay <- rbind(ytest,ytrain)
datas <- rbind(stest,strain)
dataset <- cbind(datas,datay,datax)

# read features and activity_labels

names(dataset)[1] <- "Participant" 
names(dataset)[2] <- "Activity"

# name the activities and remove the activities numbers
mdataset <- merge(dataset, activity_labels, by.x="Activity", by.y = "V1") 
mdataset <-subset(mdataset, select = -Activity)


#reorganize the columns so that the activities are closer 
# to the participants and give the activity column a proper name
mdataset <- mdataset[,c(1,68,2:67)]
mdataset
names(mdataset)[2] <- "Activity"


# we have a tidy wide form in mdataset with each row and column 
# intersecting at one  result with no duplicates.  
# use tidyr and dplyr libraries to create a tidy long form dataset
# and calculate the means of the means and the means of the 
# standard deviations.
library(dplyr)
library(tidyr)

calcdataset <- as_tibble(mdataset)

# create a tidy long dataset with a new column Measures to accommodate
# all the readings
calcdataset_long <- pivot_longer(calcdataset,
                 names(calcdataset[,3:68]),names_to =  "Measures")


x<-     calcdataset_long %>%
        group_by(Participant,Activity,Measures) %>%
        summarize(across(everything(),mean),.groups="keep") %>%
        print

names(x)[4] <- "MeasuresMean"
# write the result of the tidy means and averages to the output

write.table(x,"tidyresult.txt",row.name=FALSE)
        
        
                
