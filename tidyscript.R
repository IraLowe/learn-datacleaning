library(dplyr)
library(utils)        )
library(reshape2)        
library(plyr)

#read data files
traindata <- read.table("X_train.txt")
trainsubjects <- read.table("subject_train.txt")
traintasks <- read.table("y_train.txt")
testdata <- read.table("X_test.txt")
testsubjects <- read.table("subject_test.txt")
testtasks <- read.table("y_test.txt")
#combine test and training data
totaldata <- rbind(testdata,traindata)
#create and add measure variable columns
varlabels  <- read.table("features.txt", stringsAsFactors = FALSE)
varlabels2 <- varlabels[,2]
colnames(totaldata) <- varlabels2
#Remove columns that do not contain "mean" or "std"
smtestmean <- totaldata[,grep ("mean" , colnames(totaldata))]
smteststd <- totaldata[,grep ("std" , colnames(totaldata))]
totalsubject  <-rbind(testsubjects, trainsubjects)
#create column heading for subject files
colnames(totalsubject) <- "subjects"
# create colums names for activity file
colnames(traintasks) <- 'tasknum'
colnames(testtasks) <- 'tasknum'
totaltasks <- rbind(testtasks, traintasks)
lookup = data.frame( activity = c("WALKING" , "WALKING_UPSTAIRS" , "WALKING_DOWNSTAIRS", "SITTING", "STANDING" ,"LAYING"), tasknum = c(1,2,3,4,5,6))
totaltasks1 <- join(totaltasks,lookup, by= "tasknum")
totaltasks1$activity <- as.character(totaltasks1$activity)
totaltasks2 <- subset(totaltasks1, select = - tasknum)
totaldata_mean_std <- cbind(smtestmean, smteststd)
# combine activity file and subject file to data file
totaldata_subjects <- cbind(totalsubject,totaldata_mean_std)
totaldatafinal <-  cbind(totaltasks2,totaldata_subjects)
#summerise file with mean for each variable by subject and activity
totaldatafinalmean  <- totaldatafinal %>% group_by(activity,subjects) %>% summarise_each(funs(mean))
#create tidy file
Totaltiny <- melt(totaldatafinalmean, 
                                   variable.name = "measure",
                                   value.name = "summary means",
                                    id.vars = c("activity", "subjects"))
write.table(Totaltiny,"Totaltiny.txt", row.names = FALSE)









