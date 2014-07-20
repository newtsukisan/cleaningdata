# CLEANING DATA ASSIGMENT


# libraries ---------------------------------------------------------------

require (data.table)      # instead of data.frame
require (foreach)         # similar to compressions


# 0. Settings for files ---------------------------------------------------


file.medidas.test     <- "data/test/X_test.txt"          # path of X_test.txt. Section 1
file.medidas.train    <- "data/train/X_train.txt"        # path of X_test.txt. Section 1

file.features.desc    <- "documents/features.txt"        # path of features description. Section 2

file.activity.desc    <- "documents/activity_labels.txt" # path of activity description.  Section 3
file.activity.test    <- "data/test/y_test.txt"          # path of activity test  values. Section 3
file.activity.train   <- "data/train/y_train.txt"        # path of activity train values. Section 3

file.subject.test     <- "data/test/subject_test.txt"    # path of subject test values.  Section 4
file.subject.train    <- "data/train/subject_train.txt"  # path of subject train values. Section 4

file.saving.merging   <- "data/finalData.RData"          # path for saving object. Section 6 and 7

file.saving.solution  <- "data/solution.csv"             # path for saving solution. Section 8



# 1. Loading meassurements data ----------------------------------------------

# Read meassurements from files. We consider test and train.
medidasTest  <- read.table (file.medidas.test,  header = FALSE)
medidasTrain <- read.table (file.medidas.train,header = FALSE)
# view some messaruments
head(medidasTest[,c(1,2)])


# 2. Features Selection ------------------------------------------------------

#  Select features. In this file we can found name of features
claves <- read.table(file.features.desc)
# With this function we select only mean a std desviation features
# feature -> Boolean
# return true if in the text of feature appears mean() or std()
getFeatures   <- function (feature){
  grepl("mean()",feature) | grepl("std()",feature)
}# function to be used in filter
# Select mean and std features. We need name and number row of the feature.
mainFeatures  <- claves[getFeatures(claves$V2),]
# Now we select only mean and std variables. And use wrigth names for columns
testFiltered               <- medidasTest[,mainFeatures$V1]
colnames (testFiltered)    <- mainFeatures$V2
trainFiltered              <- medidasTrain[,mainFeatures$V1]
colnames (trainFiltered )  <- mainFeatures$V2

# Deleting....
rm(medidasTest)
rm(medidasTrain)
gc()


# 3. Activity as factor with rigth labels ------------------------------------

# Get labels from file activity_labels.txt
ActivityLabels  <- read.table(file.activity.desc)
# Now load values of activity from files
testActivity    <- read.table(file.activity.test )
trainActivity   <- read.table(file.activity.train)
# label correctly
testActivity$V1 <- factor(testActivity$V1,
                          levels = c(1:6),
                          labels =ActivityLabels$V2)

trainActivity$V1 <- factor(trainActivity$V1,
                           levels = c(1:6),
                           labels =ActivityLabels$V2)
# testFiltered and trainFiltered will be agregated data.tables
testFiltered$activity  <- testActivity$V1
trainFiltered$activity <- trainActivity$V1
# Deleting.....
rm(testActivity)
rm(trainActivity)
rm(ActivityLabels)
gc()


# 4. Insert subject values ---------------------------------------------------

subjectTest  <- read.table(file.subject.test)
subjectTrain <- read.table(file.subject.train)

# agregate in testFiltered and trainFiltered
testFiltered$subject  <- subjectTest$V1
trainFiltered$subject <- subjectTrain$V1

# Deleting....
rm(subjectTest)
rm(subjectTrain)
gc()


# 5. Merging test data and train data ----------------------------------------

finalData <- rbind(trainFiltered,testFiltered)
# Deleting all objects for cleaning workspace
rm(claves);rm(mainFeatures);rm(testFiltered);rm(trainFiltered)
gc()

# 6. now save for avoid recalculating in worse case... ------------------------
save (finalData, file =file.saving.merging)


# 7. Final agregation ---------------------------------------------------------

load (file.saving.merging)              # Load the data
finalD <- data.table(finalData)         # convert to data.table for operating
# grouping 
class(finalD)                           # check class
keyCol   <- c("activity","subject")     # for merging
# Calculate aggregate values of each variable
meanData <- finalD[,lapply(.SD,mean),by = list(activity,subject)]
stdData  <- finalD[,lapply(.SD,sd),  by = list(activity,subject)]
# setKeys
setkeyv(meanData,keyCol)
setkeyv(meanData,keyCol)
# Now we can merge both tables by keyCol
solution  <- merge(meanData,stdData, by = keyCol)
# .x names of columns comes from mean and .y comes from std.
# change .x -> -mean-values
# change .y -> -std-values,  to make more descriptive
names     <- colnames(solution)
names.len <- length(names)
# Using for each to apply changes
names.1  <- foreach(i = 1:names.len) %do% {
  gsub(".x","-mean-values",names[[i]])
}
names.2  <- foreach(i = 2:names.len) %do% {
    gsub(".y","-std-values",names.1[[i]])
  }
names.2  <- c(names[[1]],unlist(names.2))
# Setnames in data.table, unlist must be used in order to obtain
# character vector for setnames function
setnames(solution,names,names.2) 

# 8. Now we can save as csv this data.table -------------------------------

write.csv (solution, file.saving.solution)
