Describing the code and code book
========================================================

This README.md Markdown document explain the code used for obtaining tidy data in the Assigment of Course:

https://class.coursera.org/getdata-005

Data used are collected from the accelerometers from the Samsung Galaxy S smartphone. A full description is available at the site where the data was obtained: 

http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones 

The data for the analisys can be download from: 

https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip

1. Code
========================================================

Code is commented and divided in sections for clarity. First of all, two libraries are loaded. 

```{r}
require (data.table)               # instead of data.frame
require (foreach)                  # similar to compressions
```

foreah is used to simplify some functional actions. 

Second some path variables are used for simplify using the scripts with differents settings enviroments. These variables are need to be changed in order to use the scripts with files in differents locations. Variable has its own comment to indetify section where can be changed.

```{r}
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
```

Section 1. Load into data.table main corpus of data. 

```{r}
# Read meassurements from files. We consider test and train.
medidasTest  <- read.table (file.medidas.test,  header = FALSE)
medidasTrain <- read.table (file.medidas.train,header = FALSE)
# view some messaruments
head(medidasTest[,c(1,2)])
```

Section 2. Select only mean a std desviation features. Names from file.features.desc, are used for obtaining names of the features. getFeatures uses regular expresion for filtering this features.

```{r}
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
```

Section 3. In this section correct labels for activity are inserted into data. file.activity.desc is used for obtaining the labels. For this purpose is used factor function.

```{r}
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
```

Section 4. Subjects values are inserted.

```{r}
# 4. Insert subject values ---------------------------------------------------

subjectTest  <- read.table(file.subject.test)
subjectTrain <- read.table(file.subject.train)

# agregate in testFiltered and trainFiltered
testFiltered$subject  <- subjectTest$V1
trainFiltered$subject <- subjectTrain$V1
```

Section 5. test and train data are merge.

```{r}
# 5. Merging test data and train data ----------------------------------------

finalData <- rbind(trainFiltered,testFiltered)
# Deleting all objects for cleaning workspace
```

Section 6. In this section temporal result are saved for avoid recalculating or avoid loosing data.

```{r}
# 6. now save for avoid recalculating in worse case... ------------------------
save (finalData, file =file.saving.merging)
```

Section 7. Agregation by subject and activity. After that mean and standard desviation are used for final data.

```{r}
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
```

For clarity purpose names are change to more descriptive ones. activity name is take apart.

```{r}
# .x names of columns comes from mean and .y comes from std.
# change .x -> -mean-values
# change .y -> -std-values,  to make more descriptive
names     <- colnames(solution)
names.len <- length(names)
# Using for each to apply changes
names.1  <- foreach(i = 1:names.len) %do% {
  gsub(".x","-mean-values",names[[i]])
}
# take apart activity name
names.2  <- foreach(i = 2:names.len) %do% {
    gsub(".y","-std-values",names.1[[i]])
  }
# take into activity name
names.2  <- c(names[[1]],unlist(names.2)) 
# Setnames in data.table, unlist must be used in order to obtain
# character vector for setnames function
setnames(solution,names,names.2) 
```
Section 8. Finally results are saved in file.saving.solution
2. Code book
========================================================

Final tidy data set is agregated with subject and activity. Names of variable include mean values and std values indicating which is calculation for aggregation. List of names is:

  [1] "activity"                                                              
  [2] "subject"                                                               
  [3] "tBo-std-valuesAcc-mean()-X-mean-values"                                
  [4] "tBo-std-valuesAcc-mean()-Y-mean-values"                                
  [5] "tBo-std-valuesAcc-mean()-Z-mean-values"                                
  [6] "tBo-std-valuesAcc-std()-X-mean-values"                                 
  [7] "tBo-std-valuesAcc-std()-Y-mean-values"                                 
  [8] "tBo-std-valuesAcc-std()-Z-mean-values"                                 
  [9] "tGravi-std-valuesAcc-mean()-X-mean-values"                             
 [10] "tGravi-std-valuesAcc-mean()-Y-mean-values"                             
 [11] "tGravi-std-valuesAcc-mean()-Z-mean-values"                             
 [12] "tGravi-std-valuesAcc-std()-X-mean-values"                              
 [13] "tGravi-std-valuesAcc-std()-Y-mean-values"                              
 [14] "tGravi-std-valuesAcc-std()-Z-mean-values"                              
 [15] "tBo-std-valuesAccJerk-mean()-X-mean-values"                            
 [16] "tBo-std-valuesAccJerk-mean()-Y-mean-values"                            
 [17] "tBo-std-valuesAccJerk-mean()-Z-mean-values"                            
 [18] "tBo-std-valuesAccJerk-std()-X-mean-values"                             
 [19] "tBo-std-valuesAccJerk-std()-Y-mean-values"                             
 [20] "tBo-std-valuesAccJerk-std()-Z-mean-values"                             
 [21] "tBo-std-values-std-valuesro-mean()-X-mean-values"                      
 [22] "tBo-std-values-std-valuesro-mean()-Y-mean-values"                      
 [23] "tBo-std-values-std-valuesro-mean()-Z-mean-values"                      
 [24] "tBo-std-values-std-valuesro-std()-X-mean-values"                       
 [25] "tBo-std-values-std-valuesro-std()-Y-mean-values"                       
 [26] "tBo-std-values-std-valuesro-std()-Z-mean-values"                       
 [27] "tBo-std-values-std-valuesroJerk-mean()-X-mean-values"                  
 [28] "tBo-std-values-std-valuesroJerk-mean()-Y-mean-values"                  
 [29] "tBo-std-values-std-valuesroJerk-mean()-Z-mean-values"                  
 [30] "tBo-std-values-std-valuesroJerk-std()-X-mean-values"                   
 [31] "tBo-std-values-std-valuesroJerk-std()-Y-mean-values"                   
 [32] "tBo-std-values-std-valuesroJerk-std()-Z-mean-values"                   
 [33] "tBo-std-valuesAccMag-mean()-mean-values"                               
 [34] "tBo-std-valuesAccMag-std()-mean-values"                                
 [35] "tGravi-std-valuesAccMag-mean()-mean-values"                            
 [36] "tGravi-std-valuesAccMag-std()-mean-values"                             
 [37] "tBo-std-valuesAccJerkMag-mean()-mean-values"                           
 [38] "tBo-std-valuesAccJerkMag-std()-mean-values"                            
 [39] "tBo-std-values-std-valuesroMag-mean()-mean-values"                     
 [40] "tBo-std-values-std-valuesroMag-std()-mean-values"                      
 [41] "tBo-std-values-std-valuesroJerkMag-mean()-mean-values"                 
 [42] "tBo-std-values-std-valuesroJerkMag-std()-mean-values"                  
 [43] "fBo-std-valuesAcc-mean()-X-mean-values"                                
 [44] "fBo-std-valuesAcc-mean()-Y-mean-values"                                
 [45] "fBo-std-valuesAcc-mean()-Z-mean-values"                                
 [46] "fBo-std-valuesAcc-std()-X-mean-values"                                 
 [47] "fBo-std-valuesAcc-std()-Y-mean-values"                                 
 [48] "fBo-std-valuesAcc-std()-Z-mean-values"                                 
 [49] "fBo-std-valuesAcc-meanFreq()-X-mean-values"                            
 [50] "fBo-std-valuesAcc-meanFreq()-Y-mean-values"                            
 [51] "fBo-std-valuesAcc-meanFreq()-Z-mean-values"                            
 [52] "fBo-std-valuesAccJerk-mean()-X-mean-values"                            
 [53] "fBo-std-valuesAccJerk-mean()-Y-mean-values"                            
 [54] "fBo-std-valuesAccJerk-mean()-Z-mean-values"                            
 [55] "fBo-std-valuesAccJerk-std()-X-mean-values"                             
 [56] "fBo-std-valuesAccJerk-std()-Y-mean-values"                             
 [57] "fBo-std-valuesAccJerk-std()-Z-mean-values"                             
 [58] "fBo-std-valuesAccJerk-meanFreq()-X-mean-values"                        
 [59] "fBo-std-valuesAccJerk-meanFreq()-Y-mean-values"                        
 [60] "fBo-std-valuesAccJerk-meanFreq()-Z-mean-values"                        
 [61] "fBo-std-values-std-valuesro-mean()-X-mean-values"                      
 [62] "fBo-std-values-std-valuesro-mean()-Y-mean-values"                      
 [63] "fBo-std-values-std-valuesro-mean()-Z-mean-values"                      
 [64] "fBo-std-values-std-valuesro-std()-X-mean-values"                       
 [65] "fBo-std-values-std-valuesro-std()-Y-mean-values"                       
 [66] "fBo-std-values-std-valuesro-std()-Z-mean-values"                       
 [67] "fBo-std-values-std-valuesro-meanFreq()-X-mean-values"                  
 [68] "fBo-std-values-std-valuesro-meanFreq()-Y-mean-values"                  
 [69] "fBo-std-values-std-valuesro-meanFreq()-Z-mean-values"                  
 [70] "fBo-std-valuesAccMag-mean()-mean-values"                               
 [71] "fBo-std-valuesAccMag-std()-mean-values"                                
 [72] "fBo-std-valuesAccMag-meanFreq()-mean-values"                           
 [73] "fBo-std-valuesBo-std-valuesAccJerkMag-mean()-mean-values"              
 [74] "fBo-std-valuesBo-std-valuesAccJerkMag-std()-mean-values"               
 [75] "fBo-std-valuesBo-std-valuesAccJerkMag-meanFreq()-mean-values"          
 [76] "fBo-std-valuesBo-std-values-std-valuesroMag-mean()-mean-values"        
 [77] "fBo-std-valuesBo-std-values-std-valuesroMag-std()-mean-values"         
 [78] "fBo-std-valuesBo-std-values-std-valuesroMag-meanFreq()-mean-values"    
 [79] "fBo-std-valuesBo-std-values-std-valuesroJerkMag-mean()-mean-values"    
 [80] "fBo-std-valuesBo-std-values-std-valuesroJerkMag-std()-mean-values"     
 [81] "fBo-std-valuesBo-std-values-std-valuesroJerkMag-meanFreq()-mean-values"
 [82] "tBo-std-valuesAcc-mean()-X-std-values"                                 
 [83] "tBo-std-valuesAcc-mean()-Y-std-values"                                 
 [84] "tBo-std-valuesAcc-mean()-Z-std-values"                                 
 [85] "tBo-std-valuesAcc-std()-X-std-values"                                  
 [86] "tBo-std-valuesAcc-std()-Y-std-values"                                  
 [87] "tBo-std-valuesAcc-std()-Z-std-values"                                  
 [88] "tGravi-std-valuesAcc-mean()-X-std-values"                              
 [89] "tGravi-std-valuesAcc-mean()-Y-std-values"                              
 [90] "tGravi-std-valuesAcc-mean()-Z-std-values"                              
 [91] "tGravi-std-valuesAcc-std()-X-std-values"                               
 [92] "tGravi-std-valuesAcc-std()-Y-std-values"                               
 [93] "tGravi-std-valuesAcc-std()-Z-std-values"                               
 [94] "tBo-std-valuesAccJerk-mean()-X-std-values"                             
 [95] "tBo-std-valuesAccJerk-mean()-Y-std-values"                             
 [96] "tBo-std-valuesAccJerk-mean()-Z-std-values"                             
 [97] "tBo-std-valuesAccJerk-std()-X-std-values"                              
 [98] "tBo-std-valuesAccJerk-std()-Y-std-values"                              
 [99] "tBo-std-valuesAccJerk-std()-Z-std-values"                              
[100] "tBo-std-values-std-valuesro-mean()-X-std-values"                       
[101] "tBo-std-values-std-valuesro-mean()-Y-std-values"                       
[102] "tBo-std-values-std-valuesro-mean()-Z-std-values"                       
[103] "tBo-std-values-std-valuesro-std()-X-std-values"                        
[104] "tBo-std-values-std-valuesro-std()-Y-std-values"                        
[105] "tBo-std-values-std-valuesro-std()-Z-std-values"                        
[106] "tBo-std-values-std-valuesroJerk-mean()-X-std-values"                   
[107] "tBo-std-values-std-valuesroJerk-mean()-Y-std-values"                   
[108] "tBo-std-values-std-valuesroJerk-mean()-Z-std-values"                   
[109] "tBo-std-values-std-valuesroJerk-std()-X-std-values"                    
[110] "tBo-std-values-std-valuesroJerk-std()-Y-std-values"                    
[111] "tBo-std-values-std-valuesroJerk-std()-Z-std-values"                    
[112] "tBo-std-valuesAccMag-mean()-std-values"                                
[113] "tBo-std-valuesAccMag-std()-std-values"                                 
[114] "tGravi-std-valuesAccMag-mean()-std-values"                             
[115] "tGravi-std-valuesAccMag-std()-std-values"                              
[116] "tBo-std-valuesAccJerkMag-mean()-std-values"                            
[117] "tBo-std-valuesAccJerkMag-std()-std-values"                             
[118] "tBo-std-values-std-valuesroMag-mean()-std-values"                      
[119] "tBo-std-values-std-valuesroMag-std()-std-values"                       
[120] "tBo-std-values-std-valuesroJerkMag-mean()-std-values"                  
[121] "tBo-std-values-std-valuesroJerkMag-std()-std-values"                   
[122] "fBo-std-valuesAcc-mean()-X-std-values"                                 
[123] "fBo-std-valuesAcc-mean()-Y-std-values"                                 
[124] "fBo-std-valuesAcc-mean()-Z-std-values"                                 
[125] "fBo-std-valuesAcc-std()-X-std-values"                                  
[126] "fBo-std-valuesAcc-std()-Y-std-values"                                  
[127] "fBo-std-valuesAcc-std()-Z-std-values"                                  
[128] "fBo-std-valuesAcc-meanFreq()-X-std-values"                             
[129] "fBo-std-valuesAcc-meanFreq()-Y-std-values"                             
[130] "fBo-std-valuesAcc-meanFreq()-Z-std-values"                             
[131] "fBo-std-valuesAccJerk-mean()-X-std-values"                             
[132] "fBo-std-valuesAccJerk-mean()-Y-std-values"                             
[133] "fBo-std-valuesAccJerk-mean()-Z-std-values"                             
[134] "fBo-std-valuesAccJerk-std()-X-std-values"                              
[135] "fBo-std-valuesAccJerk-std()-Y-std-values"                              
[136] "fBo-std-valuesAccJerk-std()-Z-std-values"                              
[137] "fBo-std-valuesAccJerk-meanFreq()-X-std-values"                         
[138] "fBo-std-valuesAccJerk-meanFreq()-Y-std-values"                         
[139] "fBo-std-valuesAccJerk-meanFreq()-Z-std-values"                         
[140] "fBo-std-values-std-valuesro-mean()-X-std-values"                       
[141] "fBo-std-values-std-valuesro-mean()-Y-std-values"                       
[142] "fBo-std-values-std-valuesro-mean()-Z-std-values"                       
[143] "fBo-std-values-std-valuesro-std()-X-std-values"                        
[144] "fBo-std-values-std-valuesro-std()-Y-std-values"                        
[145] "fBo-std-values-std-valuesro-std()-Z-std-values"                        
[146] "fBo-std-values-std-valuesro-meanFreq()-X-std-values"                   
[147] "fBo-std-values-std-valuesro-meanFreq()-Y-std-values"                   
[148] "fBo-std-values-std-valuesro-meanFreq()-Z-std-values"                   
[149] "fBo-std-valuesAccMag-mean()-std-values"                                
[150] "fBo-std-valuesAccMag-std()-std-values"                                 
[151] "fBo-std-valuesAccMag-meanFreq()-std-values"                            
[152] "fBo-std-valuesBo-std-valuesAccJerkMag-mean()-std-values"               
[153] "fBo-std-valuesBo-std-valuesAccJerkMag-std()-std-values"                
[154] "fBo-std-valuesBo-std-valuesAccJerkMag-meanFreq()-std-values"           
[155] "fBo-std-valuesBo-std-values-std-valuesroMag-mean()-std-values"         
[156] "fBo-std-valuesBo-std-values-std-valuesroMag-std()-std-values"          
[157] "fBo-std-valuesBo-std-values-std-valuesroMag-meanFreq()-std-values"     
[158] "fBo-std-valuesBo-std-values-std-valuesroJerkMag-mean()-std-values"     
[159] "fBo-std-valuesBo-std-values-std-valuesroJerkMag-std()-std-values"      
[160] "fBo-std-valuesBo-std-values-std-valuesroJerkMag-meanFreq()-std-values" 

