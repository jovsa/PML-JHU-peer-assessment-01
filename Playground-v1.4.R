wd <- "C:/Users/jsardinha/Documents/GitHub/PML_JHU_PeerAssessment_01"
setwd(wd)

library(RCurl)
library(knitr)
library(caret)
library(ggplot2)
library(corrplot)
library(randomForest)


# Downloading Training Data
if(!file.exists("./data")){dir.create("./data")}
trainDataURL <- "https://d396quszas40orc.cloudfront.net/predmachlearn/pml-training.csv"
trainFile <- paste("./data/", basename(trainDataURL), sep = "")
download.file(trainDataURL, trainFile, method = "curl")

# Downloading Testing Data
if(!file.exists("./data")){dir.create("./data")}
testDataURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
testFile <- paste("./data/", basename(testDataURL), sep = "")
download.file(testDataURL, testFile, method = "curl")

# Importing Data
trainData <- read.csv(trainFile)
testData <- read.csv(testFile)

# #Adding original values
# trainData$OriginalRows <- row.names(trainData)

##Cleaning the traning data set

#Removing user specific data (provide more concrete reason)
toMatch <- c("X", "user_name", "raw_timestamp_part_1", "raw_timestamp_part_2", "cvtd_timestamp", "new_window", "num_window")
removeColumns <- grep(paste(toMatch,collapse="|"), colnames(trainData))

#Removing Zero Varience Values (provide more specific reason)
nzv <- nearZeroVar(trainData, saveMetrics = TRUE)
removeColumns <- c(removeColumns,which(nzv$nzv == TRUE))

#Removing aggregate data, characterized by NA
AggregateVals <- names(trainData[,colSums(is.na(trainData), na.rm = FALSE) > 0.95*nrow(trainData)])
NAColumns <- grep(paste(AggregateVals,collapse="|"), colnames(trainData))
removeColumns <- c(removeColumns,NAColumns)

#Removind uniques only and sorting them
removeColumns <- unique(removeColumns)
removeColumns <- sort(removeColumns)

#Preparing Tidy Data Sets
trainDataTidy <- trainData[,-removeColumns]
testDataTidy <- testData[,-removeColumns] 


## Preparing CV Data Set
set.seed(112)
inTest <- createDataPartition(y=trainDataTidy$classe,
                               p=0.3, list=FALSE)
training <- trainDataTidy[inTest,] 
crossVal <- trainDataTidy[-inTest,]
# This above split is in line with Andrew Ng

# Storing prediction value column number
classeCol <- grep("classe", colnames(training))

trainCorr <- cor(training[,-classeCol])
trainCorr <- round(trainCorr, digits=2)

#All gyros are very coorelated
corrplot(trainCorr, method = "square", order="hclust", tl.cex=0.55, tl.srt=90)
corrplot(trainCorr, method = "square", order="FPC", tl.cex=0.55, tl.srt=90) # That as usefull as the previous graph

# Note fom APM: When the data set consists of too many predictors to examine visually,
# techniques such as PCA can be used to characterize the magnitude of the
# problem. For example, if the first principal component accounts for a large percentage of the variance, this implies that there is at least one group of predictors
# that represent the same information. This helps reduce computation compleity and increase numerical stabilty.


## Model Selection

# Thigs to test in the future: center &scale&pca and also traincontrol()
# preProc <- preProcess(training[,-classeCol], method = "pca")
# trainProc <- predict(preProc, training[-classeCol])

set.seed(112)
# Enabeling multi-core processing
cl <- makeCluster(detectCores())
registerDoParallel(cl)
modelFit_rf <- train(classe ~ ., data=training, method="rf", prox=TRUE)

# system.time(modelFit_rf <- train(training$classe ~., method = "rf", preProcess = "pca", data = training, prox=TRUE))
confusionMatrix(crossVal$classe, predict(modelFit_rf, crossVal))
confusionMatrix(testDataTidy$classe, predict(modelFit_rf, testDataTidy))
#

set.seed(112)
cl <- makeCluster(detectCores())
registerDoParallel(cl)
modelFit_rf <- train(classe ~ ., data=training, method="rf", preProcess = "pca", prox=TRUE)


modelFit_gbm <- train(classe ~ ., method="gbm",data=training, verbose=FALSE)


# write.csv(training, file = "training.csv")
# write.csv(crossVal, file = "crossVal.csv")
# write.csv(dfTrain, file = "dfTrain.csv")
# write.csv(dfCross, file = "dfCross.csv")



#stopCluster(cl)


