wd <- "C:/Users/jsardinha/Documents/GitHub/PML_JHU_PeerAssessment_01"
setwd(wd)

library(RCurl)
library(knitr)
library(caret)
library(ggplot2)
library (corrplot)
library(doParallel)

# Downloading Training Data
if(!file.exists("./data")){dir.create("./data")}
trainDataURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
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

#Removind Uniques only
removeColumns <- unique(removeColumns)

#Preparing Tidy Data Sets
trainDataTidy <- trainData[,-removeColumns]
testDataTidy <- testData[,-removeColumns] # Try to see if this works without removing cols


## Preparing CV Data Set
set.seed(1123)
inCV <- createDataPartition(y=trainDataTidy$classe,
                               p=0.6, list=FALSE)
training <- trainDataTidy[inCV,] # 59.95% of total data
crossVal <- trainDataTidy[-inCV,] # 39.95% of total data
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


# Storing the defauly par()
defPar <- par()


## System Settings
c5 <- makeCluster(5)  # Use 5 CPU Cores
registerDoParallel(c5)
getDoParWorkers() # Verify how many cores will be used.


## Model Selection

# Thigs to test in the future: center &scale&pca and also traincontrol()
modelFit_rf <- train(classe ~., method = "rf", preProcess = "pca", data = training, prox=TRUE)
confusionMatrix(crossVal$classe, predict(modelFit_rf, testing2))
#





stopCluster(c5)



#### BIN
modelFit <- train(training2$diagnosis ~., method = "glm", data = trainPC)


# Method 1
training2 <- training[,c(1,grep("^IL", colnames(training)))]
testing2 <- testing[,c(1,grep("^IL", colnames(testing)))]
preProc <- preProcess(training2[,-1], method = "pca", thresh = 0.90)
trainPC <- predict(preProc, training2[-1])
modelFit <- train(training2$diagnosis ~., method = "glm", data = trainPC)
testPC <- predict(preProc, testing2[,-1])
confusionMatrix(testing2$diagnosis, predict(modelFit, testPC))

#Method 2

# Note here that in this model that preProcess ="pca" will use a tresh = 0.9
modelFit <- train(training2$diagnosis ~., method = "glm", preProcess = "pca", data = training2)
confusionMatrix(testing2$diagnosis, predict(modelFit, testing2))
