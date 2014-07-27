wd <- "C:/Users/jsardinha/Documents/GitHub/PML_JHU_PeerAssessment_01"
setwd(wd)

library(RCurl)
library(knitr)
library(caret)
library(ggplot2)
library (corrplot)

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
corrplot(trainCorr, order="hclust", tl.cex=0.55, tl.srt=90)
corrplot(trainCorr, order="FPC", tl.cex=0.55, tl.srt=90)











