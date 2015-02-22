
##Load Libraries
if (!require("caret")) {
  install.packages("caret")
}

if (!require("rattle")) {
  install.packages("rattle")
}

if (!require("rpart")) {
  install.packages("rpart")
}

if (!require("randomForest")) {
  install.packages("randomForest")
}

if (!require("e1071")) {
  install.packages("e1071")
}


require("caret")
require("rattle")
require("rpart")
require("randomForest")
require("e1071")

library(caret)
library(rattle)
library(rpart)
library(randomForest)
library(e1071)

##Set Seed 
set.seed(12345)


## Reading Data

#train and test files URL:
##urlTrain <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
##urlTest <- "http://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"


##dataTrain <- read.csv(url(urlTrain), na.strings=c("NA","#DIV/0!",""))
##dataTest <- read.csv(url(urlTest), na.strings=c("NA","#DIV/0!",""))

dataTrain <- read.csv(file.path(getwd(), "pml-training.csv"), na.strings=c("NA","#DIV/0!",""))
dataTest <- read.csv(file.path(getwd(), "pml-testing.csv"), na.strings=c("NA","#DIV/0!",""))

##Partioning training data set into:
##  60% for interal training (intTrain)
##  40% for interal testing (intTest):
dataPartTrain <- createDataPartition(y=dataTrain$classe, p=0.6, list=FALSE)
intTrain <- dataTrain[dataPartTrain, ]
intTest <- dataTrain[-dataPartTrain, ]


##Cleaning the data

##Generating list of NVZ columns
intDataNZV <- nearZeroVar(dataTrain, saveMetrics=TRUE)
intDataNZVList <- intDataNZV[intDataNZV$nzv,]

##Removing the NVZ columns from data:
intNZVColumns <- names(intTrain) %in% rownames(intDataNZVList)
intTrain <- intTrain[!intNZVColumns]

#Removing first column (ID) so that it does not interfer with ML Algorithms:
intTrain$X <- NULL

##Remove columns that have too many NA values (more than a threshold of 60%)
intTrain <- intTrain[,colSums(is.na(intTrain)) < (nrow(intTrain)*0.6)]

##Remove columns in test data that do not exist in training data
intTest <- intTest[, names(intTest) %in% names(intTrain)]
dataTest <- dataTest[, names(dataTest) %in% names(intTrain)]

##"Syncronize" the data type/category in the data and initTrain data frames.
dataTest$classe <- 0
dataTest <- rbind(intTrain[1,] , dataTest)
dataTest <- dataTest[-1,]
dataTest$classe <- NULL

## Machine Learning - Decision Tree

modFitDT <- rpart(classe ~ ., data=intTrain, method="class")
predictionsDT <- predict(modFitDT, intTest, type = "class")

##Test Results of Decision Tree
confusionMatrix(predictionsDT, intTest$classe)


## Machine Learning -  Random Forests

modFitRF <- randomForest(classe ~. , data=intTrain)
predictionsRF <- predict(modFitRF, intTest, type = "class")

##Test Results of Random Forests
confusionMatrix(predictionsRF, intTest$classe)

##Generating Files to submit as answers for the Assignment:

##Prediction for Assignment Test Data Using Decision Tree
predictionsDT <- predict(modFitDT, dataTest, type = "class")

#Prediction for Assignment Test Data Using Random Forests
predictionsRF <- predict(modFitRF, dataTest, type = "class")

#Function to generate files with predictions to submit for assignment:

##Provided by Assignment to Generate Solutions Files for Uploading
pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(predictionsRF)

