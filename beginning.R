library(caret)
library(e1071) # needed in "predict"
library(randomForest) # needed in "train"
set.seed(647)



pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

data <- read.csv("pml-training.csv",na.strings=c("NA",""))
testData <- read.csv("pml-testing.csv",na.strings=c("NA",""))

dumbParameters <- c("X","user_name","raw_timestamp_part_1","raw_timestamp_part_2","cvtd_timestamp","new_window","num_window")

indicesOfDumbParameters <- which(names(data) %in% dumbParameters)
strippedData <- data[-indicesOfDumbParameters]

indicesForTraining <- createDataPartition(y=strippedData$classe,p=0.2,list=FALSE)
training <- strippedData[indicesForTraining,]
internalTesting <- strippedData[-indicesForTraining,]

okData <- which(apply(!is.na(training),2,sum)==length(training[,1]))
actualTraining <- training[,okData]

model <- train(classe~.,data=actualTraining,method="rf",trControl=trainControl(method="cv",number=5),prox=TRUE,allowParallel=TRUE)
print(model)

actualInternalTesting <- internalTesting[,okData]

ans <- predict(model,newdata=actualInternalTesting)
print(confusionMatrix(actualInternalTesting$classe,ans))

strippedTestData <- testData[-indicesOfDumbParameters]
actualTesting <- strippedTestData[,okData]

ans <- predict(model,newdata=actualTesting)
print(ans)
pml_write_files(ans)
