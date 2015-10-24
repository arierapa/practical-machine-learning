#load data

training<-read.csv("pml-training.csv",na.strings=c("NA","#DIV/0!",""))
testing<-read.csv("pml-testing.csv",na.strings=c("NA","#DIV/0!",""))

#exploratory
dim(training)
#[1] 19622   160

dim(testing)
#[1]  20 160
 
# 19,622 instances for training with 160 variables, 20 instances for testing

# finding the variables (columns) that have more than 10,000 NA
nac<-colSums(is.na(training))>10000

str(training)

# There are many NAs in some variables

sum(nac)
# [1] 100
# 100 variables where there are more than 10,000 NA (around 50%)

# removing columns (variable) that have more than 10,000 NA
training2<-training[,-which(nac==TRUE)]

# remove first row that do not have information for predictions
training3<-training2[,-1]

#Fit a model
library(caret)
set.seed(32243)

#Data Partitioning
inTrain<-createDataPartition(training3$classe,p=0.6,list=FALSE)

train <- training3[inTrain,]
test<- training3[-inTrain,]

# CART
modelFitRP<-train(classe~.,data=train,method="rpart")
ModelFitRP
# Accuracy = 0.61

predRP<-predict(modelFitRP,newdata=test)
confusionMatrix(predRP,test$classe)
#Accuracy = 0.63

# CART WITH RPART 
library(rpart)

modelFitRP2<- rpart(classe ~ ., data=train, method="class")
predRP2<-predict(modelFitRP2,test,type="class")
confusionMatrix(predRP2,test$classe)
#Accuracy = 0.87


# RANDOM FOREST

library(randomForest)
modelFitRF <- randomForest(classe ~. , data=train)
predRF<-predict(modelFitRF,test,type="class")
confusionMatrix(predRF,test$classe)
#Accuracy = 0.99


# predicting on the testing data set of 20 instances
testing2<-testing[,-which(nac==TRUE)]
testing3<-testing2[,-1]

levels(testing3$cvtd_timestamp) = levels(train$cvtd_timestamp)
levels(testing3$new_window) = levels(train$new_window)

predRPt<-predict(modelFitRF,testing3,type="class")

answers = as.character(predRPt)

answers
# [1] "B" "A" "B" "A" "A" "E" "D" "B" "A" "A" "B" "C" "B" "A" "E" "E" "A" "B" "B"
# [20] "B"

pml_write_files = function(x){
  n = length(x)
  for(i in 1:n){
    filename = paste0("problem_id_",i,".txt")
    write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
  }
}

pml_write_files(answers)


