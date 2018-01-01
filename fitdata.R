require(caret)
require(dplyr)
require(reshape2)
require(ggplot2)
require(ggfortify)

train <- read.table("pml-training.csv", sep = ",", header = TRUE)
test <- read.table("pml-testing.csv", sep = ",", header = TRUE)

train.f1 <- train[, c(8, 9, 10, 38, 39, 40, 41, 42, 43, 44, 
                     45, 46, 47, 48, 60, 61, 62, 63, 64, 65, 66, 67 ,68, 84, 85, 86)]
train.f2 <- train[, -c(1:86)]
train.f2 <- train.f2[, c(27,28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 65, 66, 67, 68, 69, 70, 71, 72 ,73, 74)]

train.f <- cbind(train.f1, train.f2)

train.f <- train.f[complete.cases(train.f),]


test.f1 <- test[, c(8, 9, 10, 38, 39, 40, 41, 42, 43, 44, 
                      45, 46, 47, 48, 60, 61, 62, 63, 64, 65, 66, 67 ,68, 84, 85, 86)]
test.f2 <- test[, -c(1:86)]
test.f2 <- test.f2[, c(27,28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 65, 66, 67, 68, 69, 70, 71, 72 ,73, 74)]

test.f <- cbind(test.f1, test.f2)

test.f <- test.f[complete.cases(test.f),]


part <- createDataPartition(y=train.f$classe,p=0.5,list=FALSE)

training<-train.f[part,]


p <- ggplot(training, aes(accel_belt_z, accel_forearm_y)) + geom_point(aes(colour = factor(classe)), size = 1)
p


rf_model<-train(classe~.,data=training,method="rf",  
                trControl=trainControl(method="cv",number=5),
                prox=TRUE,allowParallel=TRUE)


