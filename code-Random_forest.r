library("randomForest")
       
set.seed(100)
train <- sample(nrow(Adult_data), 0.7*nrow(Adult_data), replace = FALSE)
TrainSet <- Adult_data[train,]
ValidSet <- Adult_data[-train,]
summary(TrainSet)
summary(ValidSet)
model1 <- randomForest(Target ~ ., data = TrainSet, importance = TRUE)
model1

predTrain <- predict(model1, TrainSet, type = "class")
# Checking classification accuracy
table(predTrain, TrainSet$Target)  

predValid <- predict(model1, ValidSet, type = "class")

mean(predValid == ValidSet$Target)                    
table(predValid,ValidSet$Target)

mean(predValid == ValidSet$Target)   
table(predValid,ValidSet$Target)
