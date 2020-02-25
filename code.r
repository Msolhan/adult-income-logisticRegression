library(tidyverse)
library(ggplot2)
#library(forcats)


Adult_data <- read.csv("../input/it405-khas/train.csv", stringsAsFactors = FALSE)
Adult_test <- read.csv("../input/it405-khas/test.csv", stringsAsFactors = FALSE)
Adult_data$Education <- NULL

head (Adult_data)

tail (Adult_data)

nrow(Adult_data)

names(Adult_data)

str(Adult_data)

sapply(Adult_data, function(x) sum(is.na(x)))

table(Adult_data$Workclass)
table(Adult_data$Occupation)
table(Adult_data$Country)

Adult_data[Adult_data == ""] <- NA
Adult_test[Adult_test == ""] <- NA
       
sapply(Adult_data, function(x) sum(is.na(x)))
       
Adult_data$Workclass[is.na(Adult_data$Workclass)]   <- "Private"
Adult_data$Occupation[is.na(Adult_data$Occupation)] <- "Prof-Specialty"
Adult_data$Country[is.na(Adult_data$Country)]       <- "United-States"
       
      
Adult_test$Workclass[is.na(Adult_test$Workclass)]   <- "Private"
Adult_test$Occupation[is.na(Adult_test$Occupation)] <- "Prof-Specialty"
Adult_test$Country[is.na(Adult_test$Country)]       <- "United-States"


# Dealing With Outliers
       
par(mfrow = c(3,3))
boxplot(Adult_data$Age, main = "Age")
boxplot(Adult_data$fnlwgt, main ="fnlwgt")
boxplot(Adult_data$Education_Num, main = "Education_Num")
       
boxplot(Adult_data$Hours_per_week, main = "Hours per week")

       
Outlier1 <- boxplot (Adult_data$Age, plot = FALSE)$out
Adult_data <- Adult_data[-which(Adult_data$Age %in% Outlier1 ),]
       
Outlier2 <- boxplot (Adult_data$fnlwgt, plot = FALSE)$out
Adult_data <- Adult_data[-which(Adult_data$fnlwgt %in% Outlier2 ),]
       
Outlier3 <- boxplot (Adult_data$Education_Num, plot = FALSE)$out
Adult_data <- Adult_data[-which(Adult_data$Education_Num %in% Outlier3 ),]

Outlier6 <- boxplot (Adult_data$Hours_per_week, plot = FALSE)$out
Adult_data <- Adult_data[-which(Adult_data$Hours_per_week %in% Outlier6 ),]
       
       
# Convert factor
       
summary(Adult_data)
str(Adult_data)

summary(Adult_data)
str(Adult_data)

       
Adult_test$Education <- NULL
Adult_data$Education <- NULL       

Adult_data$Workclass           <- as.factor(Adult_data$Workclass)
Adult_data$Martial_Status      <- as.factor(Adult_data$Martial_Status)
Adult_data$Occupation          <- as.factor(Adult_data$Occupation)
Adult_data$Relationship        <- as.factor(Adult_data$Relationship)
Adult_data$Race                <- as.factor(Adult_data$Race)
Adult_data$Sex                 <- as.factor(Adult_data$Sex)
Adult_data$Country             <- as.factor(Adult_data$Country)
Adult_data$Target              <- as.factor(Adult_data$Target)
       
summary(Adult_test)
str(Adult_test)
Adult_test$Workclass           <- as.factor(Adult_test$Workclass)
#Adult_test$Education           <- as.factor(Adult_test$Education)
Adult_test$Martial_Status      <- as.factor(Adult_test$Martial_Status)
Adult_test$Occupation          <- as.factor(Adult_test$Occupation)
Adult_test$Relationship        <- as.factor(Adult_test$Relationship)
Adult_test$Race                <- as.factor(Adult_test$Race)
Adult_test$Sex                 <- as.factor(Adult_test$Sex)
Adult_test$Country             <- as.factor(Adult_test$Country)

       

library(caTools)

split <- sample.split(Adult_data$Target, SplitRatio = 0.7)
train <- subset(Adult_data, split == TRUE)
test <- subset(Adult_data, split == FALSE)



       
classifier <- step(glm(Target~.,data=train,family = "binomial"),direction = "backward")
summary (classifier)
       
prediction <- predict (classifier , Adult_test, type = "response")
# an extra argument(type = "response") is required while using 'predict' function to generate response as probabilities
       
   
       
tab1 = table(Actual=Adult_test$Target, prediction = prediction >= 0.5)
tab1
       
       
#sum(diag(tab1)/sum(tab1))
       

#submission <- data.frame ("Id","Target" = prediction , stringsAsFactors = FALSE)

#write.csv(prediction , "submission.csv" )

#submission

  
       

       


