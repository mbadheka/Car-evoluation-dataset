install.packages("rpart")
install.packages("caret")
install.packages("e1071")

library(rpart)
library(caret)
library(e1071)


setwd("~")
data1<- read.csv("car_data1.csv", header= TRUE)

head(data1)

str(data1)

summary(data1)

traindata <- sample(nrow(data1), 0.7*nrow(data1), replace = FALSE)
trainingset <- data1[traindata,]
validationset <- data1[-traindata,]
summary(trainingset)
summary(validationset)

# We will compare mm1(object) of Random Forest with Decision Tree model


mmd1 = train(class~ ., data = trainingset, method = "rpart")
mmd2 = predict(mmd1, data = trainingset)
table(mmd2, trainingset$class)

mean(mmd2 == trainingset$class)
## accurecy is around 86.66%


## Running on Validation Set
mmd3 <-predict(mmd1, newdata = validationset) ##validation object mmd3
table(mmd3, validationset$class)

mean(mmd3 == validationset$class)

# accurecy is around 84.39

