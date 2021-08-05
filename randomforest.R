install.packages("randomForest")
library(randomForest)


setwd("~")
data1<- read.csv("car_data1.csv", header= TRUE)

head(data1)

str(data1)

summary(data1)
set.seed(100)
train <- sample(nrow(data1), 0.7*nrow(data1), replace = FALSE)
TrainSet <- data1[train,]
ValidSet <- data1[-train,]
summary(TrainSet)
summary(ValidSet)

mm1<-randomForest(class~ ., data = TrainSet, importance = TRUE)


# tuning of parameter

mm2<-randomForest(class~ ., data = TrainSet, ntree = 500, mtry = 6, importance = TRUE)

#prediction on training data set

predTrain <- predict(mm2, TrainSet, type = "class")
table(predTrain, TrainSet$class)
##mean(predTrain == TrainSet$class)

# Predicting on Validation set
predValid <- predict(mm2, ValidSet, type = "class")
# Checking classification accuracy
mean(predValid == ValidSet$class)               # accurecy is 97.30     
table(predValid,ValidSet$class)

#chekinf of imporant variable 

importance(mm2)        
varImpPlot(mm2) 

# Using For loop to identify the right mtry for model
z=c()
n=8
# k=12
z  = matrix(NA , nrow=n-2, ncol=1)
counter = 1
#for (j in seq(500,800))
#{
for (i in 3:n) {
  mm3 <- randomForest(class~ ., data = TrainSet, ntree = 500, mtry = i, importance = TRUE)
  predValid <- predict(mm3, ValidSet, type = "class")
  z[i-2] = mean(predValid == ValidSet$class)
  }
#}

z

# plot(3:8,z)



# # please add test set before running 
# # removing of objent name test set 
# Final tuning parameters of Random Forest model
# # 
# maxMatrix <- which(z == max(z), arr.ind = TRUE)
# maxMatrix
# 
# maxTrayVal <- maxMatrix[[1,1]] + 2
# maxTreeVal <- 500 + (maxMatrix[[1,2]] * 25)
# 
# mm4 <- randomForest(class~ ., data = ValidSet, ntree = maxTreeVal, mtry = maxTrayVal, importance = TRUE)
# mm4
# 
# # Predicting on Validation set
# predTest <- predict(mm4, TestSet, type = "class")
# # Checking classification accuracy
# table(predTest, TestSet$class)
# mean(predTest == TestSet$class)