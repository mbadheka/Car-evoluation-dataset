RF <- function() {
  
  #install.packages("caret")
  library(caret)
  #install.packages("e1071")
  library(e1071)
  #install.packages("kernlab")
  library(kernlab)
  
  setwd("C:/Users/hiran/Google Drive/Unitec/ML/Final_report/data")
  data1 <- read.csv("car-data.csv")
  colnames(data1)[1] = "BuyingPrice"
  summary(data1)
 
  # Split into Train and Validation sets
  # Training Set : Validation Set = 60 : 20 : 20(random)
  
  set.seed(100)
  train <- sample(nrow(data1), 0.6*nrow(data1), replace = FALSE)
  
  TrainSet <- data1[train,]
  
  RemainingSet <- data1[-train,]
  samp2 <- sample(1:nrow(RemainingSet),0.5*nrow(RemainingSet), replace = FALSE)
  
  ValidationSet <- RemainingSet[samp2,] ## First chunk of 20%
  
  TestSet <- RemainingSet[-samp2,] ## Second Chunk of 20%
  
  summary(TrainSet)
  summary(TestSet)
  summary(ValidationSet)
  
  trctrl <- trainControl(method = "repeatedcv", number = 10, repeats = 3)
  set.seed(3233)
  
  svm_Radial <- train(Condition ~., data = TrainSet, method = "svmRadial",
                      trControl=trctrl,
                      preProcess = c("center", "scale"),
                      tuneLength = 10)
  svm_Radial
  
  test_pred_Radial <- predict(svm_Radial, newdata = ValidationSet)
  confusionMatrix(test_pred_Radial, ValidationSet$Condition )
  
  grid_radial <- expand.grid(sigma = c(0,0.01, 0.02, 0.025, 0.03, 0.04,
                                       0.05, 0.06, 0.07,0.08, 0.09, 0.1, 0.25, 0.5, 0.75,0.9),
                             C = c(0,0.01, 0.05, 0.1, 0.25, 0.5, 0.75,
                                   1, 1.5, 2,5))
  set.seed(3233)
  svm_Radial_Grid <- train(Condition ~., data = TrainSet, method = "svmRadial",
                           trControl=trctrl,
                           preProcess = c("center", "scale"),
                           tuneGrid = grid_radial,
                           tuneLength = 10)
  svm_Radial_Grid
  
  plot(svm_Radial_Grid)

  #test_pred_grid <- predict(svm_Linear_Grid, newdata = ValidationSet)
  #confusionMatrix(test_pred_grid, ValidationSet$Condition )
  
  test_pred_grid <- predict(svm_Radial_Grid, newdata = ValidationSet)
  confusionMatrix(test_pred_grid, ValidationSet$Condition )
  
  test_pred_grid_1 <- predict(svm_Radial_Grid, newdata = TestSet)
  confusionMatrix(test_pred_grid_1, TestSet$Condition )
}

