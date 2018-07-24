library("randomForest")


split_train_test <- function(data,split_ratio,seed=156){
  ## Splits data into train and test set for a given split ratio and 
  ## seed that determines psuedo random numbers
  train_ind <- sample(1:nrow(data),floor(nrow(data) * split_ratio))
  test_ind  <- (1:nrow(data))[-train_ind]
  train_set <- data[train_ind,]; test_set <- data[test_ind,]
  return(list(train_set,test_set))
}



dat <- dataset
################## Decision Tree ########################
train <- split_train_test(dat,.8,101)
test  <- train[[2]]; train  <- train[[1]]


forest <- randomForest(Score ~ . , data = train,mtry = 6)
# plot(forest)
predict.forest <- predict(forest,test)
# plot(predict.forest,test$Score)
# abline(0,1)
mean((as.numeric(predict.forest) - as.numeric(test$Score))^2) # MSE of Model
forest$importance
varImpPlot(forest)
