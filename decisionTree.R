# Input load. Please do not change #
`dataset` = read.csv('C:/Users/ludwig/REditorWrapper_755d70d7-011b-4eae-8e8d-2c901be59e5a/input_df_cba1c597-79a7-477b-b919-b1647b845817.csv', check.names = FALSE, encoding = "UTF-8", blank.lines.skip = FALSE);
# Original Script. Please update your script content here and once completed copy below section back to the original editing window #
##############################


library(tree)


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

tree <- tree(Score ~ .,data = train)
# summary(tree)
plot(tree)
text(tree)

library("rpart.plot")

rpart.plot(tree)
