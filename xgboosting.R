
###################### DISREGARD - NOT REQUIRED SINCE WE'RE NOW USING MAIN.R
-------------------------------------------------------------------------------
library(lubridate)
source("helper.R")
known <- get_dataset("assignment_BADS_WS1617_known.csv")
known <- treat_missing_values(known)
known <- treat_dates(known)
known <- treat_postcodes(known)
known <- standardize_weight(known)


## Partition data, excluding columns with NA

# which columns have NAs
colnames(train_data)[colSums(is.na(train_data)) > 0]


# Draw a random, stratified sample including p percent of the data, exclude columns with NAs
idx.train <- createDataPartition(y = known$return_customer, p = 0.8, list = FALSE) 
train <- known[idx.train, -which(names(known) %in% c("form_of_address","advertising_code","deliverydate_estimated","deliverydate_actual"))] # training set
test <-  known[-idx.train, -which(names(known) %in% c("form_of_address","advertising_code","deliverydate_estimated","deliverydate_actual"))] # test set


confusionMatrix(predict(model_xgb_pca, test))

summary(predict(model_xgb_pca, test))
summary(test$return_customer)

############################ END OF CODE TO BE DISREGARDED
--------------------------------------------------------------------------------


####### CODE STARTS HERE  #########


### Load packages and set up the basic cleaned dataset
if(!require("klaR")) install.packages("klaR")
library(klaR)
if(!require("pROC")) install.packages("pROC"); library("pROC") # load the package
if(!require("randomForest")) install.packages("randomForest")
source("main.R")


### 1. SETUP
## the options for model selection
model.control<- trainControl(
  method = "cv", # 'cv' for cross validation
  number = 15, # number of folds in cross validation
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  allowParallel = TRUE, # Enable parallelization if available
  returnData = TRUE # We will use this to plot partial dependence
)


## Define a search grid for model selection
xgb.parms <- expand.grid(nrounds = c(20, 40, 60, 80), 
                         max_depth = c(2, 4), 
                         eta = c(0.01, 0.05, 0.1, 0.15), 
                         gamma = 0,
                         colsample_bytree = c(0.8, 1),
                         min_child_weight = 1,
                         subsample = 0.8)

### 1.2 PREPROCESSING [Optional step]
# Replace factors (more than 2) with WOE
train_data_woe <- replace_factors_by_woe(train_data)
# Predict factor levels in the test dataset
test_data_woe <- predict(woe_object, newdata = test_data, replace = TRUE)
# Replace column names so they are the same in the test and train datasets respectively
colnames(test_data_woe) [35:41] <- c("form_of_address", "email_domain", "model", "payment", "postcode_invoice", "postcode_delivery", "advertising_code")


### 2. MODELS
## 2.1 xgb without preprocessing
xgb <- train(return_customer~., data = train_data,  
                 method = "xgbTree",
                 tuneGrid = xgb.parms,
                 metric = "ROC", 
                 trControl = model.control)
## 2.1.2 xgb with woe
xgb_woe <- train(return_customer~., data = train_data_woe,  
                method = "xgbTree",
                tuneGrid = xgb.parms,
                metric = "ROC", 
                trControl = model.control)

## 2.2 xgb with woe + PCA

xgb_PCA <- train(return_customer~., data = train_data_woe,  
                method = "xgbTree",
                preProcess = "pca",
                tuneGrid = xgb.parms,
                metric = "ROC", 
                trControl = model.control)


### 3. PREDICTION
xgb.pred <- predict(xgb, newdata = test_data, type = "prob")[,2]
xgb.pred.woe <- predict(xgb_woe, newdata = test_data_woe, type = "prob")[,2]
xgb.pca.pred <- predict(xgb_PCA, newdata = test_data_woe, type = "prob")[,2]


### 4. MODEL EVALUATION: 
## 4.1 Estimate performance on unseen data based on test set
auc(test_data$return_customer, xgb.pred)
# Area under the curve: 0.672
auc(test_data_woe$return_customer, xgb.pred.woe)
# Area under the curve: 0.667
auc(test_data$return_customer, xgb.pca.pred)
# Area under the curve: 0.651


xgb.pred.woe[1:20]
summary(xgb.pred.woe)

y.validation <- as.numeric(test_data$return_customer)-1
h <- HMeasure(y.validation, xgb.pred)
plotROC(h, which = 1)


## 4.2 Confusion matrix
tau <- 0.05  #confused about how we pick the tau, decreasing it imrpoves the accuracy
#convert probability prediction to discrete class predictions
yhat.xgb.class <- factor(xgb.pred> tau, labels = c("yes", "no"))
confusionMatrix(data = yhat.xgb.class, reference = test_data$return_customer, positive = "yes")


### 5. SCORE
predictive_performance(test_data$return_customer, xgb.pred, cutoff = 0.19)
#  maxmizes the score for xgb + woe i.e. 
predictive_performance(test_data_woe$return_customer, xgb.pred.woe, cutoff = 0.231)
# 0.231 maxmizes the score for xgb + woe i.e. 0.846
predictive_performance(test_data_woe$return_customer, xgb.pca.pred, cutoff = 0.19)
# 0.19 maxmizes the score for xgb + woe i.e. 0.835



### 6. LOSS FUNCTION FOR XGBOOST
library(xgboost)
install.packages("readr")
library(readr)
library(stringr)
library(car)
install.packages("MatrixModels")
library(Matrix)
train <- sparse.model.matrix(return_customer ~ .-1, data = train_data_woe)
test <- sparse.model.matrix(return_customer ~ .-1, data = test_data_woe)

dtrain <- xgb.DMatrix(data = train, label = as.numeric(train_data_woe$return_customer)-1)
dtest <- xgb.DMatrix(data = test, label = as.numeric(test_data_woe$return_customer)-1)


watchlist <- list(eval = dtest, train = dtrain)


bst <- xgb.train(data=dtrain, max.depth=2, eta=1, nthread = 2, nround=2, watchlist=watchlist, objective = "binary:logistic")
bst <- xgb.train(data=dtrain, max.depth=2, eta=1, nthread = 2, nround=2, watchlist=watchlist, eval.metric = "error", eval.metric = "logloss", objective = "binary:logistic")
bst <- xgb.train(data=dtrain, booster = "gblinear", max.depth=2, nthread = 2, nround=2, watchlist=watchlist, eval.metric = "error", eval.metric = "logloss", objective = "binary:logistic")

importance_matrix <- xgb.importance(model = bst)
print(importance_matrix)
xgb.plot.importance(importance_matrix = importance_matrix)


logregobj <- function(preds, train_data_woe) {
  labels <- getinfo(train_data_woe, "label")
  preds <- 1/(1 + exp(-preds))
  grad <- preds - labels
  hess <- preds * (1 - preds)
  return(list(grad = grad, hess = hess))
}






------------------------------------------------------------------------------------------------
############# RANDOM FOREST STARTS HERE

### 1. Setup  
## Specify the number of folds
# Remember that each candidate model will be constructed on each fold
k <- 3
# Set a seed for the pseudo-random number generator
set.seed(123)

### Initialize the caret framework
# This part specifies how we want to compare the models
# It includes the validation procedure, e.g. cross-validation
# and the error metric that is return (summaryFunction)
# Note: You can look into the summaryFunctions to see what
# happens and even write your own
# Try: print(twoClassSummary)

model.control <- trainControl(
  method = "cv", # 'cv' for cross validation
  number = k, # number of folds in cross validation
  classProbs = TRUE, # Return class probabilities
  summaryFunction = twoClassSummary, # twoClassSummary returns AUC
  allowParallel = TRUE # Enable parallelization if available
)

### 2. Model
# Define a search grid of values to test for a sequence of randomly
# sampled variables as candidates at each split
rf.parms <- expand.grid(mtry = 1:10)

# 2.1 Train random forest rf with a 5-fold cross validation 
rf.caret <- train(return_customer~., 
                  data = train_data,  
                  method = "rf", 
                  ntree = 500, 
                  tuneGrid = rf.parms, 
                  metric = "ROC", 
                  trControl = model.control)
# 2.2 RF with woe
rf.caret.woe <- train(return_customer~., 
                  data = train_data_woe,  
                  method = "rf", 
                  ntree = 500, 
                  tuneGrid = rf.parms, 
                  metric = "ROC", 
                  trControl = model.control)





# Compare the performance of the model candidates
# on each cross-validation fold
rf.caret$results
plot(rf.caret)

### 3. PREDICTION
# Predict the outcomes of the test set with the predict function, 
# i.e. the probability of someone being a bad risk
yhat.rf.caret   <- predict(rf.caret, newdata = test_data, type = "prob")[,2]

yhat.rf.caret.woe   <- predict(rf.caret.woe, newdata = test_data_woe, type = "prob")[,2]


### 4. MODEL EVALUATION
# As done in previous exercises, the AUC is computed in order to evaluate our model performance. 
if(!require("pROC")) install.packages("pROC"); library("pROC") # load the package
auc.caret <- auc(test_data$return_customer, yhat.rf.caret) 
# Area under the curve: 0.645
auc.caret.woe <- auc(test_data_woe$return_customer, yhat.rf.caret.woe) 
# Area under the curve: 0.6503

### 5. SCORE
predictive_performance(test_data_woe$return_customer, yhat.rf.caret.woe, cutoff = 0.227)
# 0.227 maxmizes the score for xgb + woe i.e. 0.7803

