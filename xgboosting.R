
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

### 1.2 PREPROCESSING [Optional step]
# Replace factors (more than 2) with WOE
woe_object <- calculate_woe(train_data)
train_data_woe <- apply_woe(train_data, woe_object)
test_data_woe <- apply_woe(test_data, woe_object)

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
############################ END OF CODE TO BE DISREGARDED
--------------------------------------------------------------------------------


####### CODE STARTS HERE  #########


### Load packages and set up the basic cleaned dataset
if(!require("klaR")) install.packages("klaR")
library(klaR)
if(!require("pROC")) install.packages("pROC"); library("pROC") # load the package
if(!require("randomForest")) install.packages("randomForest")
source("main.R")
source("masterload.R")

### 1. SETUP
## the options for model selection
model.control<- trainControl(
  method = "cv", # 'cv' for cross validation
  number = 5, # number of folds in cross validation
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  allowParallel = TRUE, # Enable parallelization if available
  returnData = TRUE # We will use this to plot partial dependence
)


## Define a search grid for model selection
## TUNES
xgb.parms.default <- expand.grid(nrounds = c(20, 40, 60, 80), 
                         max_depth = c(2, 4), 
                         eta = c(0.01, 0.05, 0.1, 0.15), 
                         gamma = 0,
                         colsample_bytree = c(0.8, 1),
                         min_child_weight = 1,
                         subsample = 0.8)


xgb.parms.1 <- expand.grid(nrounds = c(20, 40, 60, 80, 200), 
                         max_depth = c(2, 4, 6), 
                         eta = c(0.01, 0.05, 0.1, 0.15, 0.2), 
                         gamma = 0,
                         colsample_bytree = c(0.5, 0.8, 1),
                         min_child_weight = 1,
                         subsample = 0.8)



xgb.parms.2 <- expand.grid(nrounds = c(200, 800, 1000), 
                         max_depth = c(2, 4, 6), 
                         eta = c(0.001, 0.01, 0.05, 0.1, 0.15, 0.2), 
                         gamma = 0,
                         colsample_bytree = c(0.5, 0.8, 1),
                         min_child_weight = 1,
                         subsample = 0.8)


### 2. MODELS
## 2.1 xgb without preprocessing
xgb.default <- train(return_customer~., data = train60_data,  
                 method = "xgbTree",
                 tuneGrid = xgb.parms.default,
                 metric = "ROC", 
                 trControl = model.control)

xgb.param1 <- train(return_customer~., data = train60_data,  
                method = "xgbTree",
                tuneGrid = xgb.parms.1,
                metric = "ROC", 
                trControl = model.control)

xgb.param2 <- train(return_customer~., data = train60_data,  
                method = "xgbTree",
                tuneGrid = xgb.parms.2,
                metric = "ROC", 
                trControl = model.control)


## 2.1.2 xgb with woe
xgb_woe.default <- train(return_customer~., 
                  data = train60_data_woe,  
                  method = "xgbTree",
                  tuneGrid = xgb.default,
                  metric = "ROC", 
                  trControl = model.control)

xgb_woe.param1 <- train(return_customer~., 
                 data = train60_data_woe,  
                 method = "xgbTree",
                 tuneGrid = xgb.parms1,
                 metric = "ROC", 
                 trControl = model.control)

xgb_woe.param2 <- train(return_customer~., 
                 data = train60_data_woe,  
                 method = "xgbTree",
                 tuneGrid = xgb.parms2,
                 metric = "ROC", 
                 trControl = model.control)


## 2.1.2 xgb with woe + binning

xgb_woe_ef.default <- train(return_customer~., 
                         data = train60_data_woe_ef,  
                         method = "xgbTree",
                         tuneGrid = xgb.default,
                         metric = "ROC", 
                         trControl = model.control)


xgb_woe_ew.default <- train(return_customer~., 
                         data = train60_data_woe_ew,  
                         method = "xgbTree",
                         tuneGrid = xgb.default,
                         metric = "ROC", 
                         trControl = model.control)



## 2.2 xgb with woe + PCA

xgb_PCA <- train(return_customer~., data = train60_data_woe,  
                method = "xgbTree",
                preProcess = "pca",
                tuneGrid = xgb.default,
                metric = "ROC", 
                trControl = model.control)


### 3. PREDICTION
##Baseline Model
xgb.default.pred <- predict(xgb.default, newdata = validation_data, type = "prob")[,2]
xgb.param1.pred <- predict(xgb.param1, newdata = validation_data, type = "prob")[,2]
xgb.param2.pred <- predict(xgb.param2, newdata = validation_data, type = "prob")[,2]

## WOE 
xgb_woe.default.pred <- predict(xgb_woe.default, newdata = validation_data_woe, type = "prob")[,2]
xgb_woe.param1.pred <- predict(xgb_woe.param1, newdata = validation_data_woe, type = "prob")[,2]
xgb_woe.param2.pred <- predict(xgb_woe.param2, newdata = validation_data_woe, type = "prob")[,2]

## WOE + Binning
xgb_woe_ef.default <- predict(xgb_woe_ef.default, newdata = validation_data_woe_ef, type = "prob")[,2]
xgb_woe_ew.default <- predict(xgb_woe_ew.default, newdata = validation_data_woe_ew, type = "prob")[,2]

## WOE + PCA
xgb.pca.pred <- predict(xgb_PCA, newdata = validation_data_woe, type = "prob")[,2]


### 4. SCORE

#Base model score
xgb_base_default_score <- predictive_performance(validation_data$return_customer, xgb.default.pred, cutoff = 0.24)
xgb_base_param1_score <-predictive_performance(test_data$return_customer, xgb.param1.pred, cutoff = 0.19)
xgb_base_param2_score <-predictive_performance(test_data$return_customer, xgb.param2.pred, cutoff = 0.19)
#  need to find optimal cutpoints

#WOE
xgb_woe_default_score <-predictive_performance(test_data_woe$return_customer, xgb_woe.default.pred, cutoff = 0.212)
xgb_woe_param1_score <-predictive_performance(test_data_woe$return_customer, xgb_woe.param1.pred, cutoff = 0.212)
xgb_woe_param2_score <-predictive_performance(test_data_woe$return_customer, xgb_woe.param2.pred, cutoff = 0.212)
#  need to find optimal cutpoints


#WOE  + Binning
xgb_ef_default_score <-predictive_performance(test_data_woe$return_customer, xgb_woe_ef.default, cutoff = 0.212)
xgb_ew_default_score <-predictive_performance(test_data_woe$return_customer, xgb_woe_ew.default, cutoff = 0.212)
#  need to find optimal cutpoints

#WOE + PCA
xgb_pca_default_score <-predictive_performance(test_data_woe$return_customer, xgb.pca.pred, cutoff = 0.19)
#  need to find optimal cutpoints


### 5. SAVE PREDICTIONS IN DF

#Call validation file to save predictions
df_predictions_validation = call_master("predictions_validation.csv")

#Add predictions
df_predictions_validation$xgb.default.pred = xgb.default.pred #hamayun
df_predictions_validation$xgb.param1.pred = xgb.param1.pred #oren
df_predictions_validation$xgb.param2.pred = xgb.param2.pred #oren
df_predictions_validation$xgb_woe.default.pred = xgb_woe.default.pred #hamayun
df_predictions_validation$xgb_woe.param1.pred = xgb_woe.param1.pred #oren
df_predictions_validation$xgb_woe.param2.pred = xgb_woe.param2.pred #oren
df_predictions_validation$xgb_woe_ef.default = xgb_woe_ef.default #hamayun
df_predictions_validation$xgb_woe_ew.default = xgb_woe_ew.default #hamayun
df_predictions_validation$xgb.pca.pred = xgb.pca.pred #hamayun
  
#Save validation file
df_predictions_validation = save_prediction_to_master("predictions_validation.csv", df_predictions_validation)
#Remember to push afterwards

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








