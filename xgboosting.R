###################### DISREGARD - NOT REQUIRED SINCE WE'RE NOW USING MAIN.R
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



####### CODE STARTS HERE  #########


### Load packages and set up the basic cleaned dataset

if(!require("pROC")) install.packages("pROC"); library("pROC") # load the package
source("main.R")


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
xgb.parms <- expand.grid(nrounds = c(20, 40, 60, 80), 
                         max_depth = c(2, 4), 
                         eta = c(0.01, 0.05, 0.1, 0.15), 
                         gamma = 0,
                         colsample_bytree = c(0.8, 1),
                         min_child_weight = 1,
                         subsample = 0.8)

### 2. MODELS
## 2.1 xgb without preprocessing
xgb <- train(return_customer~., data = train_data,  
                 method = "xgbTree",
                 tuneGrid = xgb.parms,
                 metric = "ROC", trControl = model.control)

## 2.2 xgb with PCA
xgb_PCA <- train(return_customer~., data = train_data,  
             method = "xgbTree",
             tuneGrid = xgb.parms,
             metric = "ROC", trControl = model.control)


### 3. PREDICTION
xgb.pred <- predict(xgb, newdata = test_data, type = "prob")[,2]
xgb.pca.pred <- predict(xgb_PCA, newdata = test_data, type = "prob")[,2]


### 4. MODEL EVALUATION: 
## 4.1 Estimate performance on unseen data based on test set
auc(test_data$return_customer, xgb.pred)
auc(test_data$return_customer, xgb.pca.pred)

y.validation <- as.numeric(test_data$return_customer)-1
h <- HMeasure(y.validation, xgb.pred)
plotROC(h, which = 1)


## 4.2 Confusion matrix
tau <- 0.05  #confused about how we pick the tau, decreasing it imrpoves the accuracy
#convert probability prediction to discrete class predictions
yhat.xgb.class <- factor(xgb.pred> tau, labels = c("yes", "no"))
confusionMatrix(data = yhat.xgb.class, reference = test_data$return_customer, positive = "yes")

