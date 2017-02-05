## TRAINING ON 80% AND PREDICTING TEST - TO CREATE META-MODEL

##################### PART I : SET-UP AND TRAIN PRIMARY MODELS TO PREDICT TEST DATASET

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


## 2.1 xgb without preprocessing - TRAIN80
xgb.default <- train(return_customer~., data = train_data,  
                     method = "xgbTree",
                     tuneGrid = xgb.parms.default,
                     metric = "ROC", 
                     trControl = model.control)

xgb.param1 <- train(return_customer~., data = train_data,  
                    method = "xgbTree",
                    tuneGrid = xgb.parms.1,
                    metric = "ROC", 
                    trControl = model.control)

xgb.param2 <- train(return_customer~., data = train_data,  
                    method = "xgbTree",
                    tuneGrid = xgb.parms.2,
                    metric = "ROC", 
                    trControl = model.control)


## 2.1.2 xgb with woe
xgb_woe.default <- train(return_customer~., 
                         data = train_data_woe,  
                         method = "xgbTree",
                         tuneGrid = xgb.parms.default,
                         metric = "ROC", 
                         trControl = model.control)

xgb_woe.param1 <- train(return_customer~., 
                        data = train_data_woe,  
                        method = "xgbTree",
                        tuneGrid = xgb.parms.1,
                        metric = "ROC", 
                        trControl = model.control)

xgb_woe.param2 <- train(return_customer~., 
                        data = train_data_woe,  
                        method = "xgbTree",
                        tuneGrid = xgb.parms.2,
                        metric = "ROC", 
                        trControl = model.control)


## 2.1.3 xgb with woe + binning

xgb_woe_ef.default <- train(return_customer~., 
                            data = train_data_woe_ef,  
                            method = "xgbTree",
                            tuneGrid = xgb.parms.default,
                            metric = "ROC", 
                            trControl = model.control)


xgb_woe_ew.default <- train(return_customer~., 
                            data = train_data_woe_ew,  
                            method = "xgbTree",
                            tuneGrid = xgb.parms.default,
                            metric = "ROC", 
                            trControl = model.control)



## 2.2 xgb with woe + PCA

xgb_PCA <- train(return_customer~., data = train_data_woe,  
                 method = "xgbTree",
                 preProcess = "pca",
                 tuneGrid = xgb.parms.default,
                 metric = "ROC", 
                 trControl = model.control)


## 2.3 LOGISTIC REGRESSION
library(klaR)
lr <- glm(return_customer ~., data = train_data_woe, family = binomial(link="logit"))


### 3. PREDICTION
##Baseline Model
xgb.default.pred <- predict(xgb.default, newdata = test_data, type = "prob")[,2]
xgb.param1.pred <- predict(xgb.param1, newdata = test_data, type = "prob")[,2]
xgb.param2.pred <- predict(xgb.param2, newdata = test_data, type = "prob")[,2]

## WOE 
xgb_woe.default.pred <- predict(xgb_woe.default, newdata = test_data_woe, type = "prob")[,2]
xgb_woe.param1.pred <- predict(xgb_woe.param1, newdata = test_data_woe, type = "prob")[,2]
xgb_woe.param2.pred <- predict(xgb_woe.param2, newdata = test_data_woe, type = "prob")[,2]

## WOE + Binning
xgb_woe_ef.default <- predict(xgb_woe_ef.default, newdata = test_data_woe_ef, type = "prob")[,2]
xgb_woe_ew.default <- predict(xgb_woe_ew.default, newdata = test_data_woe_ew, type = "prob")[,2]

## WOE + PCA
xgb.pca.pred <- predict(xgb_PCA, newdata = test_data_woe, type = "prob")[,2]

## LOGISTIC REGRESSION
pred.lr.woe <- predict(lr, newdata = test_data_woe, type = "response")

### 4. SCORE

#Base model score
xgb_base_default_score <- predictive_performance(test_data$return_customer, xgb.default.pred, cutoff = 0.238)
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

# LOGISTIC REGRESSION
optimal_cutoff(test_data_woe$return_customer, pred.lr.woe)
predictive_performance(test_data_woe$return_customer, pred.lr.woe, cutoff = 0.2279837, returnH = FALSE)


### 5. SAVE PREDICTIONS IN DF

df_predictions_test = data.frame(return_customer = test_data$return_customer)

#Call validation file to save predictions
df_predictions_test = call_master("predictions_test.csv")

#Add predictions
df_predictions_test$xgb.default.pred = xgb.default.pred #hamayun
df_predictions_test$xgb.param1.pred = xgb.param1.pred #oren
df_predictions_test$xgb.param2.pred = xgb.param2.pred #oren
df_predictions_test$xgb_woe.default.pred = xgb_woe.default.pred #hamayun
df_predictions_test$xgb_woe.param1.pred = xgb_woe.param1.pred #oren
df_predictions_test$xgb_woe.param2.pred = xgb_woe.param2.pred #oren
df_predictions_test$xgb_woe_ef.default = xgb_woe_ef.default #hamayun
df_predictions_test$xgb_woe_ew.default = xgb_woe_ew.default #hamayun
df_predictions_test$xgb.pca.pred = xgb.pca.pred #hamayun
df_predictions_test$lr.woe = pred.lr.woe #hamayun


#Save validation file
df_predictions_test = save_prediction_to_master("predictions_test.csv", df_predictions_test)
#Remember to push afterwards

View(df_predictions_test)


##################### PART2 - TRAIN META-MODELS

### 1. META META

# BEST 4 [5 if we get meta-model running] #

# CALL FILES WITH RESULTS
df_predictions_validation = call_master("predictions_validation.csv")
performance_validation = call_master("performance_validation.csv")
df_predictions_test = call_master("predictions_test.csv")
performance_test = call_master("performance_test.csv")


# CREATE DATAFRAME OF VALIDATION/TEST WITH 4 BEST COLUMNS
df_predictions_validation_meta4 = df_predictions_validation[,c(1,3,9, 10, 13)]
df_predictions_test_meta4 = df_predictions_test[,c(1,3,10, 12, 13)]
View(df_predictions_test)
View(performance_validation)


# CREATE DATAFRAME FOR TEST - TO TEST META-MODEL TRAINED ON TRAIN 
df_predictions_test_metaprep = df_predictions_test[,c(1,7,10,12,13)]
df_predictions_test_meta_FINAL4 = df_predictions_test_metaprep[,1:2]
df_predictions_test_meta_FINAL4$xgb = df_predictions_test_metaprep[,4]
df_predictions_test_meta_FINAL4$xgb_woe = df_predictions_test_metaprep[,2]
df_predictions_test_meta_FINAL4$logistic = df_predictions_test_metaprep[,5]
df_predictions_test_meta_FINAL4$random_forest = df_predictions_test_metaprep[,3]
df_predictions_test_meta_FINAL4$xgb_woe.param1.pred = NULL

# dataframe with 4 chosen models
df_predictions_test_meta_FINAL4

# 1.1 Logistic regression
lr <- glm(return_customer ~., data = df_train_predictions, family = binomial(link="logit"))
meta4.lr.train <- predict(lr, newdata = df_predictions_test_meta_FINAL4, type = "response")
optimal_cutoff(df_predictions_test_meta_FINAL4$return_customer, meta4.lr.train)
predictive_performance(df_predictions_test_meta_FINAL4$return_customer, meta4.lr.train, cutoff =  0.1748927, returnH = FALSE)


# 1.2 Random forest

k <- 5
# Set a seed for the pseudo-random number generator
set.seed(123)
model.control <- trainControl(
  method = "cv", # 'cv' for cross validation
  number = k, # number of folds in cross validation
  classProbs = TRUE, # Return class probabilities
  summaryFunction = twoClassSummary, # twoClassSummary returns AUC
  allowParallel = TRUE # Enable parallelization if available
)

rf.parms <- expand.grid(mtry = 1:10)

rf.meta.train <- train(return_customer~., 
                 data = df_train_predictions,  
                 method = "rf", 
                 ntree = 500, 
                 tuneGrid = rf.parms, 
                 metric = "ROC", 
                 trControl = model.control)
#Predict
meta4.rf.train   <- predict(rf.meta.train, newdata = df_predictions_test_meta_FINAL4, type = "prob")[,2]
optimal_cutoff(df_predictions_test_meta_FINAL4$return_customer, meta4.rf.train)
predictive_performance(df_predictions_test_meta_FINAL4$return_customer, meta4.rf.train, cutoff = 0.246, returnH = FALSE)



# 1.3 XGB meta
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
xgb.parms.default <- expand.grid(nrounds = c(20, 40, 60, 80), 
                                 max_depth = c(2, 4), 
                                 eta = c(0.01, 0.05, 0.1, 0.15), 
                                 gamma = 0,
                                 colsample_bytree = c(0.8, 1),
                                 min_child_weight = 1,
                                 subsample = 0.8)
# Model
xgb.def.train <- train(return_customer~., data = df_train_predictions,  
                     method = "xgbTree",
                     tuneGrid = xgb.parms.default,
                     metric = "ROC", 
                     trControl = model.control)



#Predict using XGB
xgb.meta4.train <- predict(xgb.def.train, newdata = df_predictions_test_meta_FINAL4, type = "prob")[,2]
xgb.params2.meta4 <- predict(xgb.def, newdata = df_predictions_test_meta4, type = "prob")[,2]
optimal_cutoff(df_predictions_test_meta_FINAL4$return_customer, xgb.meta4.train)
predictive_performance(df_predictions_test_meta_FINAL4$return_customer, xgb.meta4.train, cutoff =  0.3455043, returnH = FALSE)


# 1.4 REGULARIZED REGRESSION  # not using it due to unsatisfactory results
if(!require("rrlda")) install.packages("rrlda"); library("rrlda")
if(!require("glmnet")) install.packages("glmnet"); library("glmnet")


cctrl1 <- trainControl(method = "cv", number = 3, 
                       classProbs = TRUE,
                       summaryFunction = twoClassSummary)
x = df_predictions_validation_meta4[,-1]
y = df_predictions_validation_meta4[,1]
x_test = df_predictions_test_meta4[,-1]
y_test = df_predictions_meta4[,-1]


rega.meta <- train(x = x,
                   y = y,
                    method = "rrlda",
                    trControl = cctrl1,
                    metric = "ROC")

rega2.meta <- glmnet(x = x, y = y, alpha = 1, family = "binomial")


meta4.rega <- predict(rega.meta, x_test, type = "response")
optimal_cutoff(df_predictions_test_meta4$return_customer, meta4.rega)
predictive_performance(y_test, meta4.rega, cutoff =  0.5204702, returnH = FALSE)


## 2. SAVE PREDICTIONS TO RESULTS 

## Saving to a csv
df_predictions_test = call_master("predictions_test.csv")
View(df_predictions_test)


df_predictions_test$meta4.lr = meta4.lr
df_predictions_test$meta4.xgb = xgb.meta4
df_predictions_test$meta4.rf = meta4.rf


df_predictions_test = save_prediction_to_master("predictions_test.csv", df_predictions_test)


## 2.2 SAVE PERFORMANCE MEASURES
df_performance_test = call_master("performance_test.csv")
df_performance_test = get_optimal_cutpoint(df_predictions_test, df_performance_test)
View(df_performance_test)





