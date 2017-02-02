## TRAINING ON 80% AND PREDICTING TEST - TO CREATE META-MODEL



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

#Save validation file
df_predictions_test = save_prediction_to_master("predictions_test.csv", df_predictions_test)
#Remember to push afterwards

View(df_predictions_test)


### 6. META META
# 6.1 Logistic regression
lr <- glm(return_customer ~., data = df_predictions_validation_1, family = binomial(link="logit"))
meta.lr <- predict(lr, newdata = df_predictions_test, type = "response")
predictive_performance(test_data$return_customer, pred.lr, cutoff = 0.201)

# 6.2 Random forest

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

rf.meta <- train(return_customer~., 
                 data = df_predictions_validation_1,  
                 method = "rf", 
                 ntree = 500, 
                 tuneGrid = rf.parms, 
                 metric = "ROC", 
                 trControl = model.control)
#Predict
meta.rf   <- predict(rf.meta, newdata = df_predictions_test, type = "prob")[,2]

#xgb meta
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
xgb.default <- train(return_customer~., data = df_predictions_validation_1,  
                     method = "xgbTree",
                     tuneGrid = xgb.parms.default,
                     metric = "ROC", 
                     trControl = model.control)

meta.xgb = xgb.default

#Predict using XGB
xgb.meta <- predict(xgb.default, newdata = df_predictions_test_1, type = "prob")[,2]
predictive_performance(test_data$return_customer, xgb.meta, cutoff = 0.342)

## Saving to a csv
df_predictions_test = call_master("df_predictions_test.csv")
df_predictions_test$meta.rf = meta.xgb #getting an error, need to fix it#
df_predictions_test = save_prediction_to_master("df_predictions_test.csv", df_predictions_test)


