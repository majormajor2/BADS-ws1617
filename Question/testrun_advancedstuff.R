### 1. SETUP
## the options for model selection
model.control<- trainControl(
  method = "cv", # 'cv' for cross validation
  number = 5, # number of folds in cross validation
  classProbs = TRUE,
  summaryFunction = stephanie.cutoff,
  allowParallel = TRUE, # Enable parallelization if available
  returnData = TRUE # We will use this to plot partial dependence
)
## TUNES
xgb.parms.default <- expand.grid(nrounds = c(20, 40, 60, 80), 
                                 max_depth = c(2, 4), 
                                 eta = c(0.01, 0.05, 0.1, 0.15), 
                                 gamma = 0,
                                 colsample_bytree = c(0.8, 1),
                                 min_child_weight = 1,
                                 subsample = 0.8)

# RUN MODEL
xgb_woe.default <- train(return_customer~., 
                         data = train_data_woe,  
                         method = "xgbTree",
                         tuneGrid = xgb.parms.default,
                         metric = "avg_return", 
                         trControl = model.control)