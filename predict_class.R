# Make predictions for the class dataset with the 5 primary models we have selected in earlier stages
predict_class = function(filename = "class_predictions.csv", save_to_csv = TRUE)
{
  # Set up parallel computing
  cores = detectCores()
  cl = makeCluster(max(1,cores))
  registerDoParallel(cl)
  message(paste("Registered number of cores:",getDoParWorkers()))
  on.exit(stopCluster(cl))
  
  # Initialise model control
  model.control = trainControl(
    method = "cv", # 'cv' for cross validation, 'adaptive_cv' drops unpromising models
    number = 5, # number of folds in cross validation (or number of resampling iterations)
    #repeats = 5, # number of repeats for repeated cross validation
    search = "grid", # or grid for a grid search
    classProbs = TRUE,
    summaryFunction = stephanie.cutoff,
    #sampling = "smote", # This resolves class imbalances. 
    # Possible values are "none", "down", "up", "smote", or "rose". The latter two values require the DMwR and ROSE packages, respectively.
    allowParallel = TRUE, # Enable parallelization if available
    returnData = FALSE) # The training data will not be included in the output training object

  # Train models
  print("Begin training xgb")
  xgb_parms_cphsv = expand.grid(nrounds = 800,
                           max_depth = 4,
                           eta = 0.01,
                           gamma = 0,
                           colsample_bytree = 1,
                           min_child_weight = 1,
                           subsample = 0.8)
  
  xgb_mdl_cphsv = train(return_customer~.,
                        data = known,
                        method = "xgbTree",
                        tuneGrid = xgb_parms_cphsv,
                        metric = "ROC",
                        trControl = model.control)

  print("Begin training xgb_woe")
  xgb_woe_parms_cphsv = expand.grid(nrounds = 200,
                               max_depth = 4,
                               eta = 0.05,
                               gamma = 0,
                               colsample_bytree = 1,
                               min_child_weight = 1,
                               subsample = 0.8)
  
  xgb_woe_mdl_cphsv = train(return_customer~.,
                       data = known_woe,
                       method = "xgbTree",
                       tuneGrid = xgb_woe_parms_cphsv,
                       metric = "ROC",
                       trControl = model.control)

  print("Begin training logistic")
  lr_woe_mdl_cphsv = glm(return_customer~., data = known_woe, family = binomial(link = "logit"))
  print("Begin training random_forest")
  rf_woe_mdl_cphsv = randomForest(return_customer~., data = known_woe, mtry = 8)
  print("Begin training neuralnet")
  neuralnet = nnet(return_customer ~ ., data = known_norm, size = 12, decay = 10, maxit = 1000)

  # predictions
  print("Predicting class")
  xgb_pred_cphsv = predict(xgb_mdl_cphsv, newdata = class, type = "prob")[,2]
  xgb_woe_pred_cphsv = predict(xgb_woe_mdl_cphsv, newdata = class_woe, type = "prob")[,2]
  lr_woe_pred_cphsv = predict(lr_woe_mdl_cphsv, newdata = class_woe, type = "response")
  rf_woe_pred_cphsv = predict(rf_woe_mdl_cphsv, newdata = class_woe, type = "prob")[,2]
  prediction_neuralnet = as.numeric(predict(neuralnet, newdata = class_norm, type = "raw"))

  # create data frame with all the predictions
  df_predictions_class = data.frame(xgb = xgb_pred_cphsv, 
                                  xgb_woe  = xgb_woe_pred_cphsv, 
                                  logistic = lr_woe_pred_cphsv, 
                                  random_forest = rf_woe_pred_cphsv,
                                  neuralnet = prediction_neuralnet)

  # save it to csv file
  if(save_to_csv)
  {
    print(paste("Writing to"), filename)
    write.csv(df_predictions_class, file = filename, row.names = row.names(class))
  }
    
  # check correlations between the models
  corrplot(cor(df_predictions_class))
  
  return(df_predictions_class)
}
