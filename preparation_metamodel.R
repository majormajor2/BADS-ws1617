####### Predictions for Meta Model ######
# Loop over 5 folds. Within the loop:
# 1. Train the best models (given fixed, previously validated hyperparameters) on 4 folds
# 2. Predict for remainder
# 2. 

# Set number of folds
k = 5
# Set seed for reproducability
set.seed(123)
# Create folds for cross validation (these are the big folds 4/5 of total)
training_folds = createFolds(train_data$return_customer, k = k, list = TRUE, returnTrain = TRUE)



predictions_known = foreach(fold = training_folds, .combine = rbind, .verbose = TRUE, outfile = "") %dopar% # .packages = required_packages, .export = required_functions, .combine = list
{
  # Sourcing function files - potentially required for foreach loop
  source("helper.R")
  source("woe.R")
  source("performance_measures.R")
  
  # Split data into training and prediction folds
  train_fold = train_data[fold,]
  test_fold  = train_data[-fold,]
  
  # Replace multilevel factors by WoE
  columns_to_replace = c("form_of_address", "email_domain", "model", "payment", "postcode_invoice", "postcode_delivery", "advertising_code")
  woe_object = calculate_woe(train_fold, target = "return_customer", columns_to_replace = columns_to_replace)
  train_fold_woe = apply_woe(dataset = train_fold, woe_object = woe_object)
  test_fold_woe = apply_woe(dataset = test_fold, woe_object = woe_object)

  # Set hyperparameters
  # Train models
  #xgb           = xgb.train(params = list(max_depth = 4, eta = 0.01, gamma = 0, colsample_bytree = 1, min_child_weight = 1, subsample = 0.8), data = train_fold, nrounds = 800)
  #xgb_woe       = xgb.train(params = list(max_depth = 4, eta = 0.05, gamma = 0, colsample_bytree = 1, min_child_weight = 1, subsample = 0.8), data = train_fold_woe, nrounds = 200)
  logistic      = glm(return_customer ~ ., data = train_fold_woe, family = binomial(link = "logit"))
  #random_forest = randomForest(return_customer ~ ., data = train_fold_woe, mtry = 8)
  #neuralnet     =
  
  # Predict return_customer on remainder
  #prediction_xgb = predict(xgb, newdata = test_fold, type = "prob")[,2]
  #prediction_xgb_woe = predict(xgb_woe, newdata = test_fold_woe, type = "prob")[,2]
  prediction_logistic = predict(logistic, newdata = test_fold_woe, type = "response")
  #prediction_random_forest = predict(random_forest, newdata = test_fold_woe, type = "prob")[,2]
  #prediction_neuralnet = predict(neuralnet, newdata = test_fold_woe, type = "prob")[,2]
  
  # Return a list of predictions
  
  predictions = data.frame(
    #xgb = prediction_xgb, xgb_woe = prediction_xgb_woe, 
                     logistic = prediction_logistic
                     #,random_forest = prediction_random_forest
                     #,neuralnet =
                     )
}