####### Predictions for Meta Model ######
# Loop over 5 folds. Within the loop:
# 1. Train the best models (given fixed, previously validated hyperparameters) on 4 folds
# 2. Predict for remainder
# 3. Create data.frame of predictions

#### Setup of parallel backend ####
# Detect number of available clusters, which gives you the maximum number of "workers" your computer has
big_server = TRUE
cores = detectCores()
if(big_server){cores = cores - 1} # use on bigger computers, to leave one core for system
cl = makeCluster(max(1,cores))
registerDoParallel(cl)
message(paste("Registered number of cores:",getDoParWorkers()))
on.exit(stopCluster(cl))

###### Initialise model control ######
model_control = trainControl(
  method = "cv", # 'cv' for cross validation, 'adaptive_cv' drops unpromising models
  number = 5, # number of folds in cross validation (or number of resampling iterations)
  #repeats = 5, # number of repeats for repeated cross validation
  search = "grid", # or grid for a grid search
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  #timingSamps = length(fold), # number of samples to predict the time taken
  sampling = "smote", # This resolves class imbalances. 
  # Possible values are "none", "down", "up", "smote", or "rose". The latter two values require the DMwR and ROSE packages, respectively.
  allowParallel = TRUE, # Enable parallelization if available
  #savePredictions = TRUE, # Save the hold-out predictions
  verboseIter = TRUE, # Print training log
  returnData = FALSE) # The training data will not be included in the output training object


# Set number of folds
k = 5
# Set seed for reproducability
set.seed(123)
# Define fold membership for cross validation
fold_membership = createFolds(train_data$return_customer, list = FALSE, k = k)


predictions_known = foreach(i = 1:k, .combine = rbind.data.frame, .verbose = TRUE) %dopar% # fold = training_folds, .packages = required_packages, .export = required_functions, .combine = list
{
  # Sourcing function files - potentially required for foreach loop
  source("helper.R")
  source("woe.R")
  source("performance_measures.R")
  
  # Split data into training and prediction folds
  idx_test = which(fold_membership == i)
  test_fold = train_data[idx_test,]
  train_fold = train_data[-idx_test,]
  
  # Replace multilevel factors by WoE
  columns_to_replace = c("form_of_address", "email_domain", "model", "payment", "postcode_invoice", "postcode_delivery", "advertising_code")
  woe_object = calculate_woe(train_fold, target = "return_customer", columns_to_replace = columns_to_replace)
  train_fold_woe = apply_woe(dataset = train_fold, woe_object = woe_object)
  test_fold_woe = apply_woe(dataset = test_fold, woe_object = woe_object)

  # Set hyperparameters (only a single set - so we can use the train function, others are defined in their function)
  xgb_parms = expand.grid(nrounds = 800, max_depth = 4, eta = 0.01, gamma = 0, colsample_bytree = 1, min_child_weight = 1, subsample = 0.8)
  xgb_woe_parms = expand.grid(nrounds = 200, max_depth = 4, eta = 0.05, gamma = 0, colsample_bytree = 1,min_child_weight = 1, subsample = 0.8)
  
  # Train models
  xgb = train(return_customer~., data = train_fold, method = "xgbTree", tuneGrid = xgb_parms, metric = "ROC", trControl = model_control)
  xgb_woe = train(return_customer~., data = train_fold_woe, method = "xgbTree", tuneGrid = xgb_woe_parms, metric = "ROC", trControl = model_control)
  logistic      = glm(return_customer ~ ., data = train_fold_woe, family = binomial(link = "logit"))
  random_forest = randomForest(return_customer ~ ., data = train_fold_woe, mtry = 8)
  #neuralnet     =
  
  # Predict return_customer on remainder
  prediction_xgb = predict(xgb, newdata = test_fold, type = "prob")[,2]
  prediction_xgb_woe = predict(xgb_woe, newdata = test_fold_woe, type = "prob")[,2]
  prediction_logistic = predict(logistic, newdata = test_fold_woe, type = "response")
  prediction_random_forest = predict(random_forest, newdata = test_fold_woe, type = "prob")[,2]
  #prediction_neuralnet = predict(neuralnet, newdata = test_fold_woe, type = "prob")[,2]
  

  predictions = cbind.data.frame(return_customer = test_fold$return_customer,
                                 xgb = prediction_xgb, xgb_woe = prediction_xgb_woe, 
                                 logistic = prediction_logistic, 
                                 random_forest = prediction_random_forest #,neuralnet =
                                 )
  # Return the list of predictions
  predictions

}

# Order the data frame by row.name
predictions_known = predictions_known[order(as.numeric(row.names(predictions_known))),]

# Stop the parallel computing cluster
stopCluster(cl)