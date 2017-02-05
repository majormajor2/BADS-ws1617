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
fold_membership = createFolds(known$return_customer, list = FALSE, k = k)

# Create a data frame of predictions
print("Starting creation of prediction data frame at", Sys.time())

known_predictions = foreach(i = 1:k, .combine = rbind.data.frame, .verbose = TRUE) %dopar% # fold = training_folds, .packages = required_packages, .export = required_functions, .combine = list
{
  # Sourcing function files - potentially required for foreach loop
  source("helper.R")
  source("woe.R")
  source("performance_measures.R")
  
  # Split data into training and prediction folds
  idx_test = which(fold_membership == i)
  test_fold = known[idx_test,]
  train_fold = known[-idx_test,]
  
  # Replace multilevel factors by WoE
  columns_to_replace = c("form_of_address", "email_domain", "model", "payment", "postcode_invoice", "postcode_delivery", "advertising_code")
  woe_object = calculate_woe(train_fold, target = "return_customer", columns_to_replace = columns_to_replace)
  train_fold_woe = apply_woe(dataset = train_fold, woe_object = woe_object)
  test_fold_woe = apply_woe(dataset = test_fold, woe_object = woe_object)

  # Perform normalization
  dropped_correlated_variables = strongly_correlated(train_fold_woe, threshold = 0.6)
  train_fold_norm = prepare(train_fold_woe, dropped_correlated_variables)
  test_fold_norm = prepare(test_fold_woe, dropped_correlated_variables)
  
  # Set hyperparameters (only a single set - so we can use the train function, others are defined in their function)
  xgb_parms = expand.grid(nrounds = 800, max_depth = 4, eta = 0.01, gamma = 0, colsample_bytree = 1, min_child_weight = 1, subsample = 0.8)
  xgb_woe_parms = expand.grid(nrounds = 200, max_depth = 4, eta = 0.05, gamma = 0, colsample_bytree = 1,min_child_weight = 1, subsample = 0.8)
  
  # Train models
  xgb = train(return_customer~., data = train_fold, method = "xgbTree", tuneGrid = xgb_parms, metric = "ROC", trControl = model_control)
  xgb_woe = train(return_customer~., data = train_fold_woe, method = "xgbTree", tuneGrid = xgb_woe_parms, metric = "ROC", trControl = model_control)
  logistic      = glm(return_customer ~ ., data = train_fold_woe, family = binomial(link = "logit"))
  random_forest = randomForest(return_customer ~ ., data = train_fold_woe, mtry = 8)
  neuralnet     = nnet(return_customer ~ ., data = train_fold_norm, size = 3, decay = 1, maxit = 1000)
  
  # Predict return_customer on remainder
  prediction_xgb = predict(xgb, newdata = test_fold, type = "prob")[,2]
  prediction_xgb_woe = predict(xgb_woe, newdata = test_fold_woe, type = "prob")[,2]
  prediction_logistic = predict(logistic, newdata = test_fold_woe, type = "response")
  prediction_random_forest = predict(random_forest, newdata = test_fold_woe, type = "prob")[,2]
  prediction_neuralnet = predict(neuralnet, newdata = test_fold_norm, type = "prob")[,2]
  

  predictions = cbind.data.frame(return_customer = test_fold$return_customer,
                                 xgb = prediction_xgb, xgb_woe = prediction_xgb_woe, 
                                 logistic = prediction_logistic, 
                                 random_forest = prediction_random_forest #,neuralnet =
                                 )

  # Return the list of predictions
  predictions
}
print("Completed creation of prediction data frame at", Sys.time())

# Order the data frame by row.name
known_predictions = known_predictions[order(as.numeric(row.names(known_predictions))),]

# Stop the parallel computing cluster
stopCluster(cl)

# Start a new cluster
cl = makeCluster(max(1,cores))
registerDoParallel(cl)
message(paste("Registered number of cores:",getDoParWorkers()))
on.exit(stopCluster(cl))

# Train the meta model on the predictions dataframe
meta_model = foreach(i = 1:k, .verbose = TRUE) %dopar% # fold = training_folds, .packages = required_packages, .export = required_functions, .combine = rbind.data.frame, 
{
  # Sourcing function files - potentially required for foreach loop
  source("helper.R")
  source("woe.R")
  source("performance_measures.R")
  
  # Split data into training and prediction folds
  idx_test = which(fold_membership == i)
  test_fold = known_predictions[idx_test,]
  train_fold = known_predictions[-idx_test,]

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
  
  # Set hyperparameters (only a single set - so we can use the train function, others are defined in their function)
  parameters = expand.grid(nrounds = 800, max_depth = 4, eta = 0.01, gamma = 0, colsample_bytree = 1, min_child_weight = 1, subsample = 0.8)

  # Train models
  #model = train(return_customer~., data = train_fold, method = "xgbTree", tuneGrid = xgb_parms, metric = "ROC", trControl = model_control)
  model= glm(return_customer ~ ., data = train_fold, family = binomial(link = "logit"))
    
  # Predict return_customer on remainder
  prediction = predict(model, newdata = test_fold, type = "response")
  cutoff = optimal_cutoff(test_fold$return_customer, prediction)
  avg_return = predictive_performance(test_fold$return_customer, prediction, cutoff, returnH = FALSE)$avg_return
  output = list(model = model, cutoff = cutoff, avg_return = avg_return)

  # Return
  return(output)
}


# Initialise to 0
optimal_cutoff_for_class = 0
avg_return = 0
# Add for all folds
for(i in 1:k)
{
  optimal_cutoff_for_class = optimal_cutoff_for_class + meta_model[[i]]$cutoff
  avg_return = avg_return + meta_model[[i]]$avg_return
}
# Take averages
optimal_cutoff_for_class = optimal_cutoff_for_class / k
avg_return = avg_return / k

# Check performance
# First load the old predictions and convert them so that they can be used with the meta model
predictions_test = read.csv("predictions_test.csv")
predictions_test = predictions_test[-1]
predictions_test = predictions_test[c(1,7,10,12,13)]
colnames(predictions_test) = c("return_customer","xgb_woe","random_forest","xgb","logistic")

meta_predictions_test = data.frame(return_customer = predictions_test$return_customer)
meta_performance_test = data.frame(metrics = c("brier_score","classification_error","h_measure","area_under_curve","gini","precision","true_positives","false_positives","true_negatives","false_negatives","avg_return"))

for(i in 1:k)
{
  meta_predictions_test[,paste("Fold",i)] = predict(meta_model[[i]]$model, newdata = predictions_test, type = "response")
  meta_performance_test[,paste("Fold",i)] = as.numeric(predictive_performance(predictions_test$return_customer, meta_predictions_test[,paste("Fold",i)], cutoff = meta_model[[i]]$cutoff, returnH = FALSE))


}