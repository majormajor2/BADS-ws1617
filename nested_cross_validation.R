###### Nested Cross Validation ######


# Run a normal neural network
run_neural_network = function(dataset, fold_membership, model_control, number_of_folds = 5, big_server = FALSE, dropped_correlated_variables = NULL)
{
  #### Setup of parallel backend ####
  # Detect number of available clusters, which gives you the maximum number of "workers" your computer has
  cores = detectCores()
  if(big_server){cores = cores - 1} # use on bigger computers, to leave one core for system
  cl = makeCluster(max(1,cores))
  registerDoParallel(cl)
  message(paste("Registered number of cores:",getDoParWorkers()))
  on.exit(stopCluster(cl))
  #required_packages = c("caret","nnet", "pROC", "klaR")
  #required_functions = c("calculate_woe","apply_woe", "prepare", "strongly_correlated", "predictive_performance", "treat_outliers", "truncate_outliers", "standardize", "normalize", "normalize_dataset")
  
  #### Initialise output lists ####
  object = list()
  
  # Start timing
  print(paste("Started timing at",Sys.time()))
  timing = Sys.time()

  #### Start loops ####
  # Use anormal loop here because otherwise the inner 
  # cross validation with train is not parallelized.
  for(i in 1:number_of_folds) 
  #object <- foreach(i = 1:number_of_folds, .verbose = TRUE) %dopar% # .packages = required_packages, .export = required_functions, .combine = list
  {
      # Sourcing function files - potentially required for foreach loop
      #source("helper.R")
      #source("woe.R")
      #source("performance_measures.R")
      
      print(paste("Begin inner cross validation in fold", i))
      
      #### Split data into training and validation folds ####
      idx_test = which(fold_membership == i)
      #idx_validation = which(fold_membership == ifelse(i == number_of_folds, 1, i+1))
      
      test_fold = dataset[idx_test,]
      #validation_fold = dataset[idx_validation,]
      #train_fold = dataset[-c(idx_test,idx_validation),]
      train_fold = dataset[-idx_test,]
      
      #### Calculate Weight of Evidence ####
      print("Replacing multilevel factors with weight of evidence.")
      # Will create a new dataframe consisting of all the variables of known but replaces the factor
      # variables into numerical variables according to the weight of evidence
      columns_to_replace = c("form_of_address", "email_domain", "model", "payment", "postcode_invoice", "postcode_delivery", "advertising_code")
      # Calculate WoE from train_fold and return woe object
      woe_object = calculate_woe(train_fold, target = "return_customer", columns_to_replace = columns_to_replace)
      # Replace multilevel factor columns in train_fold by their WoE
      train_fold_woe = apply_woe(dataset = train_fold, woe_object = woe_object)
      # Apply WoE to all the other folds 
      #validation_fold_woe = apply_woe(dataset = validation_fold, woe_object = woe_object)
      test_fold_woe = apply_woe(dataset = test_fold, woe_object = woe_object)
      
      #### Normalize folds ####
      if(is.null(dropped_correlated_variables)){dropped_correlated_variables = strongly_correlated(train_fold_woe, threshold = 0.6)}
      
      print("Perform normalization operations.")
      train_fold_woe = prepare(train_fold_woe, dropped_correlated_variables)
      #validation_fold_woe = prepare(validation_fold_woe, dropped_correlated_variables)
      test_fold_woe = prepare(test_fold_woe, dropped_correlated_variables)
      
      #### Create hyperparameter grid ####
      ANN_parms = expand.grid(decay = c(0, 10^seq(-3, 1, 1),25), size = seq(3,45,3))
      
      print("Begin training of primary model.")
      
      #### Train Normal Artificial Neural Network ####
      ANN = train(return_customer~., data = train_fold_woe,  
                  method = "nnet", maxit = 1000, trace = FALSE, # options for nnet function
                  tuneGrid = ANN_parms, # parameters to be tested
                  #tuneLength = 100,
                  metric = "ROC", trControl = model_control)
      
      print("Training of primary model completed.")
      
      #### Predict on validation fold ####
      prediction_ANN = predict(ANN, newdata = test_fold_woe, type = "prob")[,2]
      #result_ANN = predictive_performance(validation_fold_woe$return_customer, prediction_ANN, returnH = FALSE)
      
      print("Prediction by primary model completed.")

      print(paste("Completed all tasks in fold", i, "- Saving now."))
      
      #### Return output of the loop ####
      object[[i]] = list(model = ANN, prediction = prediction_ANN) #, result = result_ANN
  } 

  print(paste("Ended timing at",Sys.time()))
  timing = as.numeric(Sys.time() - timing)
  print(paste("Ended cross validation after", timing, "seconds."))
  
  
  #### Stop parallel computing cluster ####
  # This is taken care of by the on.exit at the beginning
  
  #### End function ####
  return(list(all = object, timing = timing))
}


######## CHECK! ############
performance_neuralnet = data.frame(metrics = c("brier_score","classification_error","h_measure","area_under_curve","gini","precision","true_positives","false_positives","true_negatives","false_negatives","avg_return"))

# Choose neuralnet with best generalisation (i.e. best avg_return given by predictive_performance)
for(i in c(1,2,3,5))
{
  best_cutoff = optimal_cutoff(train_data_norm[which(fold_membership == i),]$return_customer, output_neuralnet$all[[i]]$prediction)
  performance_neuralnet[,paste("Fold",i)] = as.numeric(predictive_performance(train_data_norm[which(fold_membership == i),]$return_customer, output_neuralnet$all[[i]]$prediction, cutoff = best_cutoff, returnH = FALSE))
}

# Now find best tune and save - this will be used to train on the entire known
size = output_neuralnet$all[[2]]$model$bestTune$size # Models from fold 1 and 2 have best AUC and avg_return
decay = output_neuralnet$all[[2]]$model$bestTune$decay # and also the same hyperparameters

# Then test on test_data and add prediction
ANN = nnet(return_customer~., train_data_norm, size=size, decay=decay, maxit = 1000)
neural_predictions_test = as.numeric(predict(ANN, newdata = test_data_norm, type = "raw"))
write.csv(data.frame(return_customer = test_data$return_customer, neural_predictions = neural_predictions_test), "neuralnet_predictions_test.csv")





# Run xgboosting with WoE
run_xgboosting_woe = function(dataset, fold_membership, model_control, number_of_folds = 5, big_server = FALSE, dropped_correlated_variables = NULL)
{
  #### Setup of parallel backend ####
  # Detect number of available clusters, which gives you the maximum number of "workers" your computer has
  cores = detectCores()
  if(big_server){cores = cores - 1} # use on bigger computers, to leave one core for system
  cl = makeCluster(max(1,cores))
  registerDoParallel(cl)
  message(paste("Registered number of cores:",getDoParWorkers()))
  on.exit(stopCluster(cl))
  #required_packages = c("caret","nnet", "pROC", "klaR")
  #required_functions = c("calculate_woe","apply_woe", "prepare", "strongly_correlated", "predictive_performance", "treat_outliers", "truncate_outliers", "standardize", "normalize", "normalize_dataset")
  
  #### Initialise output lists ####
  object = list()
  
  # Start timing
  print(paste("Started timing at",Sys.time()))
  timing = Sys.time()
  
  #### Start loops ####
  # Use anormal loop here because otherwise the inner 
  # cross validation with train is not parallelized.
  for(i in 1:number_of_folds) 
    #object <- foreach(i = 1:number_of_folds, .verbose = TRUE) %dopar% # .packages = required_packages, .export = required_functions, .combine = list
  {
    # Sourcing function files - potentially required for foreach loop
    #source("helper.R")
    #source("woe.R")
    #source("performance_measures.R")
    
    print(paste("Begin inner cross validation in fold", i))
    
    #### Split data into training and validation folds ####
    idx_test = which(fold_membership == i)
    #idx_validation = which(fold_membership == ifelse(i == number_of_folds, 1, i+1))
    
    test_fold = dataset[idx_test,]
    #validation_fold = dataset[idx_validation,]
    #train_fold = dataset[-c(idx_test,idx_validation),]
    train_fold = dataset[-idx_test,]
    
    #### Calculate Weight of Evidence ####
    print("Replacing multilevel factors with weight of evidence.")
    # Will create a new dataframe consisting of all the variables of known but replaces the factor
    # variables into numerical variables according to the weight of evidence
    columns_to_replace = c("form_of_address", "email_domain", "model", "payment", "postcode_invoice", "postcode_delivery", "advertising_code")
    # Calculate WoE from train_fold and return woe object
    woe_object = calculate_woe(train_fold, target = "return_customer", columns_to_replace = columns_to_replace)
    # Replace multilevel factor columns in train_fold by their WoE
    train_fold_woe = apply_woe(dataset = train_fold, woe_object = woe_object)
    # Apply WoE to all the other folds 
    #validation_fold_woe = apply_woe(dataset = validation_fold, woe_object = woe_object)
    test_fold_woe = apply_woe(dataset = test_fold, woe_object = woe_object)
    
    #### Normalize folds ####
    if(is.null(dropped_correlated_variables)){dropped_correlated_variables = strongly_correlated(train_fold_woe, threshold = 0.6)}
    
    print("Perform normalization operations.")
    train_fold_woe = prepare(train_fold_woe, dropped_correlated_variables)
    #validation_fold_woe = prepare(validation_fold_woe, dropped_correlated_variables)
    test_fold_woe = prepare(test_fold_woe, dropped_correlated_variables)
    
    #### Create hyperparameter grid ####
    parameters = expand.grid(nrounds = c(20, 40, 60, 80, 200, 800), 
                             max_depth = c(2, 4, 6), 
                             eta = c(0.001, 0.01, 0.05, 0.1, 0.15, 0.2),
                             gamma = 0,
                             colsample_bytree = c(0.5, 0.8, 1),
                             min_child_weight = 1,
                             subsample = 0.8)
    
    print("Begin training of primary model.")
    
    #### Train Model ####
    model = train(return_customer~., data = train_fold_woe,  
                method = "xgbTree",
                tuneGrid = parameters, # parameters to be tested
                #tuneLength = 100,
                metric = "ROC", trControl = model_control)
    
    print("Training of primary model completed.")
    
    #### Predict on validation fold ####
    prediction = predict(model, newdata = test_fold_woe, type = "prob")[,2]
    #result_ANN = predictive_performance(validation_fold_woe$return_customer, prediction_ANN, returnH = FALSE)
    
    print("Prediction by primary model completed.")
    
    print(paste("Completed all tasks in fold", i, "- Saving now."))
    
    #### Return output of the loop ####
    object[[i]] = list(model = model, prediction = prediction) #, result = result_ANN
  } 
  
  print(paste("Ended timing at",Sys.time()))
  timing = as.numeric(Sys.time() - timing)
  print(paste("Ended cross validation after", timing, "seconds."))
  
  
  #### Stop parallel computing cluster ####
  # This is taken care of by the on.exit at the beginning
  
  #### End function ####
  return(list(all = object, timing = timing))
}



# Run xgboosting
run_xgboosting = function(dataset, fold_membership, model_control, number_of_folds = 5, big_server = FALSE, dropped_correlated_variables = NULL)
{
  #### Setup of parallel backend ####
  # Detect number of available clusters, which gives you the maximum number of "workers" your computer has
  cores = detectCores()
  if(big_server){cores = cores - 1} # use on bigger computers, to leave one core for system
  cl = makeCluster(max(1,cores))
  registerDoParallel(cl)
  message(paste("Registered number of cores:",getDoParWorkers()))
  on.exit(stopCluster(cl))

  #### Initialise output lists ####
  object = list()
  
  # Start timing
  print(paste("Started timing at",Sys.time()))
  timing = Sys.time()
  
  #### Start loops ####
  # Use anormal loop here because otherwise the inner 
  # cross validation with train is not parallelized.
  for(i in 1:number_of_folds) 
    #object <- foreach(i = 1:number_of_folds, .verbose = TRUE) %dopar% # .packages = required_packages, .export = required_functions, .combine = list
  {
    # Sourcing function files - potentially required for foreach loop
    #source("helper.R")
    #source("woe.R")
    #source("performance_measures.R")
    
    print(paste("Begin inner cross validation in fold", i))
    
    #### Split data into training and validation folds ####
    idx_test = which(fold_membership == i)
    #idx_validation = which(fold_membership == ifelse(i == number_of_folds, 1, i+1))
    
    test_fold = dataset[idx_test,]
    #validation_fold = dataset[idx_validation,]
    #train_fold = dataset[-c(idx_test,idx_validation),]
    train_fold = dataset[-idx_test,]
    
    #### Normalize folds ####
    if(is.null(dropped_correlated_variables)){dropped_correlated_variables = strongly_correlated(train_fold_woe, threshold = 0.6)}
    train_fold[,dropped_correlated_variables] = NULL
    test_fold[,dropped_correlated_variables] = NULL
    
    #### Create hyperparameter grid ####
    parameters = expand.grid(nrounds = c(20, 40, 60, 80, 200, 800), 
                             max_depth = c(2, 4, 6), 
                             eta = c(0.001, 0.01, 0.05, 0.1, 0.15, 0.2),
                             gamma = 0,
                             colsample_bytree = c(0.5, 0.8, 1),
                             min_child_weight = 1,
                             subsample = 0.8)
    
    print("Begin training of primary model.")
    
    #### Train Model ####
    model = train(return_customer~., data = train_fold,  
                  method = "xgbTree",
                  tuneGrid = parameters, # parameters to be tested
                  #tuneLength = 100,
                  metric = "ROC", trControl = model_control)
    
    print("Training of primary model completed.")
    
    #### Predict on validation fold ####
    prediction = predict(model, newdata = test_fold, type = "prob")[,2]
    #result_ANN = predictive_performance(validation_fold_woe$return_customer, prediction_ANN, returnH = FALSE)
    
    print("Prediction by primary model completed.")
    
    print(paste("Completed all tasks in fold", i, "- Saving now."))
    
    #### Return output of the loop ####
    object[[i]] = list(model = model, prediction = prediction) #, result = result_ANN
  } 
  
  print(paste("Ended timing at",Sys.time()))
  timing = as.numeric(Sys.time() - timing)
  print(paste("Ended cross validation after", timing, "seconds."))
  
  
  #### Stop parallel computing cluster ####
  # This is taken care of by the on.exit at the beginning
  
  #### End function ####
  return(list(all = object, timing = timing))
}



# Run random forest
run_random_forest = function(dataset, fold_membership, model_control, number_of_folds = 5, big_server = FALSE, dropped_correlated_variables = NULL)
{
  #### Setup of parallel backend ####
  # Detect number of available clusters, which gives you the maximum number of "workers" your computer has
  cores = detectCores()
  if(big_server){cores = cores - 1} # use on bigger computers, to leave one core for system
  cl = makeCluster(max(1,cores))
  registerDoParallel(cl)
  message(paste("Registered number of cores:",getDoParWorkers()))
  on.exit(stopCluster(cl))
  #required_packages = c("caret","nnet", "pROC", "klaR")
  #required_functions = c("calculate_woe","apply_woe", "prepare", "strongly_correlated", "predictive_performance", "treat_outliers", "truncate_outliers", "standardize", "normalize", "normalize_dataset")
  
  #### Initialise output lists ####
  object = list()
  
  # Start timing
  print(paste("Started timing at",Sys.time()))
  timing = Sys.time()
  
  #### Start loops ####
  # Use anormal loop here because otherwise the inner 
  # cross validation with train is not parallelized.
  for(i in 1:number_of_folds) 
    #object <- foreach(i = 1:number_of_folds, .verbose = TRUE) %dopar% # .packages = required_packages, .export = required_functions, .combine = list
  {
    # Sourcing function files - potentially required for foreach loop
    #source("helper.R")
    #source("woe.R")
    #source("performance_measures.R")
    
    print(paste("Begin inner cross validation in fold", i))
    
    #### Split data into training and validation folds ####
    idx_test = which(fold_membership == i)
    #idx_validation = which(fold_membership == ifelse(i == number_of_folds, 1, i+1))
    
    test_fold = dataset[idx_test,]
    #validation_fold = dataset[idx_validation,]
    #train_fold = dataset[-c(idx_test,idx_validation),]
    train_fold = dataset[-idx_test,]
    
    #### Calculate Weight of Evidence ####
    print("Replacing multilevel factors with weight of evidence.")
    # Will create a new dataframe consisting of all the variables of known but replaces the factor
    # variables into numerical variables according to the weight of evidence
    columns_to_replace = c("form_of_address", "email_domain", "model", "payment", "postcode_invoice", "postcode_delivery", "advertising_code")
    # Calculate WoE from train_fold and return woe object
    woe_object = calculate_woe(train_fold, target = "return_customer", columns_to_replace = columns_to_replace)
    # Replace multilevel factor columns in train_fold by their WoE
    train_fold_woe = apply_woe(dataset = train_fold, woe_object = woe_object)
    # Apply WoE to all the other folds 
    #validation_fold_woe = apply_woe(dataset = validation_fold, woe_object = woe_object)
    test_fold_woe = apply_woe(dataset = test_fold, woe_object = woe_object)
    
    #### Normalize folds ####
    if(is.null(dropped_correlated_variables)){dropped_correlated_variables = strongly_correlated(train_fold_woe, threshold = 0.6)}
    
    print("Perform normalization operations.")
    train_fold_woe = prepare(train_fold_woe, dropped_correlated_variables)
    #validation_fold_woe = prepare(validation_fold_woe, dropped_correlated_variables)
    test_fold_woe = prepare(test_fold_woe, dropped_correlated_variables)
    
    #### Create hyperparameter grid ####
    parameters = expand.grid(mtry = 1:10)
    
    print("Begin training of primary model.")
    
    #### Train Model ####
    model = train(return_customer~., data = train_fold_woe,  
                  method = "rf", ntree = 500,
                  tuneGrid = parameters, # parameters to be tested
                  #tuneLength = 100,
                  metric = "ROC", trControl = model_control)
    
    print("Training of primary model completed.")
    
    #### Predict on validation fold ####
    prediction = predict(model, newdata = test_fold_woe, type = "prob")[,2]
    #result_ANN = predictive_performance(validation_fold_woe$return_customer, prediction_ANN, returnH = FALSE)
    
    print("Prediction by primary model completed.")
    
    print(paste("Completed all tasks in fold", i, "- Saving now."))
    
    #### Return output of the loop ####
    object[[i]] = list(model = model, prediction = prediction) #, result = result_ANN
  } 
  
  print(paste("Ended timing at",Sys.time()))
  timing = as.numeric(Sys.time() - timing)
  print(paste("Ended cross validation after", timing, "seconds."))
  
  
  #### Stop parallel computing cluster ####
  # This is taken care of by the on.exit at the beginning
  
  #### End function ####
  return(list(all = object, timing = timing))
}


# Run logistic regression
run_logistic = function(dataset, fold_membership, model_control, number_of_folds = 5, big_server = FALSE, dropped_correlated_variables = NULL)
{
  #### Setup of parallel backend ####
  # Detect number of available clusters, which gives you the maximum number of "workers" your computer has
  cores = detectCores()
  if(big_server){cores = cores - 1} # use on bigger computers, to leave one core for system
  cl = makeCluster(max(1,cores))
  registerDoParallel(cl)
  message(paste("Registered number of cores:",getDoParWorkers()))
  on.exit(stopCluster(cl))
  #required_packages = c("caret","nnet", "pROC", "klaR")
  #required_functions = c("calculate_woe","apply_woe", "prepare", "strongly_correlated", "predictive_performance", "treat_outliers", "truncate_outliers", "standardize", "normalize", "normalize_dataset")
  
  #### Initialise output lists ####
  object = list()
  
  # Start timing
  print(paste("Started timing at",Sys.time()))
  timing = Sys.time()
  
  #### Start loops ####
  # Use anormal loop here because otherwise the inner 
  # cross validation with train is not parallelized.
  for(i in 1:number_of_folds) 
    #object <- foreach(i = 1:number_of_folds, .verbose = TRUE) %dopar% # .packages = required_packages, .export = required_functions, .combine = list
  {
    # Sourcing function files - potentially required for foreach loop
    #source("helper.R")
    #source("woe.R")
    #source("performance_measures.R")
    
    print(paste("Begin inner cross validation in fold", i))
    
    #### Split data into training and validation folds ####
    idx_test = which(fold_membership == i)
    #idx_validation = which(fold_membership == ifelse(i == number_of_folds, 1, i+1))
    
    test_fold = dataset[idx_test,]
    #validation_fold = dataset[idx_validation,]
    #train_fold = dataset[-c(idx_test,idx_validation),]
    train_fold = dataset[-idx_test,]
    
    #### Calculate Weight of Evidence ####
    print("Replacing multilevel factors with weight of evidence.")
    # Will create a new dataframe consisting of all the variables of known but replaces the factor
    # variables into numerical variables according to the weight of evidence
    columns_to_replace = c("form_of_address", "email_domain", "model", "payment", "postcode_invoice", "postcode_delivery", "advertising_code")
    # Calculate WoE from train_fold and return woe object
    woe_object = calculate_woe(train_fold, target = "return_customer", columns_to_replace = columns_to_replace)
    # Replace multilevel factor columns in train_fold by their WoE
    train_fold_woe = apply_woe(dataset = train_fold, woe_object = woe_object)
    # Apply WoE to all the other folds 
    #validation_fold_woe = apply_woe(dataset = validation_fold, woe_object = woe_object)
    test_fold_woe = apply_woe(dataset = test_fold, woe_object = woe_object)
    
    #### Normalize folds ####
    if(is.null(dropped_correlated_variables)){dropped_correlated_variables = strongly_correlated(train_fold_woe, threshold = 0.6)}
    
    print("Perform normalization operations.")
    train_fold_woe = prepare(train_fold_woe, dropped_correlated_variables)
    #validation_fold_woe = prepare(validation_fold_woe, dropped_correlated_variables)
    test_fold_woe = prepare(test_fold_woe, dropped_correlated_variables)
    
    #### Create hyperparameter grid ####
    # None for logistic regression
    
    print("Begin training of primary model.")
    
    #### Train Model ####
    model = glm(return_customer ~ ., data = train_fold_woe, family = binomial(link = "logit"))
    
    print("Training of primary model completed.")
    
    #### Predict on validation fold ####
    prediction = predict(model, newdata = test_fold_woe, type = "response")
    
    print("Prediction by primary model completed.")
    
    print(paste("Completed all tasks in fold", i, "- Saving now."))
    
    #### Return output of the loop ####
    object[[i]] = list(model = model, prediction = prediction) #, result = result_ANN
  } 
  
  print(paste("Ended timing at",Sys.time()))
  timing = as.numeric(Sys.time() - timing)
  print(paste("Ended cross validation after", timing, "seconds."))
  
  
  #### Stop parallel computing cluster ####
  # This is taken care of by the on.exit at the beginning
  
  #### End function ####
  return(list(all = object, timing = timing))
}


# Run a deep neural network
run_deep_neural_network = function(dataset, fold_membership, model_control, number_of_folds = 5, big_server = FALSE)
{
  #### Setup of parallel backend ####
  # Detect number of available clusters, which gives you the maximum number of "workers" your computer has
  cores = detectCores()
  if(big_server){cores = cores - 1} # use on bigger computers, to leave one core for system
  cl = makeCluster(max(1,cores))
  registerDoParallel(cl)
  message(paste("Registered number of cores:",getDoParWorkers()))
  on.exit(stopCluster(cl))
  #required_packages = c("caret","nnet", "pROC", "klaR")
  #required_functions = c("calculate_woe","apply_woe", "prepare", "strongly_correlated", "predictive_performance", "treat_outliers", "truncate_outliers", "standardize", "normalize", "normalize_dataset")
  
  #### Initialise output lists ####
  object = list()
  
  # Start timing
  print(paste("Started timing at",Sys.time()))
  timing = Sys.time()
  
  #### Start loops ####
  # Use anormal loop here because otherwise the inner 
  # cross validation with train is not parallelized.
  for(i in 1:number_of_folds) 
    #object <- foreach(i = 1:number_of_folds, .verbose = TRUE) %dopar% # .packages = required_packages, .export = required_functions, .combine = list
  {
    # Sourcing function files - potentially required for foreach loop
    #source("helper.R")
    #source("woe.R")
    #source("performance_measures.R")
    
    print(paste("Begin inner cross validation in fold", i))
    
    #### Split data into training and validation folds ####
    idx_test = which(fold_membership == i)
    #idx_validation = which(fold_membership == ifelse(i == number_of_folds, 1, i+1))
    
    test_fold = dataset[idx_test,]
    #validation_fold = dataset[idx_validation,]
    #train_fold = dataset[-c(idx_test,idx_validation),]
    train_fold = dataset[-idx_test,]
    
    #### Calculate Weight of Evidence ####
    print("Replacing multilevel factors with weight of evidence.")
    # Will create a new dataframe consisting of all the variables of known but replaces the factor
    # variables into numerical variables according to the weight of evidence
    columns_to_replace = c("form_of_address", "email_domain", "model", "payment", "postcode_invoice", "postcode_delivery", "advertising_code")
    # Calculate WoE from train_fold and return woe object
    woe_object = calculate_woe(train_fold, target = "return_customer", columns_to_replace = columns_to_replace)
    # Replace multilevel factor columns in train_fold by their WoE
    train_fold_woe = apply_woe(dataset = train_fold, woe_object = woe_object)
    # Apply WoE to all the other folds 
    #validation_fold_woe = apply_woe(dataset = validation_fold, woe_object = woe_object)
    test_fold_woe = apply_woe(dataset = test_fold, woe_object = woe_object)
    
    #### Normalize folds ####
    
    dropped_correlated_variables = strongly_correlated(train_fold_woe, threshold = 0.6)
    
    print("Perform normalization operations.")
    train_fold_woe = prepare(train_fold_woe, dropped_correlated_variables)
    #validation_fold_woe = prepare(validation_fold_woe, dropped_correlated_variables)
    test_fold_woe = prepare(test_fold_woe, dropped_correlated_variables)
    
    #### Create hyperparameter grid ####
    DANNII_parms = expand.grid(layer1 = seq(12, 36, 3), layer2 = seq(3, 24, 3), layer3 = seq(1, 10, 1), hidden_dropout = c(0, 10^seq(-4, 1, 1)), visible_dropout = c(0, 10^seq(-4, 1, 1)))
    
    print("Begin training of primary model.")
    
    #### Train Normal Artificial Neural Network ####
    DANNII = train(return_customer~., data = train_fold_woe,  
                method = "dnn", maxit = 100, trace = FALSE, # options for nnet function
                tuneGrid = DANNII_parms, # parameters to be tested
                #tuneLength = 100,
                metric = "ROC", trControl = model_control)
    
    print("Training of primary model completed.")
    
    #### Predict on validation fold ####
    prediction_DANNII = predict(DANNII, newdata = test_fold_woe, type = "prob")[,2]
    #result_DANNII = predictive_performance(validation_fold_woe$return_customer, prediction_DANNII, returnH = FALSE)
    
    print("Prediction by primary model completed.")
    
    print(paste("Completed all tasks in fold", i, "- Saving now."))
    
    #### Return output of the loop ####
    object[[i]] = list(model = DANNII, prediction = prediction_DANNII) #, result = result_DANNII
  } 
  
  print(paste("Ended timing at",Sys.time()))
  timing = as.numeric(Sys.time() - timing)
  print(paste("Ended cross validation after", timing, "seconds."))
  
  
  #### Stop parallel computing cluster ####
  # This is taken care of by the on.exit at the beginning
  
  #### End function ####
  return(list(all = object, timing = timing))
}














#################### After this there be dragons! ##########################
# What follows is just a testing ground and will be deleted at some point. #


# Run a normal neural network
run_neural_network_old = function(dataset, fold_membership, model_control, number_of_folds = 5, runWoE = TRUE, perform_normalization = TRUE, big_server = FALSE)
{
  #### Setup of parallel backend ####
  # Detect number of available clusters, which gives you the maximum number of "workers" your computer has
  cores = detectCores()
  if(big_server){cores = cores - 1} # use on bigger computers, to leave one core for system
  cl = makeCluster(max(1,cores))
  registerDoParallel(cl)
  message(paste("Registered number of cores:",getDoParWorkers()))
  packages_neuralnet = c("caret","nnet", "pROC")
  
  #### Initialise output lists ####
  
  # Start timing
  print(paste("Started timing at",date()))
  timing = system.time( 
    #### Start loops ####
    #for(i in 1:number_of_folds)
    object <- foreach(i = 1:number_of_folds, .combine = list, .packages = packages_neuralnet, .verbose = TRUE) %dopar%
    {
      
      print(paste("Begin inner cross validation in fold", i))
      
      #### Split data into training and validation folds ####
      idx_test = which(fold_membership == i)
      idx_validation = which(fold_membership == ifelse(i == number_of_folds, 1, i+1))
      
      test_fold = dataset[idx_test,]
      validation_fold = dataset[idx_validation,]
      train_fold = dataset[-c(idx_test,idx_validation),]
      
      #### Calculate Weight of Evidence ####
      print("Replacing multilevel factors with weight of evidence.")
      # Will create a new dataframe consisting of all the variables of known but replaces the factor
      # variables into numerical variables according to the weight of evidence
      columns_to_replace = c("form_of_address", "email_domain", "model", "payment", "postcode_invoice", "postcode_delivery", "advertising_code")
      # Calculate WoE from train_fold and return woe object
      woe_object = calculate_woe(train_fold, target = "return_customer", columns_to_replace = columns_to_replace)
      # Replace multilevel factor columns in train_fold by their WoE
      train_fold_woe = apply_woe(dataset = train_fold, woe_object = woe_object)
      # Apply WoE to all the other folds 
      validation_fold_woe = apply_woe(dataset = validation_fold, woe_object = woe_object)
      test_fold_woe = apply_woe(dataset = test_fold, woe_object = woe_object)
      
      #### Normalize folds ####
      
      dropped_correlated_variables = strongly_correlated(train_fold_woe, threshold = 0.6)
      
      print("Perform normalization operations.")
      train_fold_woe = prepare(train_fold_woe)
      validation_fold_woe = prepare(validation_fold_woe)
      test_fold_woe = prepare(test_fold_woe)
      
      #### Create hyperparameter grid ####
      ANN_parms = expand.grid(decay = c(0, 10^seq(-5, 1, 1)), size = seq(3,45,3))
      
      print("Begin training of primary model.")
      
      #### Train Normal Artificial Neural Network ####
      ANN = train(return_customer~., data = train_fold_woe,  
                  method = "nnet", maxit = 1000, trace = FALSE, # options for nnet function
                  tuneGrid = ANN_parms, # parameters to be tested
                  #tuneLength = 100,
                  metric = "ROC", trControl = model_control)
      
      print("Training of primary model completed.")
      
      #### Predict on validation fold ####
      prediction_ANN = predict(ANN, newdata = validation_fold_woe, type = "prob")[,2]
      result_ANN = predictive_performance(validation_fold_woe$return_customer, prediction_ANN, returnH = FALSE)
      
      print("Prediction by primary model completed.")

      print(paste("Completed all tasks in fold", i, "- Saving now."))
      
      #### Return output of the loop ####
      list(model = ANN, prediction = prediction_ANN, result = result_ANN) 
    } 
  )[3]   # End timing
  print(paste("Ended cross validation after", timing))
  
  
  #### Stop parallel computing cluster ####
  on.exit(stopCluster(cl))
  
  #### End function ####
  return(list(all = object, timing = timing))
}


train_network = function(method, dataset, model_control)
{
  if(method %in% c("nnet", "pca", "avNNet"))
  {
    preprocessing = NULL
    if(method == "pcaNNet"){method = "nnet" ; preprocessing = "pca"}
    # Normal Artificial Neural Network
    output = train(return_customer~., data = dataset,  
              method = method, maxit = 1000, trace = FALSE, # options for nnet function
              preProc = preprocessing,
              tuneLength = 100,
              metric = "ROC", trControl = model_control)
  }
  else
  {
    output = train(return_customer~., data = dataset,  
                method = method, 
                tuneLength = 150,
                metric = "ROC", trControl = model_control)
  }
  
  return(output)
}


run_neural_networks_parallel_parallel = function(dataset, fold_membership, model_control, number_of_folds = 5)
{
  
  # Initialise output lists
  models = list()
  predictions = list()
  results = list()
  output = list()
  packages_neuralnet = c("caret","nnet", "deepnet", "neuralnet", "FCNN4R", "plyr", "pROC")
  nets = c("nnet", "pcaNNet", "avNNet", "neuralnet", "dnn", "mlpSGD")
  
  # Start timing
  timing = system.time( 
    
  fold_output <- foreach(i = 1:number_of_folds, .combine = list, .packages = packages_neuralnet, .verbose = TRUE, .export = "train_network") %:% foreach(net = nets, .combine = list, .packages = packages_neuralnet, .verbose = TRUE, .export = "train_network") %dopar%
  {
      print(paste("Begin inner cross validation in fold", i))
      
      # Split data into training and validation folds
      idx_test = which(fold_membership == i)
      idx_validation = which(fold_membership == ifelse(i == number_of_folds, 1, i+1))
      
      test_fold = dataset[idx_test,]
      validation_fold = dataset[idx_validation,]
      train_fold = dataset[-c(idx_test,idx_validation),]
      
      print("Begin training of primary model.")

      output = list(model = train_network(net, train_fold, model_control), prediction = predict(model, newdata = validation_fold, type = "raw"))

      print("Training of primary model completed.")
      print("Prediction by primary model completed.")
      print(paste("Completed all tasks in fold", i))
      
      output
    } 
  )[3]   # End timing
  print(paste("Ended cross validation after", timing))
  
  # Stop the parallel computing cluster
  on.exit(stopCluster(cl))
  
  return(list(folds = fold_output, timing = timing))
}





run_neural_network_all = function(dataset, fold_membership, model_control, number_of_folds = 5)
{

  # Initialise output lists
  models = list()
  predictions = list()
  results = list()
  output = list()
   
  # Start timing
  timing = system.time( 
  for(i in 1:number_of_folds)
  {

    print(paste("Begin inner cross validation in fold", i))
    
    # Split data into training and validation folds
    idx_test = which(fold_membership == i)
    idx_validation = which(fold_membership == ifelse(i == k, 1, i+1))
    
    test_fold = dataset[idx_test,]
    validation_fold = dataset[idx_validation,]
    train_fold = dataset[-c(idx_test,idx_validation),]
    
    print("Begin training of primary model.")
    
    # Normal Artificial Neural Network
    ANN = train(return_customer~., data = train_fold,  
                  method = "nnet", maxit = 1000, trace = FALSE, # options for nnet function
                  #tuneGrid = ANN_parms, # parameters to be tested
                  tuneLength = 100,
                  metric = "ROC", trControl = model_control)
    
    # Artificial Neural Network with Principal Component Analysis
    pcANN = train(return_customer~., data = train_fold,  
                method = "nnet", maxit = 1000, trace = FALSE, # options for nnet function
                preProc = "pca", # using principal component analysis
                #tuneGrid = pcANN_parms, # parameters to be tested
                tuneLength = 100,
                metric = "ROC", trControl = model_control)
    
    # Model Averaged Neural Network
    avANN = train(return_customer~., data = train_fold,  
                method = "avNNet", maxit = 1000, trace = FALSE, # options for nnet function
                #tuneGrid = avANN_parms, # parameters to be tested
                tuneLength = 100,
                metric = "ROC", trControl = model_control)
    
    # Deep Artificial Neural Network (using package neuralnet)
    DANN = train(return_customer~., data = train_fold,  
                method = "neuralnet", 
                #tuneGrid = DANN_parms, # parameters to be tested
                tuneLength = 150,
                metric = "ROC", trControl = model_control)
    
    # Deep Artificial Neural Network (using package deepnet)
    DANNII = train(return_customer~., data = train_fold,  
                 method = "dnn", 
                 #tuneGrid = DANNII_parms, # parameters to be tested
                 tuneLength = 150,
                 metric = "ROC", trControl = model_control)
    
    # Multilayer Perceptron Network by Stochastic Gradient Descent
    # parameters: size, l2reg, lambda, learn_rate, momentum, gamma, minibatchsz, repeats
    MLP = train(return_customer~., data = train_fold,  
                method = "mlpSGD", 
                #tuneGrid = MLP_parms, # parameters to be tested
                tuneLength = 150,
                metric = "ROC", trControl = model_control)
    
    print("Training of primary model completed.")
    
    prediction_ANN = predict(ANN, newdata = validation_fold, type = "raw")
    prediction_avANN = predict(avANN, newdata = validation_fold, type = "raw")
    prediction_pcANN = predict(pcANN, newdata = validation_fold, type = "raw")
    prediction_DANN = predict(DANN, newdata = validation_fold, type = "raw")
    prediction_DANNII = predict(DANNII, newdata = validation_fold, type = "raw")
    prediction_MLP = predict(MLP, newdata = validation_fold, type = "raw")
    
    print("Prediction by primary model completed.")
    
    print(paste("Completed all tasks in fold", i))
    
    models[i] = ANN
    predictions[i] = prediction_ANN
    results[i] = predictive_performance(validation_fold$return_customer, prediction_ANN, returnH = FALSE)
    output[i] = list(ANN = ANN, prediction_ANN = prediction_ANN)
  } 
)[3]   # End timing
print(paste("Ended cross validation after", timing))


# Stop the parallel computing cluster
stopCluster(cl)

return(list(models = models, predictions = predictions, results = results, all = output, timing = timing))
}