###### Nested Cross Validation ######

# Setup of parallel backend - this should be done in the function !
# Detect number of available clusters, which gives you the maximum number of "workers" your computer has
#no_of_cores = detectCores()
#cl = makeCluster(max(1,no_of_cores-1)) #- use on bigger computers, to leave one core for system
#cl = makeCluster(max(1,no_of_cores))
#registerDoParallel(cl)
#message(paste("Registered number of cores:",getDoParWorkers()))


##### Slice data set into folds #####
# Set number of folds
k = 5
# Set seed for reproducability
set.seed(123)
# Create folds for cross validation (these are the big folds 4/5 of total) - not used in function at the moment
training_folds = createFolds(train_data$return_customer, k = k, list = TRUE, returnTrain = TRUE)
# Set seed for reproducability
set.seed(123)
# Define fold membership for cross validation
fold_membership = createFolds(train_data$return_customer, list = FALSE, k = k)

###### Create hyperparameter grids for the grid search ######
#ANN_parms = expand.grid(decay = c(0, 10^seq(-5, 0, 1)), size = seq(3,30,3))
ANN_parms = expand.grid(decay = c(0, 10^seq(-5, 1, 1)), size = seq(3,45,3))
pcANN_parms = expand.grid(decay = c(0, 10^seq(-5, 1, 1)), size = seq(3,45,3))
avANN_parms = expand.grid(bag = c(FALSE, TRUE), decay = c(0, 10^seq(-5, 1, 1)), size = seq(3,45,3))
DANN_parms = expand.grid(layer1 = seq(5, 50, 3), layer2 = seq(0, 30, 3), layer3 = seq(0, 10, 1))
DANNII_parms = expand.grid(layer1 = seq(5, 50, 3), layer2 = seq(3, 30, 3), layer3 = seq(1, 10, 1), hidden_dropout = c(0, 10^seq(-5, 1, 1)), visible_dropout = c(0, 10^seq(-5, 1, 1)))





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
  savePredictions = TRUE, # Save the hold-out predictions
  verboseIter = TRUE, # Print training log
  returnData = FALSE) # The training data will not be included in the output training object



# Run a normal neural network
run_neural_network = function(dataset, fold_membership, model_control, number_of_folds = 5, big_server = FALSE)
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
  models = list()
  predictions = list()
  results = list()
  object = list()
  
  # Start timing
  print(paste("Started timing at",date()))
  timing = system.time( 
    #### Start loops ####
    #for(i in 1:number_of_folds)
    fold_output <- foreach(i = 1:number_of_folds, .combine = list, .packages = packages_neuralnet, .verbose = TRUE) %dopar%
    {
      
      print(paste("Begin inner cross validation in fold", i))
      
      # Split data into training and validation folds
      idx_test = which(fold_membership == i)
      idx_validation = which(fold_membership == ifelse(i == number_of_folds, 1, i+1))
      
      test_fold = dataset[idx_test,]
      validation_fold = dataset[idx_validation,]
      train_fold = dataset[-c(idx_test,idx_validation),]
      
      # Create hyperparameter grid
      ANN_parms = expand.grid(decay = c(0, 10^seq(-5, 1, 1)), size = seq(3,45,3))
      
      print("Begin training of primary model.")
      
      # Normal Artificial Neural Network
      ANN = train(return_customer~., data = train_fold,  
                  method = "nnet", maxit = 1000, trace = FALSE, # options for nnet function
                  tuneGrid = ANN_parms, # parameters to be tested
                  #tuneLength = 100,
                  metric = "ROC", trControl = model_control)
      
      print("Training of primary model completed.")
      
      prediction_ANN = predict(ANN, newdata = validation_fold, type = "prob")[,2]
      result_ANN = predictive_performance(validation_fold$return_customer, prediction_ANN, returnH = FALSE)
      
      print("Prediction by primary model completed.")
      
      if(runWoE)
      {
        ###### Weight of Evidence ######
        print("Starting calculations with weight of evidence.")
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
        
        print("Begin training of primary model with weight of evidence.")
        
        # Normal Artificial Neural Network with Weights of Evidence
        ANN_woe = train(return_customer~., data = train_fold_woe,  
                        method = "nnet", maxit = 1000, trace = FALSE, # options for nnet function
                        tuneGrid = ANN_parms, # parameters to be tested
                        #tuneLength = 100,
                        metric = "ROC", trControl = model_control)
        
        print("Training of primary model completed.")
        
        prediction_ANN_woe = predict(ANN_woe, newdata = validation_fold_woe, type = "prob")[,2]
        result_ANN_woe = predictive_performance(validation_fold_woe$return_customer, prediction_ANN_woe, returnH = FALSE)
        
        print("Prediction by primary model completed.")
        
        if(perform_normalization)
        {
          dropped_correlated_variables = strongly_correlated(train_fold_woe, threshold = 0.6)
          
          print("Perform normalization operations.")
          train_fold_woe = prepare(train_fold_woe)
          validation_fold_woe = prepare(validation_fold_woe)
          test_fold_woe = prepare(test_fold_woe)
          
          print("Begin training of primary model with normalization.")
          
          # Normal Artificial Neural Network (Normalized)
          ANN_norm = train(return_customer~., data = train_fold_woe,  
                           method = "nnet", maxit = 1000, trace = FALSE, # options for nnet function
                           tuneGrid = ANN_parms, # parameters to be tested
                           #tuneLength = 100,
                           metric = "ROC", trControl = model_control)
          
          print("Training of primary model completed.")
          
          prediction_ANN_norm = predict(ANN_norm, newdata = validation_fold_woe, type = "prob")[,2]
          result_ANN_norm = predictive_performance(validation_fold_woe$return_customer, prediction_ANN_norm, returnH = FALSE)
          
          print("Prediction by primary model completed.")
          
        }
      }
      
      
      print(paste("Completed all tasks in fold", i, "- Saving now."))
      
      models[i] = list(raw = ANN, woe = ANN_woe, normalized = ANN_norm)
      predictions[i] = list(raw = prediction_ANN, woe = prediction_ANN_woe, normalized = prediction_ANN_norm)
      results[i] = list(raw = results_ANN, woe = results_ANN_woe, normalized = results_ANN_norm)
      object[i] = list(models = models[i], predictions = predictions[i], results = results[i])
      
    } 
  )[3]   # End timing
  print(paste("Ended cross validation after", timing))
  
  
  # Stop the parallel computing cluster
  stopCluster(cl)
  
  return(list(models = models, predictions = predictions, results = results, all = object, timing = timing))
}













# Initialise model control
model_control = trainControl(
  method = "adaptive_cv", # 'cv' for cross validation, 'adaptive_cv' drops unpromising models
  number = 5, # number of folds in cross validation (or number of resampling iterations)
  repeats = 5, # number of repeats for repeated cross validation
  search = "random", # or grid for a grid search
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  #timingSamps = length(fold), # number of samples to predict the time taken
  sampling = "smote", # This resolves class imbalances. 
  # Possible values are "none", "down", "up", "smote", or "rose". The latter two values require the DMwR and ROSE packages, respectively.
  allowParallel = TRUE, # Enable parallelization if available
  savePredictions = TRUE, # Save the hold-out predictions
  verboseIter = TRUE, # Print training log
  returnData = FALSE) # The training data will not be included in the output training object


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























############################


timing_parallel = system.time( 
  results <- foreach(i = 1:k, .combine = c, .packages = c("caret","nnet", "pROC"), .verbose = TRUE) %dopar%
  {
    # Split data into training and validation folds
    idx_test = which(fold_membership == i)
    idx_validation = which(fold_membership == ifelse(i == k, 1, i+1))
    
    test_fold = train_data[idx_test,]
    validation_fold = train_data[idx_validation,]
    train_fold = train_data[-c(idx_test,idx_validation)]
    
    ANN = train(return_customer~., data = train_fold,  
                method = "avNNet", maxit = 100, trace = FALSE, # options for nnet function
                #tuneGrid = ANN_parms, # parameters to be tested
                tuneLength = 10,
                metric = "ROC", trControl = model_control)
    
    prediction_ANN = predict(ANN, newdata = validation_fold, type = "raw")
    
    metaANN = train(return_customer~., data = cbind.data.frame(return_customer = validation_fold$return_customer, prediction_ANN),  
                     method = "avNNet", maxit = 100, trace = FALSE, # options for nnet function
                     #tuneGrid = ANN_parms, # parameters to be tested
                     tuneLength = 10,
                     metric = "ROC", trControl = model_control)
    
    prediction_metaANN = predict(metaANN, newdata = test_fold, type = "raw")
    
    list(ANN = ANN, prediction_ANN = prediction_ANN, 
         metaANN = metaANN, prediction_metaANN = prediction_metaANN, 
         auc = auc(test_fold$return_customer, prediction_metaANN))
  } 
)[3]   # End timing




# Option to run_meta

if(run_meta)
{
  metaANN = train(return_customer~., data = cbind.data.frame(return_customer = validation_fold$return_customer, prediction_ANN),  
                  method = "avNNet", maxit = 100, trace = FALSE, # options for nnet function
                  #tuneGrid = ANN_parms, # parameters to be tested
                  tuneLength = 10,
                  metric = "ROC", trControl = model_control)
  
  print("Training of meta model completed.")
  
  prediction_metaANN = predict(metaANN, newdata = test_fold, type = "raw")
  
  print("Prediction by meta model completed.")
}


























results_sequential = vector()
# Start timing
timing_sequential = system.time( 
  for(fold in training_folds)
  {
    # Split data into training and validation folds
    train_fold = train_data[fold,]
    test_fold  = train_data[-fold,]
    
    decision_tree = train(return_customer~., data = train_fold,  
                          method = "rpart", 
                          #maxit = 1000, trace = FALSE, # options for nnet function
                          #tuneGrid = dt_parms, # parameters to be tested
                          tuneLength = 15,
                          metric = "ROC", trControl = model_control)
    
    prediction_dt = predict(decision_tree, newdata = test_fold, type = "prob")[,2]
    results_sequential = append(results_sequential, auc(test_fold$return_customer, prediction_dt))
  }
)[3] # End timing



timing_parallel = system.time( 
  results <- foreach(fold = training_folds, .combine = c, .packages = c("caret","rpart", "pROC")) %dopar%
  {
    # Split data into training and validation folds
    train_fold = train_data[fold,]
    test_fold  = train_data[-fold,]
    
    
    decision_tree = train(return_customer~., data = train_fold,  
                          method = "rpart", 
                          #maxit = 1000, trace = FALSE, # options for nnet function
                          #tuneGrid = dt_parms, # parameters to be tested
                          tuneLength = 15,
                          metric = "ROC", trControl = model_control)
    
    prediction_dt = predict(decision_tree, newdata = test_fold, type = "prob")[,2]
    auc(test_fold$return_customer, prediction_dt)
  } 
)[3]   # End timing

