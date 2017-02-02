###### Nested Cross Validation ######

# Setup of parallel backend
# Detect number of available clusters, which gives you the maximum number of "workers" your computer has
no_of_cores = detectCores()
cl = makeCluster(max(1,no_of_cores))
registerDoParallel(cl)
message(paste("\n Registered number of cores:\n",getDoParWorkers(),"\n"))


# Set number of folds
k = 4
# Set seed for reproducability
set.seed(123)
# Create folds for cross validation
training_folds = createFolds(train_data$return_customer, k = k, list = TRUE, returnTrain = TRUE)
# Set seed for reproducability
set.seed(123)
# Define fold membership for cross validation
fold_membership = createFolds(train_data$return_customer, list = FALSE, k = k)


# Define a search grid of tuning parameters to test
dt_parms = expand.grid(cp = c(0, 10^seq(-5, 0, 1)), minbucket = seq(5,20,1))
#ANN_parms = expand.grid(decay = c(0, 10^seq(-5, 0, 1)), size = seq(3,30,3))
ANN_parms = expand.grid(decay = c(0, 10^seq(-5, 0, 1)), size = seq(3,30,3))

# Initialise model control
model_control = trainControl(
  method = "adaptive_cv", # 'cv' for cross validation, 'adaptive_cv' drops unpromising models
  number = 5, # number of folds in cross validation (or number of resampling iterations)
  repeats = 5, # number of repeats for repeated cross validation
  search = "random", # or grid for a grid search
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  #timingSamps = length(fold), # number of samples to predict the time taken
  #sampling = "smote", # This resolves class imbalances. 
  # Possible values are "none", "down", "up", "smote", or "rose". The latter two values require the DMwR and ROSE packages, respectively.
  allowParallel = TRUE, # Enable parallelization if available
  savePredictions = TRUE, # Save the hold-out predictions
  verboseIter = TRUE, # Print training log
  returnData = FALSE) # The training data will not be included in the output training object



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
                method = "avNNet", maxit = 1000, trace = FALSE, # options for nnet function
                bag = TRUE,
                #tuneGrid = ANN_parms, # parameters to be tested
                tuneLength = 15,
                metric = "Kappa", trControl = model_control)
    
    prediction_ANN = predict(ANN, newdata = validation_fold, type = "raw")
    
    metaANN = train(return_customer~., data = cbind.data.frame(return_customer = validation_fold$return_customer, prediction_ANN),  
                     method = "avNNet", maxit = 100, trace = FALSE, # options for nnet function
                     bag = TRUE,
                     #tuneGrid = ANN_parms, # parameters to be tested
                     tuneLength = 10,
                     metric = "Kappa", trControl = model_control)
    
    prediction_metaANN = predict(metaANN, newdata = test_fold, type = "raw")
    
    list(ANN = ANN, prediction_ANN = prediction_ANN, 
         metaANN = metaANN, prediction_metaANN = prediction_metaANN, 
         auc = auc(test_fold$return_customer, prediction_metaANN))
  } 
)[3]   # End timing

































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
                          metric = "Kappa", trControl = model_control)
    
    prediction_dt = predict(decision_tree, newdata = test_fold, type = "prob")[,2]
    auc(test_fold$return_customer, prediction_dt)
  } 
)[3]   # End timing

