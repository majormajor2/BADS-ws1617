source("main.R")
source("optimal_cutpoints.R")

##### Convert all to numeric #####
train60_normalized = train60_data_woe # copy dataset
for(column in colnames(train60_normalized)){ # loop over all columns
  if(column != "return_customer"){
    train60_normalized[,column] = sapply(train60_normalized[,column],as.numeric)}} # convert to numeric

validation_normalized = validation_data_woe # copy dataset
for(column in colnames(validation_normalized)){ # loop over all columns
  if(column != "return_customer"){
    validation_normalized[,column] = sapply(validation_normalized[,column],as.numeric)}} # convert to numeric

##### Truncate outliers #####
train60_normalized = treat_outliers(train60_normalized)   
validation_normalized = treat_outliers(validation_normalized)  
##### Normalize to -1;1 #####
train60_normalized = normalize_dataset(train60_normalized, c("return_customer"))
validation_normalized = normalize_dataset(validation_normalized, c("return_customer"))
##### Drop Correlated Variables #####
dropped_correlated_variables = strongly_correlated(train60_normalized, threshold = 0.6)
train60_normalized[dropped_correlated_variables] = NULL
validation_normalized[dropped_correlated_variables] = NULL
##### Plot Correlation Matrix #####
correlation_matrix = cor(train60_normalized[, sapply(train60_normalized, is.numeric)])
corrplot(correlation_matrix, title = "Correlation Matrix", type = "full", order = "AOE", tl.cex = 0.5, tl.srt = 45, mar = c(1,0,1,1))



##### Neural Networks #####
# Set up parallel computing - look at Exercise 7 for more details
if(!require("doParallel")) install.packages("doParallel"); library("doParallel") # load the package
if(!require("microbenchmark")) install.packages("microbenchmark"); library("microbenchmark") # load the package

# Setup up parallel backend
# Detect number of available clusters, which gives you the maximum number of "workers" your computer has
no_of_cores = detectCores()
cl = makeCluster(max(1,no_of_cores))
registerDoParallel(cl)
message(paste("\n Registered number of cores:\n",getDoParWorkers(),"\n"))

# Initialise model control
model_control = trainControl(
  method = "adaptive_cv", # 'cv' for cross validation
  number = 4, # number of folds in cross validation
  repeats = 1, # number for repeated cross validation
  search = "random",
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  timingSamps = 10000, # number of samples to predict the time taken
  sampling = "smote", # This resolves class imbalances. 
  # Possible values are "none", "down", "up", "smote", or "rose". The latter two values require the DMwR and ROSE packages, respectively.
  allowParallel = TRUE, # Enable parallelization if available
  savePredictions = TRUE, # Save the hold-out predictions
  verboseIter = TRUE, # Print training log
  returnData = FALSE # The training data will not be included in the output training object
)

# Define a search grid of values to test
ANN_parms = expand.grid(decay = c(0, 10^seq(-5, 0, 1)), size = seq(3,30,3))


# Train neural network ANN with 5-fold cross validation
ANN = train(return_customer~., data = train60_normalized,  
              method = "nnet", maxit = 1000, trace = FALSE, # options for nnet function
              tuneGrid = ANN_parms, # parameters to be tested
              metric = "ROC", trControl = model_control)

# Don't forget to stop the parallel computing cluster when you don't need it anymore!
stopCluster(cl)

prediction_ANN = predict(ANN, newdata = validation_normalized, type = "prob")[,2]
prediction_ANN_norm = normalize(prediction_ANN)
predictions_validation = data.frame(return_customer = validation_normalized$return_customer, ANN = prediction_ANN, ANN_norm = prediction_ANN_norm)




predictive_performance(validation_normalized$return_customer, prediction_ANN_norm, optimal_cutoff(validation_normalized$return_customer, prediction_ANN_norm))
