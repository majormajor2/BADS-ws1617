known_predictions = call_master("known_predictions.csv")
View(known_predictions)
# Set number of folds
k = 5
# Set seed for reproducability
set.seed(123)
# Define fold membership for cross validation
fold_membership = createFolds(known$return_customer, list = FALSE, k = k)




# Train the meta model on the predictions dataframe
meta_model = foreach(i = 1:k, .verbose = TRUE) %dopar% # fold = training_folds, .packages = required_packages, .export = required_functions, .combine = rbind.data.frame, 
{
  # Sourcing function files - potentially required for foreach loop
  source("helper.R")
  source("woe.R")
  source("performance_measures.R")
  source("controlcutoffs_fortraincontrol.R")
  
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
    summaryFunction = stephanie.cutoff,
    #timingSamps = length(fold), # number of samples to predict the time taken
    sampling = "smote", # This resolves class imbalances. 
    # Possible values are "none", "down", "up", "smote", or "rose". The latter two values require the DMwR and ROSE packages, respectively.
    allowParallel = TRUE, # Enable parallelization if available
    #savePredictions = TRUE, # Save the hold-out predictions
    verboseIter = TRUE, # Print training log
    returnData = FALSE) # The training data will not be included in the output training object
  
  # Set hyperparameters (only a single set - so we can use the train function, others are defined in their function)
  parameters = expand.grid(nrounds = 200, max_depth = 4, eta = 0.05, gamma = 0, colsample_bytree = 1,min_child_weight = 1, subsample = 0.8)
  
  # Train models
  model = train(return_customer~., data = train_fold, method = "xgbTree", tuneGrid = parameters, metric = "avg_return", trControl = model_control)
  
  # Predict return_customer on remainder
  prediction = predict(model, newdata = test_fold, type = "prob")[,2]
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
# Load predictions
predictions_test = read.csv("predictions_test.csv", row.names = 1)

meta_predictions_test = data.frame(return_customer = predictions_test$return_customer)
meta_performance_test = data.frame(metrics = c("brier_score","classification_error","h_measure","area_under_curve","gini","precision","true_positives","false_positives","true_negatives","false_negatives","avg_return"))

for(i in 1:k)
{
  meta_predictions_test[,paste("Fold",i)] = predict(meta_model[[i]]$model, newdata = predictions_test, type = "prob")[,2]
  meta_performance_test[,paste("Fold",i)] = as.numeric(predictive_performance(predictions_test$return_customer, meta_predictions_test[,paste("Fold",i)], cutoff = meta_model[[i]]$cutoff, returnH = FALSE))
}

View(meta_performance_test)


############### WHICH CUT-OFF SHOULD WE PICK

# First pull the known_predictions file and save as known_predictions

known_predictions = call_master("known_predictions.csv")

set.seed(666)
idx_train_meta  = createDataPartition(y = known_predictions$return_customer, p = 0.8, list = FALSE) 
train_data_meta = known_predictions[idx_train_meta, ] # training set
test_data_meta  =  known_predictions[-idx_train_meta, ] # test set (drop all observations with train indices)



model.control<- trainControl(
  method = "cv", # 'cv' for cross validation
  number = 5, # number of folds in cross validation
  classProbs = TRUE,
  summaryFunction = stephanie.cutoff,
  allowParallel = TRUE, # Enable parallelization if available
  returnData = TRUE # We will use this to plot partial dependence
)

parameters = expand.grid(nrounds = 800, max_depth = 4, eta = 0.01, gamma = 0, colsample_bytree = 1, min_child_weight = 1, subsample = 0.8)

xgb.parms.default <- expand.grid(nrounds = c(20, 40, 60, 80), 
                                 max_depth = c(2, 4), 
                                 eta = c(0.01, 0.05, 0.1, 0.15), 
                                 gamma = 0,
                                 colsample_bytree = c(0.8, 1),
                                 min_child_weight = 1,
                                 subsample = 0.8)

xgb.def.train <- train(return_customer~., data = train_data_meta,  
                       method = "xgbTree",
                       tuneGrid = xgb.parms.default,
                       metric = "avg_return", 
                       trControl = model.control)

xgb.params2.meta4 <- predict(xgb.def.train, newdata = test_data_meta, type = "prob")[,2]
optimal_cutoff(test_data_meta$return_customer, xgb.params2.meta4)
predictive_performance(test_data_meta$return_customer, xgb.params2.meta4, cutoff =0.246046, returnH = FALSE)


#test1 - setseed 666
#cutoff: 0.2117774
#avg_return: 0.8393408
#auc: 0.6572061
# train.cutoff: 0.2236857
# train.avg_return: 0.8245952


# test2 - setseed  100
# optimal.cutoff: 0.2201024
# avg_return: 0.8537008
# auc: 0.6592809
# train.cutoff:0.2261933
# train.avg_return: 0.8361604


#test3 - setseed  200
#optimal.cutoff: 0.2349651
#avg_return: 0.8277756
#auc: 0.6552513
# train.cutoff: 0.2274853
# train.avg_return: 0.815054


#test4 - setseed  300
#optimal.cutoff:0.2425775
#avg_return:0.8443524 
#auc: 0.6610999
# train.cutoff:  0.2326611
# train.avg_return:0.8369314


#test5 - setseed  400
#optimal.cutoff: 0.2419465
#avg_return: 0.8529298
#auc: 0.6598767
# train.cutoff: 0.2303491
# train.avg_return: 0.8306669


optimized_cutoff = (0.21177744 + 0.2268287 + 0.2349651 + 0.2425775 + 0.2419465)/5
optimized_cutoff_final = (optimized_cutoff + xgb.def.train$results$optimal.cutoff)/2



# PREDICT CLASS
model.control<- trainControl(
  method = "cv", # 'cv' for cross validation
  number = 5, # number of folds in cross validation
  classProbs = TRUE,
  summaryFunction = stephanie.cutoff,
  allowParallel = TRUE, # Enable parallelization if available
  returnData = TRUE # We will use this to plot partial dependence
)

parameters = expand.grid(nrounds = 800, max_depth = 4, eta = 0.01, gamma = 0, colsample_bytree = 1, min_child_weight = 1, subsample = 0.8)

xgb.class <- train(return_customer~., data = known_predictions,  
                       method = "xgbTree",
                       tuneGrid = xgb.parms.default,
                       metric = "avg_return", 
                       trControl = model.control)

# load class dataset

class_predictions = call_master("class_predictions.csv")
meta4.class.pred <- predict(xgb.def.train, newdata = class_predictions, type = "prob")[,2]
class_predictions$return_customer = ifelse(meta4.class.pred > optimized_cutoff_final, "1", "0")
group27 = call_master("group27.csv")
group27$return_customer = ifelse(meta4.class.pred > optimized_cutoff_final, 1, 0)


summary(group27$return_customer)

group27 = save_prediction_to_master("group27.csv", group27)
