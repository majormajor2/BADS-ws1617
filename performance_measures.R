#### Model Performance Measures ####


# Helper function to compute measures of predictive accuracy
predictive_performance = function(y=NULL, prediction=NULL, cutoff=.5, returnH = TRUE, shallPlot = TRUE) 
{
  # Assumptions:
  # y is a vector of factors
  # prediction is a vector of probability predictions
  
  if (length(y) != length(prediction))
  {
    stop("Data vector and prediction vector must have same length!")
  }
  
  
  
  # Calculate Brier Score
  # y - 1 because levels of factors start at 1 not 0
  brier_score = sum(((as.numeric(y) - 1) - prediction)^2) / length(y)
  
  # Calculate Classification error
  classification = factor(as.numeric(prediction >= cutoff), levels = c(0,1), labels=levels(y)) 
  #classification = factor(as.numeric(prediction >= cutoff), labels=c("negative", "positive")) 
  classification_error = 1 - sum(y==classification) / length(y)
  
  
  # Calculate Area Under the Curve with pROC
  auc = as.numeric(roc(response = y, predictor = prediction)$auc)
  
  # print confusion matrix
  # class_counts = misclassCounts(classification, y); class_counts$conf.matrix
  # print misclassification-based statistic - e.g. error rate
  # print(class_counts$metrics, digits = 3)
  
  # Compute the H-measure and other scalar classification performance metrics
  H = HMeasure(y, prediction, threshold = cutoff, severity.ratio = 3/10)
  h_measure = H$metrics$H
  gini = H$metrics$Gini
  precision = H$metrics$Precision
  TP = H$metrics$TP
  FP = H$metrics$FP
  TN = H$metrics$TN
  FN = H$metrics$FN
  
  # Compute Average Return per Customer
  score = (3*TN - 10*FN)/(TP+FP+TN+FN)
  
  # Compute average expected costs
  exp_cost = mean(ifelse(y == "yes", 10*(1-prediction), 3*(prediction)))
  
  # Calculate ROC
  if(shallPlot){plotROC(results = H)}
  
  # create a list of the performance measures
  output = list(brier_score = brier_score, 
                classification_error = classification_error, 
                h_measure = h_measure,
                area_under_curve = auc,
                gini = gini,
                precision = precision,
                true_positives = TP,
                false_positives = FP,
                true_negatives = TN,
                false_negatives = FN,
                avg_return = score,
                exp_cost = exp_cost,
                H = H)
  
  # if returnH is FALSE, drop H object from output
  if(!returnH){output$H = NULL}
  
  return(output)
}


## Function to construct a cost matrix

build_cost_matrix = function(CBTN = +3, CBFN = -10, CBFP = 0, CBTP = 0)
{
  # calculate costs with 0 on diagonals
  CFN = CBFN - CBTP
  CFP = CBFP - CBTN
  
  # build cost-matrix
  cost.matrix = matrix(c(
    0, CFN,
    CFP, 0),
    2, 2, byrow=TRUE)
  
  # name rows and columns
  colnames(cost.matrix) = list("noreturn", "return")
  rownames(cost.matrix) = list("noreturn", "return")
  
  return(cost.matrix)
}


# Function to return the optimal cutoff 
# given a target vector of factors and a vector of predictions as probabilities.
# Returns a number.
optimal_cutoff = function(target, prediction, cost_matrix = build_cost_matrix(), tag_false = "no")
{
  # create dataframe for later use
  df = data.frame(target = target, prediction = prediction)
  # MODEL CONTROL
  # method: maxKappa
  model_control_cutpoints = control.cutpoints(CFP = -cost_matrix[2,1], CFN = -cost_matrix[1,2], costs.ratio = -cost_matrix[2,1]/-cost_matrix[1,2], weighted.Kappa = TRUE)
  
  # get the OC object
  oc = optimal.cutpoints(X = "prediction", 
                         status = "target",
                         tag.healthy = tag_false,
                         methods = "MCT", 
                         data = df, 
                         control = model_control_cutpoints)
  
  # extract optimal cutoff
  optimal_cutoff = oc$MCT$Global$optimal.cutoff$cutoff
  return(optimal_cutoff)
}



### Custom function for train.control
# use avg retrun as metric to choose the model
# input: dataframe with predictions
# output: average return

revenue_maximization = function(data, lev = NULL, model = NULL)
{
  # load inside function to ensure that with parallel computing workers have it in their environment
  if(!require("OptimalCutpoints")) install.packages("OptimalCutpoints"); library("OptimalCutpoints")
  source("helper.R")
  
  # GET COST MATRIX
  cost.matrix <- build_cost_matrix()
  
  # MODEL.CONTROL
  # method: maxKappa
  model.control.optc = control.cutpoints(CFP = -cost.matrix[2,1], CFN = -cost.matrix[1,2], costs.ratio = -cost.matrix[2,1]/-cost.matrix[1,2], weighted.Kappa = TRUE)
  
  # RUN OPTIMAL CUTPOINTS
  oc = optimal.cutpoints(
    X = "yes", 
    status = "obs", 
    tag.healthy = "no", 
    methods = "MCT", 
    data = data, 
    control = model.control.optc)
  
  # SELECT OPTIMAL CUTPOINT
  # define temporary dataframes to store cutoffs 
  df <- data.frame(cutoff = oc$MCT$Global$optimal.cutoff$cutoff)
  # check if cutpoint unique                  
  for(index in 1:length(oc$MCT$Global$optimal.cutoff$cutoff)){
    # optimal cutpoint
    df[index,"avg_return"] <- predictive_performance(data[,"obs"], prediction = data[,"yes"], cutoff = df[index,"cutoff"], returnH = FALSE)$avg_return
  }
  # Choose cutoff that maximises avg return
  opt.cutoff <- df[df$avg_return == max(df$avg_return), "cutoff"]
  # Calculate average return
  avg_return <- predictive_performance(y = data$obs, prediction = data$yes, cutoff = opt.cutoff, returnH = FALSE)$avg_return
  
  
  # name metrics
  names(avg_return) <- "avg_return"
  names(opt.cutoff) <- "optimal.cutoff"
  
  
  # OUTPUT
  return(c(avg_return, opt.cutoff))
}



### Custom function for trainControl
# use expected cost as metric to choose the tune hyperparameters of the model
# input: dataframe with true values and predictions
# output: expected cost per customer

cost_minimization = function(data, lev = c("no","yes"), model = NULL)
{
  # Define costs of False Positives (CFP) and False Negatives (CFN)
  CFP = -3; CFN = -10
  # Generate cost matrix
  #cost_matrix = matrix(c(0, CFN,
  #                       CFP, 0), nrow = 2, ncol = 2, byrow=TRUE, 
  #                              dimnames = list(c("non-return", "return"), c("non-return", "return")))
  
  # Check if observations are encoded as expected
  #print(summary(as.numeric(data$obs)))
  
  # Calculate expected costs (true values are in data$obs, probability predictions for a return in data$yes)
  expected_cost = ifelse(data$obs == "yes", CFN*(1-data$yes), CFP*(data$yes))
  
  # Calculate mean of expected costs
  expected_cost = mean(expected_cost)
  
  # Name metrics
  names(expected_cost) = "exp_cost"
  
  # OUTPUT
  return(expected_cost)
}


# This function chooses the best tune that generalizes best to other folds
# out of a selection of models from nested cross-validation 
# Input: a list of model objects from nested x-val functions
# Output: data.frame with hyperparameters for final fitting
choose_best_tune = function(models)
{
  highest_return = 0
  for(i in 1:length(models))
  {
    model = models[[i]]$model
    prediction = models[[i]]$prediction
    best_cutoff = optimal_cutoff(known[which(fold_membership == i),]$return_customer, prediction)
    avg_return = as.numeric(predictive_performance(known[which(fold_membership == i),]$return_customer, prediction, cutoff = best_cutoff, returnH = FALSE)$avg_return)
    # Check if the highest return is better than that of the other sets of hyperparameters
    # i.e. if it generalizes better than the others
    if(avg_return > highest_return)
    {
      # Pick best set of hyperparameters
      hyperparameters = data.frame(model$bestTune)
      highest_return = avg_return
    }
  }
  return(hyperparameters)
}

# This function lists the predictive performance of each fold
# from nested cross-validation 
# Input: a list of model objects from nested x-val functions, dataframe to store the result
# Output: dataframe
list_fold_performance = function(models, name, store = NULL)
{
  # Initialise
  if(is.null(store)){store = data.frame(row.names = c("avg_return (mean)","avg_return (SD)","exp_cost (mean)", "exp_cost (SD)", "AUC (mean)","AUC (SD)"))}
  avg_return = vector()
  exp_cost = vector()
  auc = vector()
  
  for(i in 1:length(models))
  {
    model = models[[i]]$model
    prediction = models[[i]]$prediction
    best_cutoff = optimal_cutoff(known[which(fold_membership == i),]$return_customer, prediction)
    
    avg_return = append(avg_return, predictive_performance(known[which(fold_membership == i),]$return_customer, prediction, cutoff = best_cutoff, returnH = FALSE)$avg_return)
    exp_cost = append(exp_cost, predictive_performance(known[which(fold_membership == i),]$return_customer, prediction, shallPlot = FALSE, returnH = FALSE)$exp_cost)
    auc = append(auc, predictive_performance(known[which(fold_membership == i),]$return_customer, prediction, shallPlot = FALSE, returnH = FALSE)$area_under_curve)
  }
  store["avg_return (mean)",name] = mean(avg_return)
  store["avg_return (SD)",name] = sd(avg_return)
  store["exp_cost (mean)",name] = mean(exp_cost)
  store["exp_cost (SD)",name] = sd(exp_cost)
  store["AUC (mean)",name] = mean(auc)
  store["AUC (SD)",name] = sd(auc)
    
  return(store)
}