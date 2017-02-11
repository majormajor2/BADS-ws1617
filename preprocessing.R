# Weight of Evidence functions to turn factors with more than 2 levels into numerical variables according to their WoE

# Calculate WoE on train_dataset & return WoE object
calculate_woe = function(train_dataset, target = "return_customer", columns_to_replace = c("form_of_address", "email_domain", "model", "payment", "postcode_invoice", "postcode_delivery", "advertising_code"))
{
  woe_object = woe(as.formula(paste(target, paste(columns_to_replace, collapse="+"), sep = "~")), data = train_dataset, zeroadj = 0.5)
  return(woe_object)
}

# Apply WoE & return data set with columns replaced by woe
apply_woe = function(dataset, woe_object, doReplace = TRUE)
{
  # Columns to replace are the columns that are in WoE-object
  columns_to_replace = names(woe_object$woe)
  # Predict WoE in the new dataset with the WoE-object and replace factor levels with their WoE
  dataset_woe = predict(woe_object, newdata = dataset, replace = doReplace)
  # Change names (get rid of the woe.-prefix)
  colnames(dataset_woe) = gsub(pattern = "woe.", replacement = "", x = colnames(dataset_woe))
  # Return the dataset with replaced columns
  return(dataset_woe)
}

## Function to truncate outliers in the entire dataset
treat_outliers = function(dataset, mode = "sqrt")
{
  data = dataset
  
  # get the columns that include time differences
  time_diff_columns = c("deliverydate_estimated","deliverydate_actual")
  data[,time_diff_columns] = sapply(data[,time_diff_columns], truncate_outliers)
  data$account_creation_date = normalize(abs(data$account_creation_date), mode = "sqrt")
  data$account_creation_date[data$account_creation_date > 0] = truncate_outliers(data$account_creation_date[data$account_creation_date > 0])
  
  # get the columns that have only positive values (all item counts + weight)
  include_pattern = c("_count|_items|weight")
  exclude_pattern = c("weight_missing")
  columns_with_only_positive = setdiff(grep(include_pattern, colnames(data)), grep(exclude_pattern, colnames(data)))
  
  # replace with normalized values
  if(mode == "log"){data[,columns_with_only_positive] = data[,columns_with_only_positive]+1}
  data[,columns_with_only_positive] = sapply(data[,columns_with_only_positive], normalize, mode = mode)
  
  return(data)
}

## Normalization function (minmax) for the entire dataset
## cannot normalize multilevel factors, therefore those remain untouched
## input: dataset, colnames of multilevel factors
## output: dataset
normalize_dataset = function(data, multilevel_factors = c("form_of_address", "email_domain", "model", "payment", "postcode_invoice", "postcode_delivery", "advertising_code"), target = "return_customer")
{
  print("Performing normalization on all parameters that are not multilevel factors:")
  
  for(column in colnames(data)) # loop over all columns
  {
    if(!column %in% multilevel_factors)
    {
      print(column)
      data[,column] = sapply(data[,column],as.numeric) # convert to numeric
      data[,column] = standardize(data[,column]) # standardise
      data[,column] = normalize(data[,column], new_min = -1, new_max = 1) # run minmax-normalization
    }
  }
  return(data)
}


# A normalization function wrapper
# Input should be a WoE-dataset and a vector of column names that should be dropped
# Output is a normalized dataset
prepare = function(dataset, dropped_correlated_variables, target = "return_customer")
{
  for(column in colnames(dataset)){ # loop over all columns
    if(column != target){
      dataset[,column] = sapply(dataset[,column],as.numeric)}} # convert to numeric
  
  dataset = treat_outliers(dataset)
  dataset = normalize_dataset(dataset, c(target))
  dataset[dropped_correlated_variables] = NULL
  
  return(dataset)
}




### --- UNSUPERVISED BINNING --- ###
# this functions cuts a column into a pre-specified no of bins, either with equal width or equal frequency
# input: datset, columns (default is set)
# output: new dataset with columns replaced by factor levels = number of bins

create_bins  = function(dataset, woe_object = woe_object, NO_BINS = 5, DO_EQUAL_WIDTH = TRUE, run_woe = TRUE){
  
  ## --- PRELIMINARY --- ##
  
  # define columns to be binned
  # columns form_of_address, model, payment only 3-4 levels, so they are not binned
  columns <- c("email_domain", "postcode_invoice", "postcode_delivery", "advertising_code")
  
  # check if columns are numeric
  for(column in columns){
    if(!is.numeric(dataset[,column])){
      stop("Please use the woe-version of your dataset. At least one of the columns you want to bin is not numeric.")
    }
  }
  
  ## --- BINNING CODE STARTS HERE --- ##
  
  
  
  # BIN WITH EQUAL WIDTH
  
  if(DO_EQUAL_WIDTH){
    
    # loop over all columns
    for(column in columns){
      
      # cut column-values into equal-width-bins and replace values with factor-levels
      dataset[,column] <- cut(dataset[,column], NO_BINS, include.lowest = TRUE, labels = paste0("level",1:NO_BINS))
    }   
    
    # BIN WITH EQUAL FREQUENCY  
  } else {
    
    # loop over all columns
    for(column in columns){
      
      # define quantile breakpoints 
      breaks <- as.numeric(quantile(dataset[,column], 0:NO_BINS/NO_BINS))
      
      # check if breaks are unique
      while(any(duplicated(breaks))){
        # change duplicate breaks by small value and repeat until no more duplicates are present in "breaks"
        breaks[duplicated(breaks)] <- breaks[duplicated(breaks)]+0.0000001
      } 
      
      # cut column-values into bins with approx. equal number of observations and replace values with factor-levels    
      dataset[,column] <- cut(dataset[,column], breaks, include.lowest = TRUE, right = FALSE, labels = paste0("level",1:NO_BINS)) 
    } 
  } # END: BIN WITH EQUAL FREQUENCY or WITH EQUAL FREQUENCY 
  
  
  # 2. APPLY WOE  
  
  if(run_woe){ # apply woe_object to current dataset
    dataset <- apply_woe(dataset = dataset, woe_object = woe_object, doReplace = TRUE)
  }
  
  # RETURN BINNED COLUMNS AS WOE-TRANSFORMED NUMERICS if run_woe is TRUE     
  # otherwise RETURNS COLUMNS AS BINNED FACTORS 
  return(dataset) 
} # END OF FUNCTION


## Check for significance of variables and drop insignificant ones
drop_insignificant_variables = function(data, proportion_to_drop = 1/3)
{
  ### Adaptive Boosting ###
  adaboost = boosting(return_customer~., data=data, boos=TRUE, mfinal=20, coeflearn='Breiman')
  
  adaboost_importance = sort(adaboost$importance, decreasing = TRUE)
  adaboost_importance[adaboost_importance > 0]
  
  decision_tree = rpart(return_customer ~ ., data = data, method = "class", cp = 0.001, minbucket = 8) # minsplit also exists
  
  data[names(sort(decision_tree$variable.importance))[1:(length(decision_tree$variable.importance)*proportion_to_drop)]] = NULL
  data[names(sort(decision_tree$variable.importance))[1:(length(decision_tree$variable.importance)*proportion_to_drop)]] = NULL

  return(data)
}
