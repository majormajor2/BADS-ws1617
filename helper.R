# This is the script that 
# 1) loads all necessary packages, 
# 2) defines helper functions and 
# 3) loads functions from other scripts.

######## Load packages ###############
# Try to load the package, if it doesn't exist, then install and load it

# Set up parallel computing - look at Exercise 7 for more details
if(!require("doParallel")) install.packages("doParallel"); library("doParallel")
if(!require("microbenchmark")) install.packages("microbenchmark"); library("microbenchmark")

# forcats to handle NAs as factor levels
if(!require("forcats")) install.packages("forcats"); library("forcats")
# lubridate for dates
if(!require("lubridate")) install.packages("lubridate"); library("lubridate") 
# matrixStats for Oren's file
if(!require("matrixStats")) install.packages("matrixStats"); library("matrixStats")
# corrplot to make correlation plots
if(!require("corrplot")) install.packages("corrplot"); library("corrplot")
# klaR to find weights of evidence (WoE)
if(!require("klaR")) install.packages("klaR"); library("klaR")
# OptimalCutpoints to calculate optimal cutpoints
if(!require("OptimalCutpoints")) install.packages("OptimalCutpoints"); library("OptimalCutpoints")

# tree for classification and regression trees
if(!require("tree")) install.packages("tree"); library("tree") 
# rpart for recursive partitioning and regression trees
if(!require("rpart")) install.packages("rpart"); library("rpart")
# rpart.plot to visualize your decision trees
if(!require("rpart.plot")) install.packages("rpart.plot"); library("rpart.plot")
# e1071 for Naive Bayes, etc.
if(!require("e1071")) install.packages("e1071"); library("e1071")
# randomForest for Random Forests
if(!require("randomForest")) install.packages("randomForest"); library("randomForest")

# caret for classification and regression training
if(!require("caret")) install.packages("caret"); library("caret") 
# adabag for adaptive boosting and bagging
if(!require("adabag")) install.packages("adabag"); library("adabag") 
# xgboost for eXtreme Gradient Boosting
if(!require("xgboost")) install.packages("xgboost"); library("xgboost") 

# nnet or neuralnet for Artificial Neural Networks
if(!require("nnet")) install.packages("nnet"); library("nnet")
if(!require("neuralnet")) install.packages("neuralnet"); library("neuralnet")
# deepnet for Deep Artificial Neural Networks
if(!require("deepnet")) install.packages("deepnet"); library("deepnet")
# darch for Deep Architectures and Restricted Boltzmann Machines
if(!require("darch")) install.packages("darch"); library("darch")
# FCNN4R for deep neural nets
if(!require("FCNN4R")) install.packages("FCNN4R"); library("FCNN4R")
# DMwR for additional sampling conducted after resampling to resolve class imbalances
if(!require("DMwR")) install.packages("DMwR"); library("DMwR")

# hmeasure for Area Under the Curve (alternatives are pROC and ROCR)
# hmeasure package requires all predictions to be available in one data frame
if(!require("hmeasure")) install.packages("hmeasure"); library("hmeasure")
if(!require("pROC")) install.packages("pROC"); library("pROC")

###### Define functions #########

# getting the dataset, creating a data.frame and 
# converting variables to correct data types
# input: filename of csv-file
# output: data frame

get_dataset = function(name) {
  
  # read csv-file, with headers, separated by ",", set first col ("ID") as row names
  data = read.csv(name, header=T, sep=",", row.names = 1)
  
  # drop data$points_redeemed because it has all zeros -> no informational value
  data$points_redeemed = NULL
  
  # factorise website model
  data$model = factor(data$model,labels=c("Design 1","Design 2", "Design 3"))
  
  # factorise all binary variables with labels "no" and "yes" where appropriate
  for(header in c("title","newsletter","coupon","giftwrapping","referrer","cost_shipping"))
  {
    data[,header] = factor(data[,header],labels=c("no","yes"))
  }
  
  # factorise binary variable data$delivery with appropriate labels
  data$delivery = factor(data$delivery, labels=c("Door delivery","Collection at post office"))

  # convert date variables to data type Date
  for(header in c("order_date","account_creation_date","deliverydate_estimated","deliverydate_actual"))
  {
    data[,header] = as.Date(data[,header])
  }

  # check if return_customer exists in data frame, if yes make it a factor
  if("return_customer" %in% colnames(data)) 
  {
    data$return_customer = factor(data$return_customer,labels=c("no","yes"))
  }

  # Return the data.frame 
  return(data)
}




# Scaling of variable to range [new_min;new_max]
# Defaults to min-max-scaling to range [0;1]
# input: column
# output: normalized column
normalize = function(x, mode = "min-max", new_min=0, new_max=1)
{
  if(!is.numeric(x)){x = as.numeric(x)} # convert to numeric
  
  # Square-root-normalization
  if(mode == "sqrt")
  {
    # Test for negative values
    if(TRUE %in% (x<0)){stop("Vector has negative entries. Cannot use sqrt-normalization.")}
    else{x = sqrt(x)}
  }
  # 
  else if(mode == "log")
  {
    # Test for zeroes - maybe we should add 1 if there are only positives?
    if(TRUE %in% (x==0)){stop("Vector has zeroes. Cannot use log-normalization.")}
    else{x = log(x)}
  }
  # Perform a min-max normalization 
  # (if a mode other than min-max has been specified this acts on the already normalized variable)
  normalized = (new_max-new_min) / (max(x)-min(x)) * (x - min(x)) + new_min
  
  return(normalized)
}

# min-max-scaling of cardinal variables to range from 0 to 1 (e.g. item count)
# input: data frame
# output: data frame
normalize_cardinal_variables = function(x) {
  min = min(x)
  max = max(x)
  normalized = (x - min)/(max - min)
  
  return(normalized)
}

# general standardization function
# input: numerical column
# output: standardized numerical column
standardize = function(x){
  mu = mean(x)
  std = sd(x)
  result = (x - mu)/std
  return(result)
}

## Converts weekdays from linear to sinusoidal
make_weekdays_cyclic = function(column)
{
  column = sin(normalize(as.numeric(column), new_min = 0, new_max = 6*pi/7))
  return(column)
}


## Truncates outliers in numerical variables
truncate_outliers = function(column, multiple = 1.5, only_positive = FALSE)
{
  # Convert to numeric if it is not yet
  if(!is.numeric(column)){column = as.numeric(column)}
  # Find values at 1st and 3rd quartile
  lower_quartile = as.numeric(summary(column))[2]
  upper_quartile = as.numeric(summary(column))[5]
  # Calculate the inter-quantile-range IQR
  IQR = upper_quartile - lower_quartile
  # Calculate lower and upper bound
  lower_bound = lower_quartile - multiple*IQR
  upper_bound = upper_quartile + multiple*IQR
  # Identify ouliers and replace by bounds
  if(!only_positive){column[column < lower_bound ] = lower_bound} # only if not onesided
  column[column > upper_bound ] = upper_bound
  
  return(column)
}


# function to check for new levels in factor variables in dataset we want to apply woe to
# input: train (or any dataset woe_object was trained on) and class dataset (or any dateset woe shall be applied to)
check_new_levels = function(known_data, class_data, target = "return_customer")
{
  new_levels = list() # create an empty list that will hold the new levels
  
  for(column in colnames(known_data)) # loops over all columns
  {
    if(is.factor(known_data[,column]) && !column == target) # checks if the column is a factor and if it is not the target variable
    {
      if(length(setdiff(levels(class_data[,column]),levels(known_data[,column]))) != 0) # checks if there are new factor levels
      {
        # before the 2nd loop: create a temporary vector to hold new levels in the column
        temp = vector()
        for(level in setdiff(levels(class_data[,column]),levels(known_data[,column]))) # loops through new factor levels
        {
          if(level %in% levels(class_data[,column]))
          {
            #print(column)
            #print(level) 
            temp = append(temp, level) # append the level to the temporary vector
          }
        }
        # after the 2nd loop: add the filled temporary vector to the list of new levels
        new_levels[column] = list(temp) 
      }
    }
  }
  return(new_levels)
}

## Function to identify highly correlated variables
## Chooses the variable that has higher mean correlation
## Input: dataset
## Output: names of variables to drop
strongly_correlated = function(dataset, threshold = 0.7)
{
  correlation_matrix = cor(dataset[, sapply(dataset, is.numeric)]) # calculate matrix only for numeric columns
  listed_variables = vector() # vector of listed variables to prevent duplication
  dropped_variables = vector()# vector of variables that will be dropped
  
  for(column in colnames(correlation_matrix))
  {
    listed_variables = append(listed_variables, column) # add column to listed variables
    # loop only over variables for which we have not calculated the correlation yet
    for(row in setdiff(row.names(correlation_matrix),listed_variables)) 
    {
      if(abs(correlation_matrix[row,column]) > threshold)
      {
        if(mean(abs(correlation_matrix[row,])) > mean(abs(correlation_matrix[,column])))
        {
          dropped_variables = append(dropped_variables, row)
        }
        else{dropped_variables = append(dropped_variables, column)}
      }
    }
  }
  # return unique variable names
  return(unique(dropped_variables))
}


# function: get the list of included columns that match one pattern but not another
# input: patterns to include and exlude
# output: list of included columns
get_matching_columns = function(dataset, exclude_pattern, include_pattern)
{
  list_exclude = colnames(dataset[,grep(exclude_pattern, invert = TRUE, colnames(dataset))])
  list_include = list_exclude[grep(include_pattern, x = list_exclude)]
  return(list_include)
}














#### MASTER DATASET 
# save predictions to one data frame & export it as csv file

# call the master file
call_master = function(filename.csv = "predictions_test.csv"){
  # download current version of masterfile
  master = read.csv(filename.csv, header=T, sep=",", row.names = 1)
  return(master)
}

# input: predictions_all data frame, vector of probability prediction + informative name
# output: predictions_all dataframe with added predictions
save_prediction = function(predictions_all, newprediction, name)
{
  predictions_all[,name] = newprediction
  return(predictions_all)
}

### MASTER FILE for PREDICTIONS ###

# input: vector of probability prediction + informative name of 
# attention: put predname and filename in " "
# attention: include suffix .csv for filename
# outputs 
# - a data frame including the new predictions (if saved to object)
# - csv file 

save_prediction_to_master = function(newprediction, predname, filename.csv)
{
  # load current version of masterfile
  master = read.csv(filename.csv, header=T, sep=",", row.names = 1)
  # check for consistency
  if(nrow(master) != length(newprediction)){
    print("Numbers of rows in master-file and length of prediction vectors do not match.")
    break
  }
  # add new column
  master[,predname] = newprediction
  # save as csv
  write.csv(x = master, file = filename.csv)
  return(master)
}


# save prediction to master
save_prediction_to_master = function(filename.csv = "predictions_test.csv", master = df_predictions_test){
  # save as csv
  write.csv(x = master, file = filename.csv)
  return(master)
}





####### Load scripts #######
source("data_cleaning.R")
source("correlation_matrix.R")
source("preprocessing.R")
source("performance_measures.R")
source("nested_cross_validation.R")
source("predictclass.R")
