

# getting the dataset, creating a data.frame and 
# converting variables to correct data types
# input: filename of csv-file
# output: data frame

get_dataset = function(name) {
  
  ##########################################################################
  # read csv-file, with headers, separated by ",", set first col ("ID") as row names
  data = read.csv(name, header=T, sep=",", row.names = 1)
  
  ##########################################################################
  # drop data$points_redeemed because it has all zeros -> no informational value
  data$points_redeemed = NULL
  
  ##########################################################################
  # factorise ID - not necessary anymore because we made the ID the row name
  #data$ID = factor(data$ID)
  
  
  # factorise website model
  data$model = factor(data$model,labels=c("Design 1","Design 2", "Design 3"))
  
  # factorise all binary variables with labels "no" and "yes" where appropriate
  for(header in c("title","newsletter","coupon","giftwrapping","referrer","cost_shipping"))
  {
    data[,header] = factor(data[,header],labels=c("no","yes"))
  }
  
  # factorise binary variable data$delivery with appropriate labels
  data$delivery = factor(data$delivery, labels=c("Door delivery","Collection at post office"))
  ##########################################################################
  # convert date variables to data type Date
  for(header in c("order_date","account_creation_date","deliverydate_estimated","deliverydate_actual"))
  {
    data[,header] = as.Date(data[,header])
  }
  ##########################################################################
  # check if return_customer exists in data frame, if yes make it a factor
  if("return_customer" %in% colnames(data)) 
  {
    data$return_customer = factor(data$return_customer,labels=c("no","yes"))
  }
  ###########################################################################
  
  return(data)
}


# treats the missing values in the data frame
# input: data frame with missing values
# output: data frame with treated missing values
treat_missing_values = function(dataset) {
  data = dataset
  
  ## 1.1: NAs for advertising_code
  # see Data_Cleaning_SF.R courtesy of Stephie
  data$advertising_code[data$advertising_code == ""] = NA
  # dummy variable is no longer needed as NAs are encoded as level "Missing"
  #data$advertising_code_missing = factor(ifelse(is.na(data$advertising_code), 1, 0), labels=c("no","yes"))
  # drop empty level from factor
  data$advertising_code = droplevels(data$advertising_code)
  data$advertising_code = fct_explicit_na(data$advertising_code, "Missing")
  
  ## 1.2: New level for advertising_code in class dataset (level in question: "AA", 1 observation)
  # replace AA in class by modal value of advertising_code in known dataset
  if("AA" %in% data$advertising_code){
    data[data$advertising_code == "AA", "advertising_code"] <- names((sort(-table(known$advertising))))[1]
  }
  
  
  ## 2: NAs for form_of_address
  # see Data_Cleaning_SF.R courtesy of Stephie
  # dummy variable is no longer needed as NAs are encoded as level "Missing"
  #data$form_of_address_missing = factor(ifelse(is.na(data$form_of_address), 1, 0), labels=c("no","yes"))
  data$form_of_address = fct_explicit_na(data$form_of_address, "Missing")
  
  
  return(data)
}


### Date cleaning function ###
# see DatacleaningDates.R - courtesy of Hamayun
# input: data frame
# output: data frame
treat_dates = function(dataset) {
  data = dataset
  
  ## Order date does not require cleaning, there are no missing values or outliers
  # i.e. all are within the correct interval of one year
  
  ## Account creation date: Create a dummy variable for NAs
  data$account_creation_date_missing = ifelse(is.na(data$account_creation_date), 1, 0)
  ## and replace NAs to match order date (equal in 89% of the cases)
  # (We notice a large clustering at the beginning of the period)
  
  # index the NAs for account creation date
  na_index = which(is.na(data$account_creation_date))
  # replace them with order date
  data$account_creation_date[na_index] = data$order_date[na_index]
  
  ## Delivery date estimated has outliers, from 2010 and 4746. Create a dummy to capture both
  data$deliverydate_estimated_outliers = ifelse(year(data$deliverydate_estimated) == 2010 | year(data$deliverydate_estimated) == 4746, 1, 0)
  
  ## Change 2010 to 2014 (all order dates are within that year)
  year(data$deliverydate_estimated[year(data$deliverydate_estimated) == 2010]) = 2014

  # replace deliverydate_estimated for year 4746 by order_date + median estimated delivery time
  deliverytime_estimated = data$deliverydate_estimated - data$order_date
  median_deliverytime_estimated = median(deliverytime_estimated[data$deliverydate_estimated_outliers == 0])
  data$deliverydate_estimated[year(data$deliverydate_estimated) == 4746] = data$order_date[year(data$deliverydate_estimated) == 4746] + round(median_deliverytime_estimated)

  ## Delivery date actual has 0000/00/00 which has been coerced to NA by the transformation to dates
  ## Create a dummy for the missing value
  data$deliverydate_actual_missing = ifelse(is.na(data$deliverydate_actual), 1, 0)
  
  ## index the NAs for delivery_date_actual
  na_index = which(is.na(data$deliverydate_actual))
  
  ## replace them with estimated delivery date (which are not outliers because they have just been treated)
  ## + average deviation of deliverydate_actual & deliverydate_estimated
  data$deliverydate_actual[na_index] = data$deliverydate_estimated[na_index] + median(data$deliverydate_estimated[-na_index]-data$deliverydate_actual[-na_index])
  
  ## factorise dummy variables
  data$account_creation_date_missing = factor(data$account_creation_date_missing, labels=c("no","yes"))
  data$deliverydate_actual_missing = factor(data$deliverydate_actual_missing, labels=c("no","yes"))
  data$deliverydate_estimated_outliers = factor(data$deliverydate_estimated_outliers, labels=c("no","yes"))
  
  ## Create factor variables for the weekdays
  ## since these are not captured in the deltas that follow from the next steps
  ## (No weekday for account_creation_date because the correlation with order_date is high)
  ## Encode it with a sin-function because the variable is cyclic.
  data$order_date_weekday = as.POSIXlt(data$order_date)$wday
  data$deliverydate_estimated_weekday = as.POSIXlt(data$deliverydate_estimated)$wday
  data$deliverydate_actual_weekday = as.POSIXlt(data$deliverydate_actual)$wday
  
  ## Replace date variables by deltas, since the dates are naturally strongly correlated
  ## and additional information is captured in their differences.
  ## We choose order_date as the starting point as there are no missing values or outliers.
  ## order_date captures a decision of the customer, 
  ## while delivery times and difference between estimated and actual delivery date capture the workings of the company.
  data$account_creation_date = data$account_creation_date - data$order_date
  data$deliverydate_estimated = data$deliverydate_estimated - data$deliverydate_actual
  data$deliverydate_actual = data$deliverydate_actual - data$order_date
  
  return(data)
}


# treats postcodes 
# standardize postcodes to 2 digits
# create dummy variable for missing values
# also convert data type to factor
# input: data frame
# output: data frame
treat_postcodes = function(dataset) {
  data = dataset
  
  ## 2: NAs for postcode delivery 
  # create dummy postcode_delivery_missing
  data$postcode_delivery_missing = vector(mode="integer",length=length(data$postcode_delivery))
  
  # replace missing values by NA, get rid of non-numeric postal codes
  data$postcode_invoice = sapply(data$postcode_invoice, as.character) 
  data$postcode_delivery = sapply(data$postcode_delivery, as.character)
  data$postcode_invoice = sapply(data$postcode_invoice, as.numeric) 
  data$postcode_delivery = sapply(data$postcode_delivery, as.numeric)
  
  # check if there are NAs in postcode_invoice and replace by 1 
  # this should only affect under 10 entries (where it is "??" or 0)

  data$postcode_invoice[is.na(data$postcode_invoice)] = 1
  data$postcode_invoice[data$postcode_invoice == 0] = 1
  data$postcode_delivery[data$postcode_delivery == 0] = 1 
  # no line for NA in postcode_delivery because NAs are missing entries
  
  # check if there are NAs in postcode_delivery
  if(NA %in% data$postcode_delivery)
  {
    # index NAs for later use in postcode_invoice
    na_index = which(is.na(data$postcode_delivery))
    # set true values in the dummy variable
    data$postcode_delivery_missing[na_index] = 1
    # replace missing postcodes with postcode_invoice
    # same in about 60% of cases, affects 49608 cases
    data$postcode_delivery[na_index] = data$postcode_invoice[na_index] 
  }
  
  # factorise dummy variable
  data$postcode_delivery_missing = factor(data$postcode_delivery_missing, levels=c(0,1), labels=c("no","yes"))
  data$postcodes_different = factor(ifelse(data$postcode_invoice != data$postcode_delivery, 1, 0), levels=c(0,1), labels=c("no","yes"))
  
  # factorise postcode variables
  data$postcode_invoice = factor(data$postcode_invoice)
  data$postcode_delivery = factor(data$postcode_delivery)
  ##########################################################################
  
  return(data)
}


### Normalization 
# min-max-scaling of cardinal variables to range from 0 to 1 (e.g. item count)
# input: data frame
# output: data frame
normalize_cardinal_variables = function(x) {
  min = min(x)
  max = max(x)
  normalized = (x - min)/(max - min)
  
  return(normalized)
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
  # (if a mode has been specified before this acts on the already normalized variable)
  normalized = (new_max-new_min) / (max(x)-min(x)) * (x - min(x)) + new_min
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

## Normalization function (minmax) for the entire dataset
## cannot normalize multilevel factors, therefore those are remain untouched
## input: dataset, colnames of multilevel factors
## output: dataset
normalize_dataset = function(dataset, multilevel_factors = c("return_customer", "form_of_address", "email_domain", "model", "payment", "postcode_invoice", "postcode_delivery", "advertising_code"))
{
  data = dataset
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



# input: predictions_all data frame, vector of probability prediction + informative name
# output: predictions_all dataframe with added predictions
save_prediction_to_master = function(predictions_all, newprediction, name)
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

save_prediction_to_master <- function(newprediction, predname, filename.csv, master){
  # download current version of masterfile
  master = read.csv(filename.csv, header=T, sep=",", row.names = 1)
  # check for consistency
  if(nrow(master) != length(newprediction)){
    print("Numbers of rows in master-file and length of prediction vectors do not match.")
    break
  }
  # add new column
  master[,predname] <- newprediction
  # save as csv
  write.csv(x = master, file = filename.csv)
  return(master)
}


###### -------- COST MATRIX -------- ######

build_cost_matrix <- function(CBTN = +3, CBFN = -10, CBFP = 0, CBTP = 0){
  # calculate costs with 0 on diagonals
  CFN = CBFN - CBTP
  CFP = CBFP - CBTN
  
  # build cost-matrix
  cost.matrix <- matrix(c(
    NA, CFN,
    CFP, NA),
    2, 2, byrow=TRUE)
  
  # name rows and columns
  colnames(cost.matrix) <- list("noreturn", "return")
  rownames(cost.matrix) <- list("noreturn", "return")
  
  return(cost.matrix)
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

## Function to truncate outliers in the entire dataset
treat_outliers = function(dataset)
{
  data = dataset
  
  # get the columns that include time differences
  time_diff_columns = c("deliverydate_estimated","deliverydate_actual")
  data[,time_diff_columns] = sapply(data[,time_diff_columns], truncate_outliers)
  data$account_creation_date = normalize(-data$account_creation_date, mode = "sqrt")
  data$account_creation_date[data$account_creation_date > 0] = truncate_outliers(data$account_creation_date[data$account_creation_date > 0])
  
  # get the columns that have only positive values (all item counts + weight)
  include_pattern = c("_count|_items|weight")
  exclude_pattern = c("weight_missing")
  columns_with_only_positive = setdiff(grep(include_pattern, colnames(data)), grep(exclude_pattern, colnames(data)))
  mode = "sqrt"
  if(mode == "log"){data[,columns_with_only_positive] = data[,columns_with_only_positive]+1}
  data[,columns_with_only_positive] = sapply(data[,columns_with_only_positive], normalize, mode = mode)
  
  return(data)
}

## Function to drop highly correlated variables
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