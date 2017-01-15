

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
  ##########################################################################
  
  return(data)
}


# treats the missing values in the data frame
# input: data frame with missing values
# output: data frame with treated missing values
treat_missing_values = function(dataset) {
  data = dataset
  
  ## 1: NAs for advertising_code - create dummy advertising_code_missing 
  # see Data_Cleaning_SF.R courtesy of Stephie
  data$advertising_code[data$advertising_code == ""] = NA
  data$advertising_code_missing = factor(ifelse(is.na(data$advertising_code), 1, 0), labels=c("no","yes"))
  # drop empty level from factor
  data$advertising_code = droplevels(data$advertising_code)
  
  ## 2: Replace missing weight with mean weight for the same number of items
  # see Data_Cleaning_SF.R courtesy of Stephie
  data$weight_missing = factor(ifelse(is.na(data$weight), 1, 0), labels=c("no","yes"))
  
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
  
  ## Change 2010 to 2014, and 4746 to order_date + the mean of delivery time needed 
  year(data$deliverydate_estimated[year(data$deliverydate_estimated) == 2010]) = 2014

  deliverytime_estimated = data$deliverydate_estimated - data$order_date
  mean_deliverytime_estimated = mean(deliverytime[known$deliverydate_estimated_outliers == "no"])
  data$deliverydate_estimated[year(data$deliverydate_estimated) == 4746] = data$order_date[year(data$deliverydate_estimated) == 4746] + round(mean_deliverytime_estimated)

  ## Delivery date actual has 0000/00/00, create a dummy for the missing value
  data$deliverydate_actual_missing = ifelse(is.na(data$deliverydate_actual), 1, 0)
  
  ## and adjust existing values to match delivery date estimated 
  
  ## index the NAs for delivery_date_actual
  na_index = which(is.na(data$deliverydate_actual))
  
  ## replace them with estimated delivery date  ?? we should check that those estimated dates aren't errors/outliers themselves
  ## ..or maybe replace them by average deviation of deliverydate actual & deliverydate estimated ?
  data$deliverydate_actual[na_index] = data$deliverydate_estimated[na_index]
  
  ## factorise dummy variables
  data$account_creation_date_missing = factor(data$account_creation_date_missing, labels=c("no","yes"))
  data$deliverydate_actual_missing = factor(data$deliverydate_actual_missing, labels=c("no","yes"))
  data$deliverydate_estimated_outliers = factor(data$deliverydate_estimated_outliers, labels=c("no","yes"))
  
  return(data)
}



## postcode standardisation function 
# takes postcode and adds a leading 0 if it has fewer than 2 chars
# input: postcode as character
# output: standardized postcode as character
standardize_postcode = function(postcode){
  # convert to character to be sure
  standardized_postcode = as.character(postcode)
  
  # if postcode has fewer than 2 characters, add a preceding 0 
  if(!(is.na(standardized_postcode)))
  {
    if(nchar(standardized_postcode, allowNA = TRUE, keepNA = TRUE)<2)
    {
      # print(standardized_postcode)
      standardized_postcode = paste("0",standardized_postcode,sep="")
      # print(standardized_postcode)
    }
  }
  
  return(standardized_postcode)
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
  
  # replace missing values by NA
  data$postcode_delivery[data$postcode_delivery == ""] = NA
  
  # the following 2 lines are performed by standardize_postcode already ?? can be taken out?
  #data$postcode_invoice = as.character(data$postcode_invoice)
  #data$postcode_delivery = as.character(data$postcode_delivery) 
  
  data$postcode_invoice = sapply(data$postcode_invoice, standardize_postcode) 
  data$postcode_delivery = sapply(data$postcode_delivery, standardize_postcode)
  
  # check if there are NAs
  if(NA %in% data$postcode_delivery)
  {
    # index NAs for later use in postcode_invoice
    na_index = which(is.na(data$postcode_delivery))
    #print(na_index)
    # set true values in the dummy variable
    data$postcode_delivery_missing[na_index] = 1
    # replace missing postcodes with postcode_invoice
    data$postcode_delivery[na_index] = data$postcode_invoice[na_index] ## ?? justification? in how many cases are they the same? 
    ## .. how many cases are affected by the replacement? in %
  }
  
  # factorise dummy variable
  data$postcode_delivery_missing = factor(data$postcode_delivery_missing, levels=c(0,1), labels=c("no","yes"))
  
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


# general standardization function
# input: numerical column
# output: standardized numerical column
standardize <- function(x){
  mu = mean(x)
  std = sd(x)
  result = (x - mu)/std
  return(result)
}

# Helper function to compute measures of predictive accuracy
predictive_performance = function(y=NULL, prediction=NULL, cutoff=.5) 
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
  classification = factor(as.numeric(prediction >= cutoff), labels=levels(y)) 
  #classification = factor(as.numeric(prediction >= cutoff), labels=c("negative", "positive")) 
  classification_error = 1 - sum(y==classification) / length(y)
  
  return(list(brier_score = brier_score, classification_error = classification_error))
}
