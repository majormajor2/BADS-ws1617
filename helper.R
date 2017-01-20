

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
  
  ## 1: NAs for advertising_code
  # see Data_Cleaning_SF.R courtesy of Stephie
  data$advertising_code[data$advertising_code == ""] = NA
  # dummy variable is no longer needed as NAs are encoded as level "Missing"
  #data$advertising_code_missing = factor(ifelse(is.na(data$advertising_code), 1, 0), labels=c("no","yes"))
  # drop empty level from factor
  data$advertising_code = droplevels(data$advertising_code)
  data$advertising_code = fct_explicit_na(data$advertising_code, "Missing")
  
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

# Weight of Evidence function to turn the factor variables into numbers according to their WoE
# Input: factor column
# Output: Numerical culumn
weights_of_evidence <- function(dataset){
  woe_object = woe(return_customer ~ form_of_address + title + email_domain + model + payment + delivery + postcode_invoice + postcode_delivery + advertising_code, data = dataset, zeroadj = 0.5)
  return(woe_object$xnew)
}


# Weight of Evidence function to turn factors with more than 2 levels into numerical variables according to their WoE
# Input: dataset
# Output: dataset with replaced factors
replace_factors_by_woe = function(dataset)
{
  target = "return_customer"
  columns_to_replace = c("form_of_address", "email_domain", "model", "payment", "postcode_invoice", "postcode_delivery", "advertising_code")
  dataset[,columns_to_replace] = replace_by_woe(target, columns_to_replace, dataset)
  return(dataset)
}

# Weight of Evidence function to turn a factor variable into a numerical one according to their WoE
# Input: factor column
# Output: numerical column
replace_by_woe = function(target, colnames_to_replace, dataset)
{
  woe_object = woe(as.formula(paste(target, paste(colnames_to_replace, collapse="+"), collapse = "~")), data = dataset, zeroadj = 0.5)
  return(woe_object$xnew)
}
