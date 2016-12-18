
# getting the dataset, creating a data.frame and 
# converting variables to correct data types
# input: filename of csv-file
# output: data frame

get_dataset = function(name) {
  
  ##########################################################################
  # read csv-file
  data = read.csv(name, header=T,sep=",")
  
  ##########################################################################
  # drop data$points_redeemed because it has all zeros -> no informational value
  data$points_redeemed = NULL
  
  ##########################################################################
  # factorise ID
  data$ID = factor(data$ID)
  
  
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
  

  ## 2: Replace missing weight with mean weight for the same number of items
  # see Data_Cleaning_SF.R courtesy of Stephie
  data$weight_missing = factor(ifelse(is.na(data$weight), 1, 0), labels=c("no","yes"))
  
  return(data)
}

# treats postcodes 
# standardise postcodes to 2 digits
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

  # the following 2 lines are performed by standardise_postcode already
  #data$postcode_invoice = as.character(data$postcode_invoice)
  #data$postcode_delivery = as.character(data$postcode_delivery) 
  
  data$postcode_invoice = sapply(data$postcode_invoice, standardise_postcode)
  data$postcode_delivery = sapply(data$postcode_delivery, standardise_postcode)
  
  # check if there are NAs
  if(NA %in% data$postcode_delivery)
  {
    # index NAs for later use in postcode_invoice
    na_index = which(is.na(data$postcode_delivery))
    #print(na_index)
    # set true values in the dummy variable
    data$postcode_delivery_missing[na_index] = 1
    # replace missing postcodes with postcode_invoice
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

# standardise cardinal variables to range from 0 to 1 (e.g. item count)
# input: data frame
# output: data frame
standardise_cardinal_variables = function(dataset) {
  data = dataset
  
  # TO DO:
  
  return(data)
}


# postcode standardisation function
# takes postcode and adds a leading 0 if it has fewer than 2 chars
# input: postcode as character
# output: standardized postcode as character
standardise_postcode = function(postcode){
  # convert to character to be sure
  standardised_postcode = as.character(postcode)
  
  # if postcode has fewer than 2 characters, add a preceding 0 
  if(!(is.na(standardised_postcode)))
  {
    if(nchar(standardised_postcode, allowNA = TRUE, keepNA = TRUE)<2)
    {
      # print(standardised_postcode)
      standardised_postcode = paste("0",standardised_postcode,sep="")
      # print(standardised_postcode)
    }
  }
  
  return(standardised_postcode)
}

# general standardization function
# input: numerical column
# output: standardized numerical column
standardise <- function(x){
  mu <- mean(x)
  std <- sd(x)
  result <- (x - mu)/std
  return(result)
}