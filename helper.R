
# getting the dataset, creating a data.frame and 
# converting variables to correct data types
# input: filename of csv-file
# output: data frame

get_dataset = function(name) {
  
  # read csv-file
  data = read.csv(name, header=T,sep=",")
  
  
  # drop data$points_redeemed because it has all zeros -> no informational value
  data$points_redeemed = NULL

  
  # factorise ID
  data$ID = factor(data$ID)
  
  # factorise postcodes
  data$postcode_invoice = factor(data$postcode_invoice)
  data$postcode_delivery = factor(data$postcode_delivery)
  
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
  

  #data$YOB[data$YOB==99] = NA
  #data$YOB_missing = factor(ifelse(is.na(data$YOB), 1, 0), labels=c("no","yes"))
  # trade off between information carried in dummy variables and increase in dimensionality
  #data$YOB[is.na(data$YOB)] = median(data$YOB, na.rm = TRUE)


  return(data)
}


# treats the missing values in the data frame
# input: data frame with missing values
# output: data frame with treated missing values
exterminate_missing_values = function(dataset) {
  data = dataset
  
  # TO DO:
  
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