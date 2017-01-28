# Weight of Evidence function to turn a factor variable into a numerical one according to their WoE
# Input: factor column
# Output: numerical column
# !!! We use the function below instead to turn all columns into WoE at once
#replace_by_woe = function(target, colnames_to_replace, dataset)
#{
#  woe_object = woe(as.formula(paste(target, paste(colnames_to_replace, collapse="+"), sep = "~")), data = dataset, zeroadj = 0.5)
#  return(woe_object$xnew)
#}

# Weight of Evidence function to turn factors with more than 2 levels into numerical variables according to their WoE
# Input: dataset
# Output: dataset with replaced factors
replace_factors_by_woe = function(dataset)
{
  target = "return_customer"
  columns_to_replace = c("form_of_address", "email_domain", "model", "payment", "postcode_invoice", "postcode_delivery", "advertising_code")
  woe_object = woe(as.formula(paste(target, paste(columns_to_replace, collapse="+"), sep = "~")), data = dataset, zeroadj = 0.5)
  dataset[, columns_to_replace] <- woe_object$xnew
  return(woe_object)
}

# calculate woe & return woe object
calculate_woe = function(dataset_train)
{
  target = "return_customer"
  columns_to_replace = c("form_of_address", "email_domain", "model", "payment", "postcode_invoice", "postcode_delivery", "advertising_code")
  woe_object = woe(as.formula(paste(target, paste(columns_to_replace, collapse="+"), sep = "~")), data = dataset_train, zeroadj = 0.5)
  return(woe_object)
}

# apply woe & return data set with columns replaced by woe
apply_woe <- function(dataset, woe_object){
  columns_to_replace = c("form_of_address", "email_domain", "model", "payment", "postcode_invoice", "postcode_delivery", "advertising_code")
  # predict
  test_data_woe <- predict(woe_object, newdata = dataset, replace = TRUE)
  # change names
  colnames(test_data_woe)[35:41] <- columns_to_replace
  return(test_data_woe)
}

# WOE-replacement for class data-set
predict_woe <- function(woe_object, newdata, grep = NULL)
{
  grep = grep(pattern = "AA", x = newdata$advertising_code, invert = TRUE)
  class_woe <- predict(woe_object, newdata = class[grep,], replace = TRUE)
  colnames(class_woe)[34:40] <- c("form_of_address", "email_domain", "model", "payment", "postcode_invoice", "postcode_delivery", "advertising_code")
  return(class_woe) 
}