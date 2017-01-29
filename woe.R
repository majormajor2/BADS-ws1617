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
  gsub(pattern = "woe.", replacement = "", x = colnames(dataset_woe))
  # Return the dataset with replaced columns
  return(dataset_woe)
}