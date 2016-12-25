
## clear workspace, if needed
rm(list = ls())


##the data cleaning function
DatacleaningDates <- function(x) {
  x = known
  ## Load packages that are needed 
  if(!require("lubridate")) install.packages("lubridate"); library("lubridate")
  
  ## Order date does not require cleaning, there are no missing values or outliers ?? justification? what was done to check?
  ## Account creation date: Create a dummy variable for NAs
  ## Replace with first order date
  
  known$account_creation_date <- as.Date(known$account_creation_date)
  known$account_creation_date_missing <- ifelse(is.na(known$account_creation_date), 1, 0)
  na_index_creation_date <- which(is.na(known$account_creation_date))
  known$account_creation_date[na_index_creation_date] <- known$order_date[na_index_creation_date]
  
  ## Delivery date estimated has outliers, from 2010 and 4746. Create a dummy to capture both
  
  known$deliverydate_estimated_outliers <- ifelse(year(known$deliverydate_estimated) == 2010 | year(known$deliverydate_estimated) == 4746, 1, 0)
  
  ## Change 2010 to 2013, 4746 to 201year(known$deliverydate_estimated[year(known$deliverydate_estimated) == 2010]) ?? not clear & justification?
  known$deliverydate_estimated  <- as.Date(known$deliverydate_estimated)
  is.Date(known$deliverydate_estimated)
  
  year(known$deliverydate_estimated[year(known$deliverydate_estimated) == 2010]) <- year(known$deliverydate_estimated[year(known$deliverydate_estimated) == 2010]) + 4
  year(known$deliverydate_estimated[year(known$deliverydate_estimated) == 4746]) <- year(known$deliverydate_estimated[year(known$deliverydate_estimated) == 4746]) - 2733
  
  
  ## Delivery date acutal has 0000/00/00, create a dummy wth the missing value
  known$deliverydate_actual <- as.Date(known$deliverydate_actual)
  is.Date(known$deliverydate_actual)
  known$deliverydate_actual_missing <- ifelse(is.na(known$deliverydate_actual), 1, 0)
  
  ## and adjust existing values to match delivery date estimated 
  
  ## index the na's for delivery_date_actual
  na.index <- which(is.na(known$deliverydate_actual))
  
  ##replace them with estimated delivery date
  known$deliverydate_actual[is.na(known$deliverydate_actual)] <- known$deliverydate_estimated[na.index]
  
  
  return(head(known$account_creation_date_missing))
  
}
