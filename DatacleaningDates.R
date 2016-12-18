
## clear workspace, if needed
rm(list = ls())


##the data cleaning function
DatacleaningDates <- function(x) {
  
  ## Load packages that are needed 
  library(lubridate)
  
  ## Order date does not require cleaning, there are no missing values or outliers
  ## Account creation date: Create a dummy variable for NAs
  ## Replace with first order date
  
  x$account_creation_date <- as.Date(x$account_creation_date)
  x$account_creation_date_missing <- ifelse(is.na(x$account_creation_date), 1, 0)
  na_index_creation_date <- which(is.na(x$account_creation_date))
  x$account_creation_date[na_index_creation_date] <- x$order_date[na_index_creation_date]
  
  ## Delivery date estimated has outliers, from 2010 and 4746. Create a dummy to capture both
  
  x$deliverydate_estimated_outliers <- ifelse(year(x$deliverydate_estimated) == 2010 | year(x$deliverydate_estimated) == 4746, 1, 0)
  
  ## Change 2010 to 2013, 4746 to 201year(known$deliverydate_estimated[year(known$deliverydate_estimated) == 2010])
  x$deliverydate_estimated  <- as.Date(x$deliverydate_estimated)
  is.Date(x$deliverydate_estimated)
  
  year(x$deliverydate_estimated[year(x$deliverydate_estimated) == 2010]) <- year(x$deliverydate_estimated[year(x$deliverydate_estimated) == 2010]) + 4
  year(x$deliverydate_estimated[year(x$deliverydate_estimated) == 4746]) <- year(x$deliverydate_estimated[year(x$deliverydate_estimated) == 4746]) - 2733
  
  
  ## Delivery date acutal has 0000/00/00, create a dummy wth the missing value
  x$deliverydate_actual <- as.Date(x$deliverydate_actual)
  is.Date(x$deliverydate_actual)
  x$deliverydate_actual_missing <- ifelse(is.na(x$deliverydate_actual), 1, 0)
  
  ## and adjust existing values to match delivery date estimated 
  
  ## index the na's for delivery_date_actual
  na.index <- which(is.na(x$deliverydate_actual))
  
  ##replace them with estimated delivery date
  x$deliverydate_actual[is.na(x$deliverydate_actual)] <- x$deliverydate_estimated[na.index]
  
  
  return(head(x$account_creation_date_missing))
  
}
