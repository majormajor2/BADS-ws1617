

DatacleaningDates <- function(x) {
  
  ## Load packages that we need
  library(lubridate)
  
  ## Order date does not require cleaning, there are no missing values or outliers
  ## Account creation date: Create a dummy variable for NAs
  
  x$account_creation_date_missing <- ifelse(is.na(x$account_creation_date), 1, 0)
  
  ## Delivery date estimated has outliers, from 2010 and 4746. Create a dummy to capture both
  ## Change 2010 to 2013, 4746 to 2014
  
  x$deliverydate_estimated_outliers <- ifelse(year(x$deliverydate_estimated) == 2010 | year(x$deliverydate_estimated) == 4746, 1, 0)
  
  
  return(head(x$account_creation_date_missing))
  return(head(x$deliverydate_estimated_outliers))
         
}

DatacleaningDates(known)




