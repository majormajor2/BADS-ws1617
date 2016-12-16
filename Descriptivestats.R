## Change order data to only look at Month and year

install.packages("lubridate")
library(stringr)
library(lubridate)
library(ggplot2)
library(foreach)

datelist <- list(known$order_date, known$account_creation_date, known$deliverydate_estimated, known$deliverydate_actual)

order_date_new <- known$order_date
as.vector(order_date_new)

tempmonth_1 <-  month(order_date_new)
str_pad(tempmonth_1, 2, pad = "0")
tempyear_1  <-  year(order_date_new)
YrMonth_order_date_new   <- paste(tempyear_1, "-", tempmonth_1, sep = "")  

account_creation_date_new <- known$account_creation_date
as.vector(account_creation_date_new)

tempmonth_2 <-  month(account_creation_date_new)
str_pad(tempmonth_2, 2, pad = "0")
tempyear_2  <-  year(account_creation_date_new)
YrMonth_account_creation_date_new   <- paste(tempyear_2, "-", tempmonth_2, sep = "")  

deliverydate_estimated_new <- known$deliverydate_estimated
as.vector(deliverydate_estimated_new)

tempmonth_3 <-  month(deliverydate_estimated_new)
str_pad(tempmonth_3, 2, pad = "0")
tempyear_3  <-  year(deliverydate_estimated_new)
YrMonth_deliverydate_estimated_new   <- paste(tempyear_3, "-", tempmonth_3, sep = "")  

deliverydate_actual_new <- known$order_date
as.vector(deliverydate_actual_new)

tempmonth_4 <-  month(deliverydate_actual_new)
str_pad(tempmonth_4, 2, pad = "0")
tempyear_4  <-  year(deliverydate_actual_new)
YrMonth_deliverydate_actual_new   <- paste(tempyear_4, "-", tempmonth_4, sep = "")  



## add month-year variable to the known dataset

known$YrMonth_order_date_new <- YrMonth_order_date_new
known$YrMonth_account_creation_date_new <- YrMonth_account_creation_date_new
known$YrMonth_deliverydate_estimated_new <- YrMonth_deliverydate_estimated_new
known$YrMonth_deliverydate_actual_new <- YrMonth_deliverydate_actual_new


## add leading 0 so dates can be sorted

str_pad(known$YrMonth, 6, pad = "0")

## aggregate data on returning customers over months


returnbymonth_orderdate <- aggregate(return_customer ~ YrMonth_order_date_new, data = known, sum)
returnbymonth_accountcreation <- aggregate(return_customer ~ YrMonth_account_creation_date_new, data = known, sum)
returnbymonth_deliveryest <- aggregate(return_customer ~ YrMonth_deliverydate_estimated_new, data = known, sum)
returnbymonth_deliveryact <- aggregate(return_customer ~ YrMonth_deliverydate_actual_new, data = known, sum)

as.data.frame(returnbymonth_orderdate)
as.data.frame(returnbymonth_accountcreation)
as.data.frame(returnbymonth_deliveryest)
as.data.frame(returnbymonth_deliveryact)

#create a time index and sort data 
returnbymonth$t <-c(7:9, 1:6, 10:12)

returnbymonth[order(returnbymonth$t),]

##create %return_customer by months
Totreturn <- sum(known$return_customer)

returnbymonth_orderdate$percentreturn <- returnbymonth_orderdate$return_customer*100/Totreturn
returnbymonth_accountcreation$percentreturn <- returnbymonth_accountcreation$return_customer*100/Totreturn
returnbymonth_deliveryest$percentreturn <- returnbymonth_deliveryest$return_customer*100/Totreturn
returnbymonth_deliveryact$percentreturn <- returnbymonth_deliveryact$return_customer*100/Totreturn


ggplot(data = returnbymonth, aes(x = YrMonth, y = percentreturn)) + geom_point()
ggplot(data = returnbymonth, aes(x = YrMonth, y = percentreturn)) + geom_point() + geom_line()

## Create a list of all columns with dates - to check distribution of customers across dates ## needs to be finished

datelist <- list(known$order_date, known$account_creation_date, known$deliverydate_estimated, known$deliverydate_actual)

lapply(datelist_df, as.POSIXlt)
lapply(datelist, monthyear)

monthyear <- function(x) {
  tempmonth <-  month(x)
  tempyear  <-  year(x)
  YrMonth   <- as.data.frame(paste(tempyear, "-", tempmonth, sep = ""))  
  }

datelist_df <- data.frame(known$order_date, known$account_creation_date, known$deliverydate_estimated, known$deliverydate_actual)