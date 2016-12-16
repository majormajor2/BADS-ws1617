## Change order data to only look at Month and year

install.packages("lubridate")
library(stringr)
library(lubridate)

for (i in known$order_date)  {
  tempmonth <-  month(known$order_date[i])
  tempyear  <-  year(known$order_date[i])
  YrMonth   <- paste(tempmonth, "-", tempyear, sep = "")  
}


## add month-year variable to the known dataset

known$YrMonth <- YrMonth

## add leading 0 so dates can be sorted

str_pad(known$YrMonth, 7, pad = "0")

## aggregate


returnbymonth <- aggregate(return_customer ~ YrMonth, data = Known, sum)

as.data.frame(returnbymonth)

returnbymonth[order(returnbymonth$YrMonth),]


known$YrMonth <- YrMonth

Totreturn <- sum(Known$return_customer)

month(Known$order_date[,1])



## group variables (for instance, dates into months-year and then see how well they predict return)
