# Preprocessing
-----------------------------
# Packages
if(!require("lubridate")) install.packages("lubridate"); library(lubridate)
if(!require("corrplot")) install.packages("corrplot"); library(corrplot) 
if(!require("ggplot2")) install.packages("ggplot2"); library(ggplot2) 
-----------------------------
### 1. form_of_address ###
# str(known$form_of_address) : 3 levels : Company, Mr, Mrs
# check for missing values
sum(is.na(known$form_of_address)) # 6.866 values are missing

# stats: calculate percentage of 3 levels + NA
form_of_address_na = sum(is.na(known$form_of_address))/nrow(known) 
company <- as.numeric(summary(known$form_of_address)[1])/nrow(known)
mr <- as.numeric(summary(known$form_of_address)[2])/nrow(known)
mrs <- as.numeric(summary(known$form_of_address)[3])/nrow(known)

# give data frame and plot pie with 4 categories & percentages
address_stats <- c(company, mr, mrs, form_of_address_na) # define vector with 4 categoires
address_stats_pct <- round(address_stats/sum(address_stats)*100) # round to full integers
labels.address <- paste(c("company", "mr", "mrs", "address_missing"), pct, "%", sep = " ") # define labels
data.frame(address_stats, row.names = c("company", "mr", "mrs", "address_missing")) # data frame of percentages
pie(x = address_stats, labels = labels.address, main = "Form of address")

### Stuff to do for form_of_adress
# 1. give percentages with 2 decimal places and "%" sign
# 2. clean up code (sapply)
###

-----------------------------
### DATES ###
# give me variable names that have a date
selectdatevars = function(data){
datevarnames = colnames(data)[grep(pattern = "date", x = colnames(data))]
return(datevarnames)
}
datecolumn = selectdatevars(known) # safe data variables in a list
c(datecolumn)

# transform to Date
for(header in c(date_columns))
{
  known[,header] = as.Date(known[,header])
}

### account_creation_date
if(!require("lubridate")) install.packages("lubridate"); library(lubridate)
# original data:
# typeof(known$account_creation_date) # data type integer
# str(known$account_creation_date) # factor with 258 levels

# check for missing values
sum(is.na(known$account_creation_date)) # 3412 NAs

# convert to Date-type 
known$account_creation_date <- as.Date(known$account_creation_date) # also needed for lubridate package
# class(known$account_creation_date)
# sum(is.na(known$account_creation_date)) # as.Date leaves NAs as they are

# use package lubridate
# split column up into quarteryear, month, week, weekdays
# week
known$acc_creation_week = week(x = known$account_creation_date)
#head(known$acc_creation_week) # works
#head(known$account_creation_date) # compare with original dates

# weekends and weekdays
# create vector of weekdays
weekdays1 = c("Mo", "Di", "Mi", "Do", "Fr")
known$wDay <- factor((weekdays(known$account_creation_date, abbreviate = TRUE) %in% weekdays1), levels=c(FALSE, TRUE), labels=c("weekend", "weekday") )
# head(known$wDay)
# weekends and weekdays per calender week 
weekend = known[known$wDay == "weekend", "acc_creation_week"]
length(weekend)
# weekends: 35%
(summary(known$wDay)[1])/nrow(known) # this will take the numeric part in summary(..)[1]
# weekdays: 64%
print((summary(known$wDay)[2])/nrow(known)) 

# split weeks into weekday and weekend
weekday = known[known$wDay == "weekday", "acc_creation_week"]
weekend = known[known$wDay == "weekend", "acc_creation_week"]

# month
known$acc_creation_month = month(x= known$account_creation_date, label = FALSE)
head(known$acc_creation_month) # works
head(known$account_creation_date) # compare with original dates
length(known$acc_creation_month) # works

# quarter
known$acc_creation_quarter = quarter(x = known$account_creation_date, with_year = TRUE)
head(known$acc_creation_quarter) # works
head(known$account_creation_date) # compare with original dates

# Distribution of account_creation_date 
hist(x = known$account_creation_date, breaks = "months") 
hist(x = known$account_creation_date, breaks = "days")

# correlation between order_date & account_creation_date
known$acc.creation_order.date <- known$account_creation_date == known$order_date
summary(known$acc.creation_order.date)
stats_firsties <- (as.numeric(summary(known$acc.creation_order.date)[3])/nrow(known))*100

# Variable ID - ID is unique 
idunique <- unique(known$ID) # unique() returns vector/data frame with like (x) but with duplicate elements/rows removed
length(idunique) # length() returns no of rows in vector idunique
nrow(known) # returns no of rows 
## Return answer, whether ID is unique (i.e. if ID count equals no rows in known)
if (length(idunique) == nrow(known)) {
  print("Variable ID is unique identifier for each row")
} else {
  print("Variabel ID is NOT unique")
}

# order_date -  !can't calculate with it
# why important: time passed since last order indicative
# is factor with 356 levels
class(known$order_date)

## convert to Data-type
known$order_date <- as.Date(known$order_date)
class(known$order_date)
# order_date_ym <- strftime(order_date, "%Y/%m") save specific attributes of Date
# Distribution of order_date per month
hist(x = known$order_date, breaks = "months") 
hist(x = known$order_date, breaks = "days")
?Date
# create variables for month & weeks for each observation, Source: https://www.r-bloggers.com/plot-weekly-or-monthly-totals-in-r/
known$order_month <- as.Date(cut(known$order_date, breaks = "month"))
known$order_week <- as.Date(cut(known$order_date, breaks = "week", start.on.monday = TRUE))
# show histogramm of no of orders per week
ggplot(data = known, aes(order_week)) + geom_histogram(binwidth = 2, show.legend = TRUE)
ggplot(data = known, aes(order_month)) + geom_histogram(binwidth = 2, show.legend = TRUE)
sort(table(known$order_week))

# time span orders
firstorder <- names(table(known$order_date))[1]
lastorder <- names(tail(table(known$order_date)))[6]
firstorder - lastorder

#time span account creation date
names(table(data$account_creation_date))[1]
tail(table(data$account_creation_date))

#orders
summary(data$goods_value)
hist(data$goods_value)#goods_value ranges from 1-5???
summary(data$item_count)
hist(data$item_count)#majority of people orders only 1 item

#return_customers
summary(data$return_customer)
str(data$return_customer)
returnc <- sum(data$return_customer == 1)
noreturnc <- sum(data$return_customer == 0)
check <- noreturnc+returnc
#return rate
noreturnc/check; returnc/check
#return goods
rgoods <- data[data$return_customer == 1, "goods_value"]
ritems <- data[data$return_customer == 1, "item_count"]
hist.data.frame(rgoods, ritems) 

rdomain <- data[data$return_customer == 1, "email_domain"]
raccount <- data[data$return_customer == 1, "account_creation_date"]
rid <- data[data$return_customer == 1, "ID"]
rtitle <- data[data$return_customer == 1, "title"]

rsummary <- matrix(c(rdomain, raccount, rid, rtitle), nrows = return, ncol = 4, byrow = TRUE)
rsummary


