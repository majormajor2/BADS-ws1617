#get data
setwd("/Users/sfinkenwirth/Documents/MEMS/Lectures and Seminars/Business Analytics & Data Science/Assignment_BADS_WS1617/Input")
bads <- read.csv("assignment_BADS_WS1617_known.csv", header = TRUE, sep = ",")
if(!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)

#get list of names and classes
vtypes_known <- write.csv(sapply(data, class))
vtypes_class <- write.csv(sapply(data_class, class))

#for histograms
par(mar=c(1,1,1,1)) # this code avoids some errors related to screen resolution; feel free to ignore it

# structure of dataset
str(bads)
summary(bads)

# --------------------------------------------------------

# form_of_address - 13% missing
str(bads$form_of_address)
as.numeric(summary(bads$form_of_address)[1])
address_missing = sum(is.na(bads$form_of_address))/nrow(bads)
# stats
company <- as.numeric(summary(bads$form_of_address)[1])/nrow(bads)
mr <- as.numeric(summary(bads$form_of_address)[2])/nrow(bads)
mrs <- as.numeric(summary(bads$form_of_address)[3])/nrow(bads)

address_stats <- c(company, mr, mrs, address_missing)
pct <- round(address_stats/sum(address_stats)*100)
labels.address <- paste(c("company", "mr", "mrs", "address_missing"), pct, "%", sep = " ")
data.frame(address_stats, row.names = c("company", "mr", "mrs", "address_missing"))

## Summary form_of_address
pie(x = address_stats, labels = labels.address, main = "Form of address")

# ----------------------------------------------------------

# account_creation_date
str(bads$account_creation_date) # factor with 258 levels
summary(bads$account_creation_date) # 3412 NAs
# converte to Date-type
bads$account_creation_date <- as.Date(bads$account_creation_date)

# Distribution of account_creation_date 
hist(x = bads$account_creation_date, breaks = "months") 
hist(x = bads$account_creation_date, breaks = "days")

# correlation between order_date & account_creation_date
bads$acc.creation_order.date <- bads$account_creation_date == bads$order_date
summary(bads$acc.creation_order.date)
stats_firsties <- (as.numeric(summary(bads$acc.creation_order.date)[3])/nrow(bads))*100


# Variable ID - ID is unique 
idunique <- unique(bads$ID) # unique() returns vector/data frame with like (x) but with duplicate elements/rows removed
length(idunique) # length() returns no of rows in vector idunique
nrow(bads) # returns no of rows 
## Return answer, whether ID is unique (i.e. if ID count equals no rows in bads)
if (length(idunique) == nrow(bads)) {
  print("Variable ID is unique identifier for each row")
} else {
  print("Variabel ID is NOT unique")
}

# order_date -  !can't calculate with it
# why important: time passed since last order indicative
# is factor with 356 levels
class(bads$order_date)

## convert to Data-type
bads$order_date <- as.Date(bads$order_date)
class(bads$order_date)
# order_date_ym <- strftime(order_date, "%Y/%m") save specific attributes of Date
# Distribution of order_date per month
hist(x = bads$order_date, breaks = "months") 
hist(x = bads$order_date, breaks = "days")
?Date
# create variables for month & weeks for each observation, Source: https://www.r-bloggers.com/plot-weekly-or-monthly-totals-in-r/
bads$order_month <- as.Date(cut(bads$order_date, breaks = "month"))
bads$order_week <- as.Date(cut(bads$order_date, breaks = "week", start.on.monday = TRUE))
# show histogramm of no of orders per week
ggplot(data = bads, aes(order_week)) + geom_histogram(binwidth = 2, show.legend = TRUE)
ggplot(data = bads, aes(order_month)) + geom_histogram(binwidth = 2, show.legend = TRUE)
sort(table(bads$order_week))

# time span orders
firstorder <- names(table(bads$order_date))[1]
lastorder <- names(tail(table(bads$order_date)))[6]
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


# Which variables are correlated with return_customer?
# Are there clusters in order_date? Are these clusters correlated with return_customer?
# How much time lies between account_creation_date and order_date?
# Correlation between coupon and return_customer?
# What have customers shopped? 
