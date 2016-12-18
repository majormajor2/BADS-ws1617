source("helper.R")

## clear workspace, if needed
rm(list = ls())


##the data cleaning function
DatacleaningDates <- function(x) {
  
  ## Load packages that are needed 
  library(lubridate)
  
  ## Order date does not require cleaning, there are no missing values or outliers
  ## Account creation date: Create a dummy variable for NAs
  
  known$account_creation_date_missing <- ifelse(is.na(known$account_creation_date), 1, 0)
  
  ## Delivery date estimated has outliers, from 2010 and 4746. Create a dummy to capture both
  
  known$deliverydate_estimated_outliers <- ifelse(year(known$deliverydate_estimated) == 2010 | year(known$deliverydate_estimated) == 4746, 1, 0)
  
  ## Change 2010 to 2013, 4746 to 201year(known$deliverydate_estimated[year(known$deliverydate_estimated) == 2010])
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

if(!require("caret")) install.packages("caret"); library("caret")
if(!require("rpart")) install.packages("rpart"); library("rpart")

known$return_customer <- as.factor(known$return_customer)

idx.train <- createDataPartition(y = known$return_customer, p = 0.8, list = FALSE) 
# Draw a random, stratified sample including p percent of the data
train <- known[idx.train, grep("postcode_delivery", invert = TRUE, colnames(known))] # training set
test <-  known[-idx.train, grep("postcode_delivery", invert = TRUE, colnames(known))] # test set (drop all observations with train indeces)
idx.validation <- createDataPartition(y = train$return_customer, p = 0.25, list = FALSE) 
# Draw a random, stratified sample of ratio p of the data
validation <- train[idx.validation, ]
train60 <- train[-idx.validation, ]

# Develop models using the training set and compute test set predictions
dt      <-rpart(return_customer ~ ., data = train60)
dt.full <-rpart(return_customer ~ ., data = train60, cp = 0, minsplit = 3) # low minimum increase or number of observations in node for a split to be attempted
dt.prunedLess <- rpart(return_customer ~ ., data = train60, cp = 0.005) # create decision tree classifier
dt.prunedMore <- rpart(return_customer ~ ., data = train60, cp = 0.015) # create decision tree classifier
lr <-glm(return_customer~., data = validation, family = binomial(link = "logit"))


modelList <- list("dt" = dt, "dt.full" = dt.full, "dt.prunedLess" = dt.prunedLess, "dt.prunedMore" = dt.prunedMore)
yhat.dt <- lapply(modelList, function(x) predict(x, newdata = validation, type = "prob")[,2])
yhat.lr <- predict(lr, newdata = validation, type = "response")
yhat.benchmark <- rep(sum(train60$BAD == "good")/nrow(train60), nrow(validation))
yhat.validation <- c(list("dt" = yhat.dt, "benchmark" = yhat.benchmark))


modelList <- list("dt" = dt, "dt.full" = dt.full, "dt.prunedLess" = dt.prunedLess, "dt.prunedMore" = dt.prunedMore)

yhat.dt <- predict(dt, newdata = validation, type = "prob")[,2]
yhat.benchmark <- rep(sum(train60$return_customer == 0)/nrow(train60), nrow(validation))

?
predict

y.validation <- as.numeric(validation$return_customer)-1

BrierScore <- function(y, yhat){
  sum((y - yhat)^2) / length(y) 
}
# Apply the brierScore function to each element of y.validation, i.e. all the prediction vectors
brier.validation <- sapply(yhat.validation, BrierScore, y = y.validation, USE.NAMES = TRUE)
print(brier.validation)

tau <- 0.5
# Deal with logistic regression:
#  convert probability prediction to discrete class predictions

yhat.dt.class <- factor(yhat.validation$dt > tau, labels = c(1,0))

# We can create a simple confusion table with base function table.
# Using, for example, the logit classifier, this equates to:
table(yhat.dt.class, validation$return_customer)
head(yhat.validation$dt)


