# This is the main script that runs all the other modules

#######################
# Load packages
library(lubridate)
# Try to load the package, if it doesn't exist, then install and load it
if(!require("caret")) install.packages("caret"); library("caret") 

#######################
# Load modules
source("helper.R")
source("adaptive_boosting.R")

#######################
# Set seed
set.seed(666)

#######################
# Load data
# known - training data
known = get_dataset("assignment_BADS_WS1617_known.csv")
# class - data to be classified
class = get_dataset("assignment_BADS_WS1617_class.csv")

#######################
# Treat postcodes and missing variables
known = treat_missing_values(known)
class = treat_missing_values(class)
known = treat_postcodes(known)
class = treat_postcodes(class)
known = treat_dates(known)
class = treat_dates(class)
#######################
# check plausability of data types
# lapply(known,class)

# summarise
#lapply(known,summary)
#lapply(class,summary)
#######################
# Partition the data

# Split data set into 80% training and 20% test data
# sample_size = size of the training set 
sample_size = ceiling(nrow(known)*0.8) 

# Draw a random stratified sample in which both train and test set have roughly the same ratio of the target classes.
# The function creatDataPartition returns the indices of a stratified training set with size p * size of data.

# Draw a random, stratified sample including p percent of the data
idx_train  = createDataPartition(y = known$return_customer, p = 0.8, list = FALSE) 
train_data = known[idx_train, ] # training set
test_data  =  known[-idx_train, ] # test set (drop all observations with train indices)
# Draw a random, stratified sample of ratio p of the data
# idx_validation = createDataPartition(y = train_data$return_customer, p = 0.25, list = FALSE)
# validation_data = train_data[idx_validation, ]
# train_data = train_data[-idx_validation, ]





#######################
# Try adaptive boosting

adaboost = adaptive_boosting(test_data)
summary(adaboost)
adaboost$trees
adaboost$weights
adaboost$importance
errorevol(adaboost,test_data)
predict(adaboost,test_data)

t1=adaboost$trees[[1]]

plot(t1)
text(t1,pretty=0)