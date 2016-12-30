# This is the main script that runs all the other modules

#######################
# Load packages
# Try to load the package, if it doesn't exist, then install and load it

# lubridate for dates
if(!require("lubridate")) install.packages("lubridate"); library("lubridate") 

# caret for classification and regression training
if(!require("caret")) install.packages("caret"); library("caret") 
# tree for classification and regression trees
if(!require("tree")) install.packages("tree"); library("tree") 
# rpart for recursive partitioning and regression trees
if(!require("rpart")) install.packages("rpart"); library("rpart")
# rpart.plot to visualize your decision trees
if(!require("rpart.plot")) install.packages("rpart.plot"); library("rpart.plot")

# adabag for adaptive boosting and bagging
if(!require("adabag")) install.packages("adabag"); library("adabag") 

# hmeasure for Area Under the Curve (alternatives are pROC and ROCR)
# hmeasure package requires all predictions to be available in one data frame
if(!require("hmeasure")) install.packages("hmeasure"); library("hmeasure")

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
# Check plausability of data types
# lapply(known,class)
# lapply(class,class)

# Summarise
#lapply(known,summary)
#lapply(class,summary)
#######################
# Partition the data

# Split data set into 80% training and 20% test data
# sample_size = size of the training set 
# sample_size = ceiling(nrow(known)*0.8) # only needed for different method of partitioning

# Draw a random stratified sample in which both train and test set have roughly the same ratio of the target classes.
# The function creatDataPartition returns the indices of a stratified training set with size p * size of data.

# Draw a random, stratified sample including p percent of the data
idx_train  = createDataPartition(y = known$return_customer, p = 0.5, list = FALSE) 
train_data = known[idx_train, ] # training set
test_data  =  known[-idx_train, ] # test set (drop all observations with train indices)
# Draw a random, stratified sample of ratio p of the data
# idx_validation = createDataPartition(y = train_data$return_customer, p = 0.25, list = FALSE)
# validation_data = train_data[idx_validation, ]
# train_data = train_data[-idx_validation, ]



#######################
# Try adaptive boosting

# !!! Don't include ID? Error message when using predict on data.frame class:
# !!! "Faktor 'ID' hat neue Stufen 51885 [...]"
adaboost = adaptive_boosting(train_data[,grep("weight",train_data,invert = TRUE)])
summary(adaboost)
adaboost$trees
adaboost$weights
adaboost$importance
errorevol(adaboost,train_data[,grep("weight",train_data,invert = TRUE)])
adaboost_prediction = predict(adaboost,test_data[,grep("weight",test_data,invert = TRUE)])

adaboost_tree = adaboost$trees[[1]]

plot(adaboost_tree)
text(adaboost_tree,pretty=0)


#######################
# Check predictive performance

#confusionMatrix(data = prediction, reference = known$return_customer, positive = "yes")
predictive_performance(y = test_data$return_customer, prediction = adaboost_prediction$prob[,2])


#######################
