#######################
# Load packages
# adabag for adaptive boosting and bagging
if(!require("adabag")) install.packages("adabag"); library("adabag") 
# tree for classification and regression trees
if(!require("tree")) install.packages("tree"); library("tree") 

#######################

# calculates an adaptive boosting (adaboost) model
# input: data frame
# output: model
adaptive_boosting = function(dataset)
{
  data = dataset
  adaboost = boosting(return_customer~., data=data, boos=TRUE, mfinal=20, coeflearn='Breiman')
  return(adaboost)
}
#######################
