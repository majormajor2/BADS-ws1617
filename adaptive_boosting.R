#######################
# Load packages
library(adabag)
library(tree)

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
