######## Standardize  ###############
multilevel_factors = c("form_of_address", "email_domain", "model", "payment", "postcode_invoice", "postcode_delivery", "advertising_code")
print("Performing normalization on all parameters that are not multilevel factors:")
for(column in colnames(known))
{
  if(!column %in% multilevel_factors)
  {
    print(column)
    known[column] = sapply(known[column],as.numeric)
    known[column] = normalize_cardinal_variables(known[column])
  }
}
#known[columns_to_replace] = sapply(known[-columns_to_replace], as.numeric)
#known[columns_to_replace] = sapply(known[-columns_to_replace], normalize_cardinal_variables)

idx_train  = createDataPartition(y = known$return_customer, p = 0.5, list = FALSE) 
train_data = known[idx_train, ] # training set
test_data  =  known[-idx_train, ] # test set (drop all observations with train indices)

######## Build models ###############

# starting with Logistic, dt and then neural networks
# set the number of nodes and hidden layers for the neural net
number_of_nodes = 10
number_of_layers = 1

linear_model = glm(return_customer ~ ., data = train_data, family = binomial(link = "logit"))
decision_tree = rpart(return_customer ~ ., data = train_data, method = "class", cp = 0.0001)
neuralnet = nnet(return_customer~ ., data = train_data, # the data and formula to be used
                  trace = FALSE, maxit = 1000, # general options
                  size = number_of_nodes, # the number of nodes in the model
                  MaxNWts = 10000) 
deep_arch = darch(return_customer~ ., data = train_data, bp.learnRate = 0.5)

#Helpful functions in reading rpart output
printcp(decision_tree)
plotcp(decision_tree)
summary(decision_tree)


#creating estimates for the two models + benchmart, creating a list with all of them
prediction_lr = predict(linear_model, newdata = test_data, type = "response")
prediction_dt = predict(decision_tree, newdata = test_data, type = "prob")[,2]
prediction_nn = as.numeric(predict(neuralnet, newdata = test_data, type = "raw"))

######## Check predictive performance ###############

#confusionMatrix(data = prediction, reference = known$return_customer, positive = "yes")
predictive_performance(y = test_data$return_customer, prediction = adaboost_prediction$prob[,2])


#######################
