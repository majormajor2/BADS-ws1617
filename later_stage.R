############ TEST ################
# Matrix of histograms
#if(!require("Hmisc")){install.packages("Hmisc")};library("Hmisc")
#hist.data.frame(train_data)

##### Drop Correlated Variables #####


######## Normalize  ###############
multilevel_factors = c("return_customer", "form_of_address", "email_domain", "model", "payment", "postcode_invoice", "postcode_delivery", "advertising_code")
## Normalization function (minmax)
## cannot normalize multilevel factors, therefore those are remain untouched
## input: dataset, colnames of multilevel factors
## output: dataset
normalize_dataset = function(dataset, multilevel_factors = c("return_customer", "form_of_address", "email_domain", "model", "payment", "postcode_invoice", "postcode_delivery", "advertising_code"))
{
  data = dataset
  print("Performing normalization on all parameters that are not multilevel factors:")
  
  for(column in colnames(data)) # loop over all columns
  {
    if(!column %in% multilevel_factors)
    {
      print(column)
      data[column] = sapply(data[column],as.numeric) # convert to numeric
      data[column] = normalize_cardinal_variables(data[column]) # run minmax-normalization
    }
  }
  return(data)
}

known_normalized = normalize_dataset(known)
class_normalized = normalize_dataset(class)

idx_train  = createDataPartition(y = known_normalized$return_customer, p = 0.8, list = FALSE)
train_data = known_normalized[idx_train, ] # training set
test_data  =  known_normalized[-idx_train, ] # test set (drop all observations with train indices)

idx_validation = createDataPartition(y = train_data$return_customer, p = 0.25, list = FALSE)
validation_data = train_data[idx_validation, ]
train_data = train_data[-idx_validation, ]


######## Plot correlation matrix ###############
correlation_matrix = cor(known_normalized[grep(paste(multilevel_factors, collapse = "|"), colnames(known_normalized), invert = TRUE)])
corrplot(correlation_matrix, title = "Correlation Matrix", type = "full", order = "AOE", tl.cex = 0.5, tl.srt = 45, mar = c(1,0,1,1))

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
deep_arch = darch(return_customer~ ., data = train_data, # darch = deep_arch,
                  layers = c(292, 6, 3, 2),
                  bp.learnRate = 1, bp.learnRateScale = 0.1, # Backpropagation Learn Rate
                  #darch.fineTuneFunction = rpropagation, 
                  darch.numEpochs = 5,
                  #dataSetValid = validation_data, # gives error
                  # darch.unitFunction = softmaxUnit,
                  darch.weightDecay = 0.05
                  )

#Helpful functions in reading rpart output
printcp(decision_tree)
plotcp(decision_tree)
summary(decision_tree)


#creating estimates for the two models + benchmark, creating a list with all of them
#prediction_lr = predict(linear_model, newdata = test_data, type = "response")
prediction_lr = predict(linear_model, newdata = test_data[grep(c("BU"),test_data$advertising_code, invert = TRUE),], type = "response")
prediction_dt = predict(decision_tree, newdata = test_data, type = "prob")[,2]
prediction_nn = as.numeric(predict(neuralnet, newdata = test_data, type = "raw"))
prediction_dn = predict(deep_arch, newdata = test_data)[,2]

######## Check predictive performance ###############

#confusionMatrix(data = prediction, reference = known$return_customer, positive = "yes")
predictive_performance(y = test_data[grep(c("AS|BH|BK|BN|BU"),test_data$advertising_code, invert = TRUE),"return_customer"], prediction = prediction_lr)
predictive_performance(y = test_data$return_customer, prediction = prediction_dt)
predictive_performance(y = test_data$return_customer, prediction = prediction_nn)
predictive_performance(y = test_data$return_customer, prediction = adaboost_prediction$prob[,2])


#######################
