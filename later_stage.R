# source("main.R")

############ TEST ################
# Matrix of histograms
#if(!require("Hmisc")){install.packages("Hmisc")};library("Hmisc")
#hist.data.frame(train_data)

##### Drop Correlated Variables #####

dropped_correlated_variables = strongly_correlated(known, threshold = 0.4)
known[dropped_correlated_variables] = NULL
class[dropped_correlated_variables] = NULL

##### Treat Outliers #####
known = treat_outliers(known)
class = treat_outliers(class)
#known_normalized = sapply(known,truncate_outliers)
#class_normalized = sapply(class,truncate_outliers)

######## Normalize  ############
columns_to_replace = c("form_of_address", "email_domain", "model", "payment", "postcode_invoice", "postcode_delivery", "advertising_code")
woe_object = calculate_woe(known, target = "return_customer", columns_to_replace = columns_to_replace)

multilevel_factors = c("return_customer", "form_of_address", "email_domain", "model", "payment", "postcode_invoice", "postcode_delivery", "advertising_code")

known_normalized = normalize_dataset(known, multilevel_factors = multilevel_factors)
class_normalized = normalize_dataset(class, multilevel_factors = multilevel_factors)


set.seed(666)
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

### Simple models ###
linear_model = glm(return_customer ~ ., data = train_data, family = binomial(link = "logit"))
decision_tree = rpart(return_customer ~ ., data = train_data, method = "class", cp = 0.001, minbucket = 8) # minsplit also exists
naive_bayes = naiveBayes(return_customer~., data = train_data, laplace = 0.0001) 

### Adaptive Boosting ###
adaboost = boosting(return_customer~., data=train_data, boos=TRUE, mfinal=20, coeflearn='Breiman')



#### Neural Networks ###


model_control = trainControl(
  method = "cv", # 'cv' for cross validation
  number = 5, # number of folds in cross validation
  repeats = 1, # number for repeated cross validation
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  allowParallel = TRUE, # Enable parallelization if available
  returnData = FALSE # The training data will not be included in the output training object
)


#neuralnet package gives an output that is incompatible with predict
#formula_ANN = as.formula(paste("return_customer", paste(setdiff(colnames(train_data),"return_customer"), collapse = " + "), sep = " ~ "))
#ANN = neuralnet(formula_ANN,data=train_data_woe,stepmax = 100, hidden=c(5,3),linear.output=FALSE)

# Deep Neural Network - needs to be tuned
#DANN = dbn.dnn.train(train_data_woe, train_data_woe[,"return_customer"], 
#              hidden = c(5,3), 
#              learningrate_scale = 0.9)

#Training single or mutiple hidden layers neural network by BP
DANN = nn.train(train_data_woe, train_data_woe[,"return_customer"], 
                initW = NULL, initB = NULL, 
                hidden = c(24,16), 
                activationfun = "sigm", output = "sigm",
                learningrate = 0.8, learningrate_scale = 0.9, momentum = 0.5, 
                numepochs = 3, batchsize = 100, 
                hidden_dropout = 0, visible_dropout = 0)
# Training a Deep neural network with weights initialized by Stacked AutoEncoder
DANNII = sae.dnn.train(train_data_woe[,setdiff(colnames(train_data_woe),"return_customer")], train_data_woe[,"return_customer"], 
                       hidden = c(24,16), 
                       activationfun = "sigm", output = "sigm", sae_output = "sigm",
                       learningrate = 0.8, learningrate_scale = 0.9, momentum = 0.5, 
                       numepochs = 5, batchsize = 100, 
                       hidden_dropout = 0.1, visible_dropout = 0.1)

prediction_DANN = nn.predict(DANN, test_data_woe)
prediction_DANNII = nn.predict(DANNII, test_data_woe)

# Define a search grid of values to test
ANN_parms = expand.grid(decay = c(0, 10^seq(-5, 0, 1)), size = seq(3,30,3))

# Train neural network ANN with 5-fold cross validation
ANNII = train(return_customer~., data = train_data,  
            method = "nnet", maxit = 1000, trace = FALSE, # options for nnet function
            tuneGrid = nn_parms, # parameters to be tested
            metric = "ROC", trControl = model_control)

### This is required for plotting the Neural Network ###
if(!require("devtools")) install.packages("devtools"); library("devtools")
source_url("https://gist.githubusercontent.com/fawda123/7471137/raw/466c1474d0a505ff044412703516c34f1a4684a5/nnet_plot_update.r")
if(!require("reshape")) install.packages("reshape"); library("reshape")
plot.nnet(ANN)

prediction_ANN = predict(ANN, newdata = test_data, type = "prob")[,2]



# set the number of nodes and hidden layers for the neural net
number_of_nodes = 10
number_of_layers = 1
neuralnet = nnet(return_customer~ ., data = train_data, # the data and formula to be used
                  trace = FALSE, maxit = 1000, # general options
                  size = number_of_nodes, # the number of nodes in the model
                  MaxNWts = 10000) 

## Deep Neural Network
deep_arch = darch(return_customer~ ., data = train_data, # darch = deep_arch,
                  layers = c(45, 12, 6, 2),
                  bp.learnRate = 1, bp.learnRateScale = 0.1, # Backpropagation Learn Rate
                  darch.batchSize = 2,
                  #darch.fineTuneFunction = minimizeAutoencoder, 
                  darch.numEpochs = 5,
                  # xValid = validation_data, # gives error message
                  # darch.unitFunction = softmaxUnit,
                  darch.weightDecay = 0.05,
                  preProc.factorToNumeric = TRUE
                  #preProc.factorToNumeric.targets = TRUE
                  )

#Helpful functions in reading rpart output
printcp(decision_tree)
plotcp(decision_tree)
summary(decision_tree)


#creating estimates for the two models + benchmark, creating a list with all of them
#prediction_lr = predict(linear_model, newdata = test_data, type = "response")
prediction_lr = predict(linear_model, newdata = test_data[grep(c("BU"),test_data$advertising_code, invert = TRUE),], type = "response")
prediction_dt = predict(decision_tree, newdata = test_data, type = "prob")[,2]
prediction_nb = predict(naive_bayes, newdata = test_data, type = "raw")[,2]
prediction_ab = predict(adaboost,newdata = test_data)
prediction_nn = as.numeric(predict(neuralnet, newdata = test_data, type = "raw"))
prediction_dn = predict(deep_arch, newdata = test_data)

######## Check predictive performance ###############

#confusionMatrix(data = prediction, reference = known$return_customer, positive = "yes")
predictive_performance(y = test_data[grep(c("BU"),test_data$advertising_code, invert = TRUE),"return_customer"], prediction = prediction_lr)
predictive_performance(y = test_data$return_customer, prediction = prediction_dt)
predictive_performance(y = test_data$return_customer, prediction = prediction_nb)
predictive_performance(y = test_data$return_customer, prediction = prediction_nn)
predictive_performance(y = test_data$return_customer, prediction = prediction_dn)

predictive_performance(y = test_data$return_customer, prediction = adaboost_prediction$prob[,2])


#######################

adaboost_importance = sort(adaboost$importance, decreasing = TRUE)
adaboost_importance[adaboost_importance > 0]


known_normalized[names(sort(decision_tree$variable.importance))[1:(length(decision_tree$variable.importance)/3)]] = NULL
class_normalized[names(sort(decision_tree$variable.importance))[1:(length(decision_tree$variable.importance)/3)]] = NULL