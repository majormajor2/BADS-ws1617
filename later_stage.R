######## Build models ###############

# starting with Logistic, dt and then neural networks

linear_model = glm(return_customer ~ ., data = train_data, family = binomial(link = "logit"))
decision_tree = rpart(return_customer ~ ., data = train_data, method = "class")
neuralnet <- nnet(return_customer~ ., data = train_data, # the data and formula to be used
                  trace = FALSE, maxit = 1000, # general options
                  size = nnet.sizes[n]) # the number of nodes in the model

#Helpful functions in reading rpart output
printcp(decision_tree)
plotcp(decision_tree)
summary(decision_tree)


#creating estimates for the two models + benchmart, creating a list with all of them
prediction_lr = predict(linear_model, newdata = test_data, type = "response")
prediction_dt = predict(decision_tree, newdata = test_data, type = "prob")[,2]
prediction_nn = predict(neuralnet, newdata = test_data, type = "raw")

######## Check predictive performance ###############

#confusionMatrix(data = prediction, reference = known$return_customer, positive = "yes")
predictive_performance(y = test_data$return_customer, prediction = adaboost_prediction$prob[,2])


#######################
