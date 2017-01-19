
######## Try adaptive boosting ###############

adaboost = adaptive_boosting(train_data[,grep("weight",train_data,invert = TRUE)])
summary(adaboost)
adaboost$trees
adaboost$weights

errorevol(adaboost,train_data[,grep("weight",train_data,invert = TRUE)])
adaboost_prediction = predict(adaboost,test_data[,grep("weight",test_data,invert = TRUE)])

# list the variables by their importance
adaboost_importance = sort(adaboost$importance, decreasing = TRUE)
adaboost_importance[adaboost_importance > 0]

# print a variable importance plot
print_importance_plot(adaboost_importance)

# plot a decision tree
adaboost_tree = adaboost$trees[[1]]

#plot(adaboost_tree)
#text(adaboost_tree,pretty=0)


######## Check predictive performance ###############

#confusionMatrix(data = prediction, reference = known$return_customer, positive = "yes")
predictive_performance(y = test_data$return_customer, prediction = adaboost_prediction$prob[,2])


#######################
