###### Calculation #################

# calculates an adaptive boosting (adaboost) model
# input: data frame
# output: model
adaptive_boosting = function(dataset)
{
  data = dataset
  adaboost = boosting(return_customer~., data=data, boos=TRUE, mfinal=20, coeflearn='Breiman')
  return(adaboost)
}
###### Printing #################

print_importance_plot = function(importance_list)
{
  dev.off()
  pdf(file = "plot_adaboost_importance.pdf")
  par(mar = c(12, 4, 4, 2) + 0.2) # increase margins such that the labels are on the plot
  barplot(importance_list[importance_list > 0], las=2)
  dev.off()
}