### Optimal Cutpoints

if(!require("OptimalCutpoints")) install.packages("OptimalCutpoints"); library("OptimalCutpoints")
source("performance_measures.R")


###### --------- control.cutpoints ---------  ######

# input: dataframe with predictions

get_optimal_cutpoint <- function(dataframe_pred, dataframe_perf){
  
  # CHECK FOR NEW PREDICTIONS
  idx <- which(!colnames(dataframe_pred)[-1] %in% colnames(dataframe_perf))
  predictions <- colnames(dataframe_pred)[idx]
  
  # GET COST MATRIX
  cost.matrix <- build_cost_matrix()
 
  # MODEL.CONTROL
  # method: maxKappa
  model.control.optc = control.cutpoints(CFP = -cost.matrix[2,1], CFN = -cost.matrix[1,2], costs.ratio = -cost.matrix[2,1]/-cost.matrix[1,2], weighted.Kappa = TRUE)

  # loop over all predictions
  for(pred in predictions){
    
    # RUN OPTIMAL CUTPOINTS
    oc = optimal.cutpoints(
      X = pred, 
      status = "return_customer", 
      tag.healthy = "no", 
      methods = "MCT", 
      data = dataframe_pred, 
      control = model.control.optc)
    
    # SELECT OPTIMAL CUTPOINT & store hmeasure and AUC in dataframe
    # define temporary dataframes to store cutoffs 
    df <- data.frame(cutoff = oc$MCT$Global$optimal.cutoff$cutoff)
    # check if cutpoint unique                  
    for(index in 1:length(oc$MCT$Global$optimal.cutoff$cutoff)){
      # optimal cutpoint
      df[index,"avg_return"] <- predictive_performance(dataframe_pred$return_customer, prediction = dataframe_pred[,pred], cutoff = df[index,"cutoff"])$avg_return
      }
    optimalcutpoint <- df[df$avg_return == max(df$avg_return), "cutoff"]
      
    # STORE RESULTS IN DATAFRAME 
    # calculate predictive_performance again
    predictive.performance <- predictive_performance(dataframe_pred$return_customer, prediction = dataframe_pred[,pred], cutoff = optimalcutpoint)
    # store performance measures
    dataframe_perf[dataframe_perf[,"metrics"] == "AUC", pred] <- predictive.performance$area_under_curve
    dataframe_perf[dataframe_perf[,"metrics"] == "hmeasure", pred] <- predictive.performance$h_measure
    dataframe_perf[dataframe_perf[,"metrics"] == "cutoff", pred] <- optimalcutpoint
    dataframe_perf[dataframe_perf[,"metrics"] == "avg_return", pred] <- predictive.performance$avg_return

    # PRINT
    print(paste("model: ", pred, "; cutoff: ",(dataframe_perf[3, pred]), "; avg_return: ", dataframe_perf[4, pred], sep = ""))
    }
  
  # OUTPUT
  return(dataframe_perf)
}




