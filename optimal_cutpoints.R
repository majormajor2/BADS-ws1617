### Optimal Cutpoints

if(!require("OptimalCutpoints")) install.packages("OptimalCutpoints"); library("OptimalCutpoints")
source("performance_measures.R")


###### --------- control.cutpoints ---------  ######

# input: dataframe with predictions

get_optimal_cutpoint <- function(dataframe = df_predictions_test){
  
  # GET COST MATRIX
  cost.matrix <- build_cost_matrix()
 
  # MODEL.CONTROL
  model.control.optc = control.cutpoints(CFP = -cost.matrix[2,1], CFN = -cost.matrix[1,2])
    
  # GET NAMES OF PREDICTIONS
  predictions <- colnames(dataframe)[-1]
  
 
  # RUN OPTIMAL CUTPOINTS
    oc = optimal.cutpoints(
      X = c(predictions), 
      status = "return_customer", 
      tag.healthy = "no", 
      methods = "MCT", 
      data = dataframe, 
      control = model.control.optc)
  return(oc)
}


