### Custom function for train.control

# use avg retrun as metric to choose the model
# output: average return

### Optimal Cutpoints

if(!require("OptimalCutpoints")) install.packages("OptimalCutpoints"); library("OptimalCutpoints")
source("performance_measures.R")


###### --------- control.cutpoints ---------  ######

# input: dataframe with predictions

optimalCutpoint <- function(data, lev = NULL, model = NULL){
  
  # GET COST MATRIX
  cost.matrix <- build_cost_matrix()
  
  # MODEL.CONTROL
  # method: maxKappa
  model.control.optc = control.cutpoints(CFP = -cost.matrix[2,1], CFN = -cost.matrix[1,2], costs.ratio = -cost.matrix[2,1]/-cost.matrix[1,2], weighted.Kappa = TRUE)

    # RUN OPTIMAL CUTPOINTS
    oc = optimal.cutpoints(
      X = "yes", 
      status = "obs", 
      tag.healthy = "no", 
      methods = "MCT", 
      data = data, 
      control = model.control.optc)
    
    # SELECT OPTIMAL CUTPOINT & store hmeasure and AUC in dataframe
    # define temporary dataframes to store cutoffs 
    df <- data.frame(cutoff = oc$MCT$Global$optimal.cutoff$cutoff)
    # check if cutpoint unique                  
    for(index in 1:length(oc$MCT$Global$optimal.cutoff$cutoff)){
      # optimal cutpoint
      df[index,"avg_return"] <- predictive_performance(data[,"obs"], prediction = data[,"yes"], cutoff = df[index,"cutoff"], returnH = FALSE)$avg_return
    }
    opt.cutoff <- df[df$avg_return == max(df$avg_return), "cutoff"]
    avg_return <- predictive_performance(y = data$obs, prediction = data$yes, cutoff = opt.cutoff, returnH = FALSE)$avg_return
    
    
   
  # OUTPUT
  return(avg_return)
}




