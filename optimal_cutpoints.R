### Optimal Cutpoints

if(!require("OptimalCutpoints")) install.packages("OptimalCutpoints"); library("OptimalCutpoints")
source("performance_measures.R")


###### --------- control.cutpoints ---------  ######

###### --------- 1. RUN THIS ---------  ######

# FUNCTION FOR DATA FRAME
# Input 1: target variable (assumed to be a factor column) 
# Input 2: a varying number of predictions for the target (each a vector of probabilities)
# Output: a dataframe
assemble_data_frame_opc = function(return_customer, ...)
{
  data_prediction = data.frame(return_customer=as.numeric(return_customer)-1, ...)
  return(data_prediction)  
}

# FUNCTION FOR MODEL.CONTROL
specify_model_controls_opc = function()
  {
  # COSTS
  # cost-benefit matrix
  CBTN = +3
  CBFN = -10
  CBFP = 0
  CBTP = 0
  # cost-matrix
  CFN = CBFN - CBTP
  CFP = CBFP - CBTN
  
  # MODEL.CONTROL
  ### ??? ### Should this object model.control.optc not be returned as the output of the function?
  model.control.optc = control.cutpoints(
    costs.ratio = (CBFP-CBTN)/(CBFN-CBTP), CFP = -CFP, CFN = CFN
  ) 
}

###### --------- 2. ADD YOUR PREDICTIONS TO DATA FRAME ---------  ######
df.prediction = assemble_data_frame_opc(test_data_woe$return_customer
                                         #,placeholder1
                                         #,placeholder1
                                         #,placeholder1
                                         #,placeholder1
                                         )



####### 1. MODEL.CONTROL ####### 


  

####### 2. MODEL  ####### 
  
# DATA FRAME: CONTAINING TARGET + PREDICTOR
# assemples data frame with return_custmoer and vector of predictions
# additional input: vector of predictions 
  
if(length(df.prediction)<2)
{
  stop("Please add at least one column with predictions to data frame df.prediction")
}

for(column in colnames(df.prediction))
{
  if(!column == "return_customer")
  {
    # initialize dataframes
    df.cutoffpoints = data.frame(cutoff_id = 1:100)
    df.final = data.frame(model_id = 1:100)
    # run optimal.cutpoints
    oc = optimal.cutpoints(
            X = prediction, ### ??? ### What is prediction? This variable is not defined anywhere, is it?
            status = "return_customer", 
            tag.healthy = 0, 
            methods = methods, ### ??? ### What is methods? This variable is not defined anywhere, is it?
            data = df.prediction[,"pred.lr"], ### !!! ### pred.lr exists only in your specific environment. Others might call their prediction differently.
            control = model.control.optc
            )
          
    ### !!! ### There seems to be an extra dot in df.cutoff.points (earlier it's called cutoffpoints)
    df.cutoff.points[1:length(oc$CB$Global$optimal.cutoff$cutoff),column] = data.frame(column = oc$CB$Global$optimal.cutoff$cutoff)
      
    for(cutoffvalue in df.cutoffpoints[1:length(oc$CB$Global$optimal.cutoff$cutoff),column])
    {
      avg_return = predictive_performance(y = df.prediction$return_customer, prediction = df.prediction[,column], cutoff = cutoffvalue)$avg_return
      print(column, avg_return)
    }
  }
}
  

