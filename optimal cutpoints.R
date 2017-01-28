### Optimal Cutpoints

if(!require("OptimalCutpoints")) install.packages("OptimalCutpoints"); library("OptimalCutpoints")
source("performance_measures.R")


###### --------- control.cutpoints ---------  ######

###### --------- 1. RUN THIS ---------  ######

# FUNCTION FOR DATA FRAME
assemble_data_frame_opc <- function(prediction1 = NULL, prediction2 = NULL, prediction3 = NULL, prediction4 = NULL,  prediction5 = NULL)
{
  df.prediction = data.frame(
    return_customer=as.numeric(test_data_woe$return_customer)-1, 
    prediction1,
    prediction2, 
    prediction3, 
    prediction4, 
    prediction5)
  return(df.prediction)  
}

# FUNCTION FOR MODEL.CONTROL
specify_model_controls_opc <- function()
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
  model.control.optc <- control.cutpoints(
    costs.ratio = (CBFP-CBTN)/(CBFN-CBTP), CFP = -CFP, CFN = CFN
  ) 
}

###### --------- 2. ADD YOUR PREDICTIONS TO DATA FRAME ---------  ######
df.prediction <- assemble_data_frame_opc(
  prediction1 = NULL, 
  prediction2 = NULL, 
  prediction3 = NULL, 
  prediction4 = NULL,  
  prediction5 = NULL
)



####### 1. MODEL.CONTROL


  

  ####### 2. MODEL 
  
  # DATA FRAME: CONTAINING TARGET + PREDICTOR
  # assemples data frame with return_custmoer and vector of predictions
  # additional input: vector of predictions 
  


  for(column in colnames(df.prediction)){
    if(!column == "return_customer")
      {
      if(length(df.prediction)<2)
        {
        print("Please add at least one column with predictions to data frame df.cutoffpoints.")
        break
      }
      # initialize dataframes
          df.cutoffpoints = data.frame(cutoff_id = 1:100)
          df.final = data.frame(model_id = 1:100)
      # run optimal.cutpoints
          oc <- optimal.cutpoints(
            X = prediction, 
            status = "return_customer", 
            tag.healthy = 0, 
            methods = methods,
            data = df.prediction[,"pred.lr"],
            control = model.control.optc
          )
      df.cutoff.points[1:length(oc$CB$Global$optimal.cutoff$cutoff),column] <- data.frame(column = oc$CB$Global$optimal.cutoff$cutoff)
      
      for(cutoffvalue in df.cutoffpoints[1:length(oc$CB$Global$optimal.cutoff$cutoff),column]){
        avg_return <- predictive_performance(y = df.prediction$return_customer, prediction = df.prediction[,column], cutoff = cutoffvalue)$avg_return
        print(column, avg_return)
      }
    }
  }
  

