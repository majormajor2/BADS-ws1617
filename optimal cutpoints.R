### Optimal Cutpoints

if(!require("OptimalCutpoints")) install.packages("OptimalCutpoints"); library("OptimalCutpoints")
source("performance_measures.R")

###### --------- control.cutpoints ---------  ######

####### 1. MODEL.CONTROL

# COSTMATRIX
  # cost-benefit-matrix
  CBTN = +3
  CBFN = -10
  CBFP = 0
  CBTP = 0
  
  # cost-matrix
  CFN = CBFN - CB
  
  # METHODS
  methods = c("CB")
  
  # MODEL.CONTROL
  model.control.optc <- control.cutpoints(
    costs.ratio = (CBFP-CBTN)/(CBFN-CBTP)
    ) 
  

  ####### 2. MODEL 
  
  # DATA FRAME: CONTAINING TARGET + PREDICTOR
  # assemples data frame with return_custmoer and vector of predictions
  # additional input: vector of predictions 
  df.prediction = data.frame(return_customer,as.numeric(test_data_woe$return_customer)-1)
  df.cutoffpoints = data.frame(cutoff_id = 1:10)

  for(column in colnames(df.prediction)){
    if(!column == "return_customer")
      {
      if(length(df.prediction)<2)
        {
        print("Please add at least one column with predictions to data frame df.cutoffpoints.")
        break
      }
          oc <- optimal.cutpoints(
            X = "prediction", 
            status = "return_customer", 
            tag.healthy = 0, 
            methods = methods,
            data = df.prediction,
            control = model.control.optc
          )
      df.cutoff.points[1:length(oc$CB$Global$optimal.cutoff$cutoff),column] <- data.frame(column = oc$CB$Global$optimal.cutoff$cutoff)
      
      for(cutoffvalue in df.cutoffpoints[1:length(oc$CB$Global$optimal.cutoff$cutoff),column]){
        print(column)
        predictive_performance(y = df.prediction$return_customer, prediction = df.prediction[,column], cutoff = cutoffvalue)
      }
    }
  }
  

  
