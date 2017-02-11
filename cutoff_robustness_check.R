## MASTER OF MASTER PERFORMANCE

# use cutoff from "validation" on test_data and add it to df_performance_test

robustnesscheck_for_cutoffs <- function(dataframe_perf_validation, dataframe_pred_test, dataframe_perf_test){
  
  # check if columns exist in bot dataframe_pred_test and dataframe_perf_test
  # get column names in dataframe_perf_test
  predictions <- colnames(dataframe_perf_test)[-1]
  for (pred in predictions){
    if(!pred %in% colnames(dataframe_pred_test)[-1]){
      stop(paste("Please include",pred,"in", dataframe_pred_test))
      }
    }
  
  # check which predicitons are present in dataframe_perf_validaiotn
  idx <- predictions %in% colnames(dataframe_perf_validation)[-1]  
  predictions <- predictions[idx]
 

  # use cutoff points from validation and do performance measures with the predictions from test
  for(pred in predictions){
    # get cutoffpoint from performance measures from predictions on validation
    cutoffpoint <- dataframe_perf_validation[dataframe_perf_validation[,"metrics"] == "cutoff", pred]
    # use that cutpoint on the predictions from test to get performance measures 
    predictive.performance <- predictive_performance(y = dataframe_pred_test$return_customer, prediction = dataframe_pred_test[,pred], cutoff = cutoffpoint, returnH = FALSE)
    # save it to dataframe_perf_test
    dataframe_perf_test[dataframe_perf_test[,"metrics"] == "cutoff_valid", pred] <- cutoffpoint
    dataframe_perf_test[dataframe_perf_test[,"metrics"] == "avg_return_valid", pred] <- predictive.performance$avg_return
    } 
  return(dataframe_perf_test)
}