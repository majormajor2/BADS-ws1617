# Upload Master files to github
source("helper.R")
#### PREDICTIONS 

# validation data
# create data frame and add return_customer from validation dataset
df_predictions_validation <- data.frame(return_customer = validation_data$return_customer)
# save it to csv file
df_predictions_validation <- save_prediction_to_master(filename.csv = "predictions_validation.csv", master = df_predictions_validation)

# test data
# create data frame and add return_customer from test dataset
df_predictions_test <- data.frame(return_customer = test_data$return_customer)
# save it to csv file
df_predictions_test <- save_prediction_to_master(filename.csv = "predictions_test.csv", master = df_predictions_test)

##### PREDICTIVE PERFORMANCE

# predictive performance on validation (model selection for meta model)
df_performance_validation <- data.frame(metrics = c("AUC", "hmeasure", "cutoff", "avg_return"))

# predictive performance on test (final model selection)
df_performance_test <- data.frame(metrics = c("AUC", "hmeasure", "cutoff", "avg_return"))





