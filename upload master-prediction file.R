# Upload Master files to github
source("helper.R")

# test data
# create data frame and add return_customer from test dataset
df_predictions_test <- data.frame(return_customer = test_data$return_customer)
# save it to csv file
df_predictions_test <- save_prediction_to_master(filename.csv = "predictions_test.csv", master = df_predictions_test)


