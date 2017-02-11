### SAVE Master

# this saves the master to csv (needs to be pushed!)
# validation
df_predictions_validation <- save_prediction_to_master(filename.csv = "predictions_validation.csv", master = df_predictions_validation)
df_performance_validation <- save_prediction_to_master(filename.csv = "performance_validation.csv", master = df_performance_validation)
# test
df_predictions_test <- save_prediction_to_master(filename.csv = "predictions_test.csv", master = df_predictions_test)
df_performance_test <- save_prediction_to_master(filename.csv = "performance_test.csv", master = df_performance_test)