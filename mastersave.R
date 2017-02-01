### SAVE Master

# this saves the master to csv (needs to be pushed!)
df_predictions_test <- save_prediction_to_master(filename.csv = "predictions_test.csv", master = df_predictions_test)
df_predictions_validation <- save_prediction_to_master(filename.csv = "predictions_validation.csv", master = df_predictions_validation)