#### LOAD Master

# loads current version of master file from github
df_predictions_test <- call_master(filename.csv = "predictions_test.csv")
df_predictions_validation <- call_master(filename.csv = "predictions_validation.csv")
