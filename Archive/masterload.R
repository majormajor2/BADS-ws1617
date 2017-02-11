#### LOAD Master

# loads current version of master file from github
# validation
df_predictions_validation <- call_master(filename.csv = "predictions_validation.csv")
df_performance_validation <- call_master(filename.csv = "performance_validation.csv")
# test
df_predictions_test <- call_master(filename.csv = "predictions_test.csv")
df_performance_test <- call_master(filename.csv = "performance_test.csv")

