call_master <- function(filename.csv = "predictions_test.csv"){
  # download current version of masterfile
  master = read.csv(filename.csv, header=T, sep=",", row.names = 1)
  return(master)
}
save_prediction_to_master <- function(filename.csv = "predictions_test.csv", master = df_predictions_test){
  # save as csv
  write.csv(x = master, file = filename.csv)
  return(master)
}