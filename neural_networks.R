source("main.R")

##### Convert all to numeric #####
train60_normalized = train60_data_woe # copy dataset
for(column in colnames(train60_normalized)){ # loop over all columns
  if(column != "return_customer"){
    train60_normalized[,column] = sapply(train60_normalized[,column],as.numeric)}} # convert to numeric

##### Truncate outliers #####
train60_normalized = treat_outliers(train60_normalized)   
##### Normalize to -1;1 #####
train60_normalized = normalize_dataset(train60_normalized, c("return_customer"))
##### Drop Correlated Variables #####
dropped_correlated_variables = strongly_correlated(train60_normalized, threshold = 0.6)
train60_normalized[dropped_correlated_variables] = NULL
##### Plot Correlation Matrix #####
correlation_matrix = cor(train60_normalized[, sapply(train60_normalized, is.numeric)])
corrplot(correlation_matrix, title = "Correlation Matrix", type = "full", order = "AOE", tl.cex = 0.5, tl.srt = 45, mar = c(1,0,1,1))
