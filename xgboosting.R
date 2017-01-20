## Load packages and set up the basic cleaned dataset

source("main.R")

## Partition data, excluding columns with NA

# which columns have NAs
colnames(train_data)[colSums(is.na(train_data)) > 0]


# Draw a random, stratified sample including p percent of the data, exclude columns with NAs
idx.train <- createDataPartition(y = known$return_customer, p = 0.8, list = FALSE) 
train <- known[idx.train, -which(names(known) %in% c("form_of_address","advertising_code","deliverydate_estimated","deliverydate_actual"))] # training set
test <-  known[-idx.train, -which(names(known) %in% c("form_of_address","advertising_code","deliverydate_estimated","deliverydate_actual"))] # test set

# Run the xtreme boosting model with PCA
set.seed(27)


model_xgb_pca <-train(return_customer ~.,
                        data=train_data,
                        method="xgbTree",
                        preProcess = "pca",
                        na.action = na.pass,
                        trControl = trainControl(method = "repeatedcv", number = 5, repeats = 10, verboseIter = FALSE))

confusionMatrix(predict(model_xgb_pca, test))

summary(predict(model_xgb_pca, test))
summary(test$return_customer)

