if(!require("Information")) install.packages("Information"); library("Information")
library(klaR)
if(!require("pROC")) install.packages("pROC"); library("pROC") # load the package
if(!require("randomForest")) install.packages("randomForest")
source("main.R")
source("helper.R")

# Train models
xgb_parms_cphsv <- expand.grid(nrounds = c(800),
                           max_depth = 4,
                           eta = 0.01,
                           gamma = 0,
                           colsample_bytree = 1,
                           min_child_weight = 1,
                           subsample = 0.8)

xgb_mdl_cphsv <- train(return_customer~.,
                        data = known,
                        method = "xgbTree",
                        tuneGrid = xgb_parms_cphsv,
                        metric = "ROC",
                        trControl = model.control)

xgb_woe_parms_cphsv <- expand.grid(nrounds = 200,
                               max_depth = 4,
                               eta = 0.05,
                               gamma = 0,
                               colsample_bytree = 1,
                               min_child_weight = 1,
                               subsample = 0.8)

xgb_woe_mdl_cphsv <- train(return_customer~.,
                       data = known_woe,
                       method = "xgbTree",
                       tuneGrid = xgb_woe_parms_cphsv,
                       metric = "ROC",
                       trControl = model.control)

lr_woe_mdl_cphsv = glm(return_customer~., data = known_woe, family = binomial(link = "logit"))
rf_woe_mdl_cphsv = randomForest(return_customer~., data = known_woe, mtry = 8)



# predictions
xgb_pred_cphsv = predict(xgb_mdl_cphsv, newdata = class, type = "prob")[,2]
xgb_woe_pred_cphsv = predict(xgb_woe_mdl_cphsv, newdata = class, type = "prob")[,2]
lr_woe_pred_cphsv = predict(lr_woe_mdl_cphsv, newdata = class, type = "response")
rf_woe_pred_cphsv = predict(rf_woe_mdl_cphsv, newdata = class, type = "prob")[,2]


# create data frame with all the predictions
df_predictions_class <- data.frame(return_customer = known$return_customer, xgb_pred_cphsv, xgb_woe_pred_cphsv, lr_woe_pred_cphsv, rf_woe_pred_cphsv)

# save it to csv file
df_predictions_validation <- save_prediction_to_master(filename.csv = "predictions.csv", master = df_predictions_validation)
write.csv(x = master, file = filename.csv)

