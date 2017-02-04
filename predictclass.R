if(!require("klaR")) install.packages("klaR")
library(klaR)
if(!require("pROC")) install.packages("pROC"); library("pROC") # load the package
if(!require("randomForest")) install.packages("randomForest")
source("main.R")



# predict woe on known



# Train models
xgb_mdl_cphsv = xgb.train (params = list(max_depth = 4, eta = 0.01, gamma = 0, colsample_bytree = 1, min_child_weight = 1, subsample = 0.8), data = known, nrounds = 800)

# predictions
xgb_pred_cphsv = predict(predict(xgb_mdl_cphsv, newdata = class, type = "prob")[,2])