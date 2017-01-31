# Set up parallel computing - look at Exercise 7 for more details
if(!require("doParallel")) install.packages("doParallel"); library("doParallel") # load the package
if(!require("microbenchmark")) install.packages("microbenchmark"); library("microbenchmark") # load the package

# Setup up parallel backend
# Detect number of available clusters, which gives you the maximum number of "workers" your computer has
no_of_cores = detectCores()
cl = makeCluster(max(1,no_of_cores-1))
registerDoParallel(cl)
message(paste("\n Registered number of cores:\n",getDoParWorkers(),"\n"))


## Parallelization with foreach
# This is the parallel version of the nested loop model selection.
# Note that each of the workers is a fresh R instance without any variables or loaded packages
# Foreach exports variables that occur in the code automatically, but we need to specify
# the packages that need to be loaded on each worker
# Loop over different number of nodes, i.e. size of the neural network
results <- foreach(n = 1:5, .combine = cbind, .packages = c("caret", "nnet", "pROC")) %:%
    # This is the cross-validation loop from before
    foreach(i = 1:5, .combine = c, .packages = c("caret","nnet", "pROC")) %dopar%{
      # Split data into training and validation
      idx_val <- which(folds == i, arr.ind = TRUE)
      cv_train <- train.rnd[-idx_val,]
      cv_val <- train.rnd[idx_val,]
      # Train the neural network model with a number of nodes n
      nn <- nnet(return_customer~., data = train_data, trace = FALSE, maxit = 1000, size = n)
      # Build and evaluate models using these partitions
      yhat <- predict(nn, newdata = cv_val, type = "raw")
      # We use our above function to calculate the classification error
      auc(cv_val$return_customer, as.vector(yhat))
    }


# Don't forget to stop the cluster when you don't need it anymore!
stopCluster(cl)