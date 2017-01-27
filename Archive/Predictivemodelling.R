
#1. Load dataset using the helper function
#2. Use the data cleaning (dates) function 
#3. Partition data into train, test and validation data sets as we did in the tutorial
#4. Try to develop the models used in TUT 4 onwards, starting with Logistic, 
    #dt and then neural networks
#5. Model evaluation (looking only at DT and LR)
#6. Try K-folds validtion - TUT 6
#7. Neural Networks



#1.  Load dataset using the helper function

## clear workspace, if needed
rm(list = ls())

source("helper.R")
known <- get_dataset("assignment_BADS_WS1617_known.csv")


#2. Use the data cleaning (dates) function 
DatacleaningDates <- function(x) {
  
  ## Load packages that are needed 
  if(!require("lubridate")) install.packages("lubridate"); library("lubridate")
  if(!require("caret")) install.packages("caret"); library("caret")
  if(!require("rpart")) install.packages("rpart"); library("rpart")
  if(!require("hmeasure")) install.packages("hmeasure"); library("hmeasure")
  
  ## Order date does not require cleaning, there are no missing values or outliers
  ## Account creation date: Create a dummy variable for NAs
  
  known$account_creation_date_missing <- ifelse(is.na(known$account_creation_date), 1, 0)
  
  ## Delivery date estimated has outliers, from 2010 and 4746. Create a dummy to capture both
  
  known$deliverydate_estimated_outliers <- ifelse(year(known$deliverydate_estimated) == 2010 | year(known$deliverydate_estimated) == 4746, 1, 0)
  
  ## Change 2010 to 2013, 4746 to 2014
  year(known$deliverydate_estimated[year(known$deliverydate_estimated) == 2010])
  known$deliverydate_estimated  <- as.Date(known$deliverydate_estimated)
  is.Date(known$deliverydate_estimated)
  
  year(known$deliverydate_estimated[year(known$deliverydate_estimated) == 2010]) <- year(known$deliverydate_estimated[year(known$deliverydate_estimated) == 2010]) + 4
  year(known$deliverydate_estimated[year(known$deliverydate_estimated) == 4746]) <- year(known$deliverydate_estimated[year(known$deliverydate_estimated) == 4746]) - 2733
  
  
  ## Delivery date acutal has 0000/00/00, create a dummy wth the missing value
  known$deliverydate_actual <- as.Date(known$deliverydate_actual)
  is.Date(known$deliverydate_actual)
  known$deliverydate_actual_missing <- ifelse(is.na(known$deliverydate_actual), 1, 0)
  
  ## and adjust existing values to match delivery date estimated 
  
  ## index the na's for delivery_date_actual
  na.index <- which(is.na(known$deliverydate_actual))
  
  ##replace them with estimated delivery date
  known$deliverydate_actual[is.na(known$deliverydate_actual)] <- known$deliverydate_estimated[na.index]
  
  
  return(head(known$account_creation_date_missing))
  
}

#3. Partition data into train, test and validation data sets as we did in the tutorial

known$return_customer <- as.factor(known$return_customer)

idx.train <- createDataPartition(y = known$return_customer, p = 0.8, list = FALSE) 
  # Draw a random, stratified sample including p percent of the data
train <- known[idx.train, grep("postcode_delivery", invert = TRUE, colnames(known))] # training set
test <-  known[-idx.train, grep("postcode_delivery", invert = TRUE, colnames(known))] # test set (drop all observations with train indeces)
idx.validation <- createDataPartition(y = train$return_customer, p = 0.25, list = FALSE) 
  # Draw a random, stratified sample of ratio p of the data
validation <- train[idx.validation, ]
train60 <- train[-idx.validation, ]


is.numeric(known$return_customer)
is.factor(known$return_customer)
known$return_customer <- as.factor(known$return_customer)

#4. Try to develop the models used in TUT 4 onwards, 
  
  # starting with Logistic, dt and then neural networks
  
lr<-glm(return_customer ~., data = train60, family = binomial(link = "logit"))
  # LR didn't work when tried on the whole dataset, "Error: cannot allocate vector of size 4.1 Gb"

  # Developed only two models with reduced dataset, testing models covered in tutorial

dt<-rpart(return_customer ~ goods_value + item_count + order_date + account_creation_date_missing + deliverydate_actual_missing, data = train60, method = "class")
lr<-glm(return_customer ~ goods_value + item_count + order_date + account_creation_date_missing + deliverydate_actual_missing, data = train60, family = binomial(link = "logit"))


  #Helpful functions in reading rpart output
printcp(dt)
plotcp(dt)
summary(dt)


  #creating estimates for the two models + benchmart, creating a list with all of them
yhat.lr <- predict(lr, newdata = validation, type = "response")
yhat.dt <- predict(dt, newdata = validation, type = "prob")[,2]
summary(yhat.dt)
summary(yhat.lr)
#dt estimates - all seem to be the same value (the mean)
yhat.benchmark <- rep(sum(as.numeric(train60$return_customer)-1 == 1)/nrow(train60), nrow(validation))
summary(yhat.benchmark)
yhat.validation <- c(list("dt" = yhat.dt, "benchmark" = yhat.benchmark, "lr" = yhat.lr))


#5. Model evaluation (looking only at DT and LR)

  #convert return_customer to numeric and subtract 1 
y.validation <- as.numeric(validation$return_customer)-1

BrierScore <- function(y, yhat){
  sum((y - yhat)^2) / length(y) 
}
  # Apply the brierScore function to each element of y.validation, i.e. all the prediction vectors
brier.validation <- sapply(yhat.validation, BrierScore, y = y.validation, USE.NAMES = TRUE)
print(brier.validation)

tau <- 0.25
  #Deal with logistic regression:
  #convert probability prediction to discrete class predictions

yhat.dt.class <- ifelse(yhat.validation$dt > tau,1,0)
yhat.lr.class <- ifelse(yhat.validation$lr > tau,1,0)

  #We can create a simple confusion table with base function table.
  #Using DT doesn't make sense since all values are the mean - need to figure out why
  #Using, for example, the logit classifier, this equates to:
table(yhat.lr.class, validation$return_customer)
summary(yhat.lr)

  #Creating a confusion matrix for the logistic regression predictions
validation$return_customer <- as.numeric(validation$return_customer)-1
confusionMatrix(data = yhat.lr.class, reference = validation$return_customer, positive = "1")

  #area under the curve
predictions.roc <- data.frame(LR = yhat.validation$lr, DT = yhat.validation$dt) 

h <- HMeasure(y.validation, predictions.roc)

  
plotROC(h, which = 1)
h$metrics["AUC"]

#6. Try K-folds validtion - TUT 6

  #shuffle rows before trying k-folds

train.rnd <- train[sample(nrow(train)),]
  # Create k folds of approximately equal size
k <- 5
folds <- cut(1:nrow(train.rnd), breaks = k, labels = FALSE)

results <- data.frame(lr = numeric(length = k), dt = numeric(length = k))

for (i in 1:k) {
  # Split data into training and validation
  idx.val <- which(folds == i, arr.ind = TRUE)
  cv.train <- train.rnd[-idx.val,]
  cv.val <- train.rnd[idx.val,]
  # Build and evaluate models using these partitions
  lr <- glm(return_customer ~ goods_value + item_count + account_creation_date_missing + deliverydate_actual_missing, data = cv.train, family = binomial(link = "logit"))
  dt <- rpart(return_customer ~ goods_value + item_count + account_creation_date_missing + deliverydate_actual_missing, data = cv.train, cp = 0.015) # create decision tree classifier
  # LR doesn't seem to work when selecting all columns
  
  yhat.lr <- predict(lr, newdata = cv.val, type = "response")
  yhat.dt <- predict(dt, newdata = cv.val, type = "prob")[,2]
  # We use our above function to calculate the classification error
  results[i, "lr"] <- BrierScore(as.numeric(cv.val$return_customer )-1, yhat.lr)
  results[i, "dt"] <- BrierScore(as.numeric(cv.val$return_customer)-1, yhat.dt)
}

cv.perf <- apply(results, 2, mean)
cv.perf.sd <- apply(results, 2, sd)
  # Now plot the results
txt <- paste("Classification brier score across", as.character(k), "folds", sep=" ")
boxplot(results,  ylab="Brier Score", 
        main = txt)

  #testing different models with k-folds validation
y.test <- as.numeric(test$return_customer)-1 # This is a good example of why you need to be careful when transforming factor variables to numeric
yhat.test.lr <- predict(lr, newdata = test, type = "response")
yhat.test.dt.prunedMore <- predict(dt, newdata = test, type = "prob")[,2]
brier.test <- sapply(list("lr" = yhat.test.lr, "dt" = yhat.test.dt.prunedMore), BrierScore, y = y.test, USE.NAMES = TRUE)
print(brier.test)

#7. Neural Network
if(!require("nnet")) install.packages("nnet"); library("nnet") # Try to load rpart, if it doesn't exist, then install and load it
if(!require("pROC")) install.packages("pROC"); library("pROC") # Try to load rpart, if it doesn't exist, then install and load it
if(!require("caret")) install.packages("caret"); library("caret") 

  ### Specify the setup:
  # The number of nodes to try for the model
nnet.sizes <- seq(from = 3, to = 15, by = 3)
  # Initialize the data frame that collects the results
results <- as.data.frame(matrix(NA, ncol = length(nnet.sizes), nrow = k))
for(n in 1:length(nnet.sizes)){
  # This is the cross-validation loop from before
  for (i in 1:k) {
    # Split data into training and validation
    idx.val <- which(folds == i, arr.ind = TRUE)
    cv.train <- train.rnd[-idx.val,]
    cv.val <- train.rnd[idx.val,]
    # Train the neural network model with a number of nodes n
    #!!!model doesn't run on the entire data set :(
    neuralnet <- nnet(return_customer~order_date + form_of_address + title + newsletter + model + goods_value + item_count + canceled_items, data = cv.train, # the data and formula to be used
                      trace = FALSE, maxit = 1000, # general options
                      size = nnet.sizes[n]) # the number of nodes in the model
    # Build and evaluate models using these partitions
    yhat <- predict(neuralnet, newdata = cv.val, type = "raw")
    # Calculate the AUC value with a function from package pROC
    results[i, n] <- auc(cv.val$return_customer, as.vector(yhat))
  }
}

colnames(results) <- nnet.sizes 
boxplot(results, xlab = "Number of nodes in hidden layer", ylab = "Area under the ROC curve (AUC)")

model.control<- trainControl(
  method = "cv", # 'cv' for cross validation
  number = 5, # number of folds in cross validation
  #repeats = 3, # number for repeated cross validation
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  allowParallel = TRUE, # Enable parallelization if available
  returnData = FALSE # The training data will not be included in the ouput training object
)

train$return_customer <- factor(train$return_customer, labels = c("yes", "no"))

nn.parms <- expand.grid(decay = c(0, 10^seq(-3, 0, 1)), size = seq(3,15,2))

nn <- train(return_customer~order_date + newsletter + model + goods_value + item_count + canceled_items, data = train,  
            method = "nnet", maxit = 200, trace = FALSE, # options for nnet function
            tuneGrid = nn.parms, # parameters to be tested
            metric = "ROC", trControl = model.control)

print(nn)
plot(nn)
summary(nn)


yhat.nn   <- predict(nn, newdata = test, type = "prob")[,2]
nn.roc <-roc(test$return_customer, yhat.nn)
auc(nn.roc)
plot.roc(nn.roc)

h <- HMeasure(true.class = as.numeric(test$return_customer)-1, scores = yhat.nn)
# Note that AUC is the same; as it should be
h$metrics["AUC"]
plotROC(h, which=1)

known$return_customer <- as.numeric(known$return_customer) -1

