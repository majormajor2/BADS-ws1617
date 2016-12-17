
## Using the exercises from class to see if any other data needs to be cleaned


if(!require("caret")) install.packages("caret"); library("caret")

idx.train <- createDataPartition(y = known$return_customer, p = 0.8, list = FALSE) 
# Draw a random, stratified sample including p percent of the data
train <- known[idx.train, ] # training set
test <-  known[-idx.train, ] # test set (drop all observations with train indeces)
idx.validation <- createDataPartition(y = train$return_customer, p = 0.25, list = FALSE) 
# Draw a random, stratified sample of ratio p of the data
validation <- train[idx.validation, ]
train60 <- train[-idx.validation, ]

# Develop models using the training set and compute test set predictions
dt      <-       rpart(return_customer ~ ., data = train60)
dt.full <-       rpart(return_customer ~ ., data = train60, cp = 0, minsplit = 3) # low minimum increase or number of observations in node for a split to be attempted
dt.prunedLess <- rpart(return_customer ~ ., data = train60, cp = 0.005) # create decision tree classifier
dt.prunedMore <- rpart(return_customer ~ ., data = train60, cp = 0.015) # create decision tree classifier
lr <-            glm(return_customer~.    , data = train60, family = binomial(link = "logit"))