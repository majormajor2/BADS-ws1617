### REGRESSIONS - LOGISTIC REGRESSION

source("helper.R")
source("main.R")



### logistic regression
lr <- glm(return_customer~., data = train_data, family = binomial(link = "logit")) # all variables go into regression (including categorical etc., but no missing values)
summary(lr)
coef(summary(lr)) # grab coefs
# prediction
pred.lr <- predict(lr, newdata = test_data, type = "response")
# predictive performance
predictive_performance(y = known$return_customer, prediction = pred.lr, cutoff = 0.5)


### reduced form logit
# only use significant variables form before
lr.reduced <- glm(return_customer ~ [significant_variables], data = train, family = binomial(link = "logit"))
pred.lr.reduced <- predict(lr.reduced, newdata=train, type="response")
brier.lr.reduced <- sum((y- pred.lr.reduced)^2) / length(y)
sprintf("Reduced logistic regression model has a Brier Score of %.5f (c.f. %.5f of the model with all variabels included)", brier.lr.reduced, brier.lr)


### decision trees
# partition data into smaller, more homogeneous groups
# used for classification and regression
# based on some measure of homogeneity e.g.Gini index/impurity 
# (in the two class case $p_1 (1 - p_1) + p_2 (1 - p_2)$) or overall sums of squared errors (SSE)

# decision tree
dt <- rpart(return_customer ~., data = train, cp = 0.0008) # max cp (4 digits) that will produce a split
pred.dt <- predict(dt, newdata = train, type = "prob")[, 2] # calculate predictions (in-sample)
head(pred.dt)
summary(pred.dt)
# plot
prp(dt)
prp(dt, extra = 104, border.col = 0) # Print the percentage of observations and class probabilities in each node
# predictive performance
predictive_performance(y = train$return_customer, prediction = pred.dt, cutoff = 0.5)

# dt w/o pruning
dt <- rpart(return_customer ~., data = train, cp = 0.0, minsplit = 3)

