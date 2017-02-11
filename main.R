# This is the main script that runs all the other modules

####### Load helper ################

source("helper.R")

####### Set seed ################

set.seed(666)

###### Set plotting margins ######

par(mar=c(1,1,1,1)) # to make sure the plot works on a small screen

####### Load data ################
# known - training data
known = get_dataset("assignment_BADS_WS1617_known.csv")
# class - data to be classified
class = get_dataset("assignment_BADS_WS1617_class.csv")

##### Check plausability of data types #####

# lapply(known,class)
# lapply(class,class)

# Summarise
#lapply(known,summary)
#lapply(class,summary)

######### Clean Data ##############

known = treat_NAs(known)
class = treat_NAs(class)
known = treat_postcodes(known)
class = treat_postcodes(class)
known = treat_dates(known)
class = treat_dates(class)
known = treat_weight(known)
class = treat_weight(class)

###### Calculate Weights of Evidence #########

# Will create a new dataframe consisting of all the variables of known but replaces the 
# factor variables into numerical variables according to the weight of evidence
columns_to_replace = c("form_of_address", "email_domain", "model", "payment", "postcode_invoice", "postcode_delivery", "advertising_code")
# Calculate WoE from known data and return woe object
woe_object = calculate_woe(known, target = "return_customer", columns_to_replace = columns_to_replace)
# Replace multilevel factor columns by their WoE
known_woe = apply_woe(dataset = known, woe_object = woe_object)
class_woe = apply_woe(dataset = class, woe_object = woe_object)

###### Normalize Data ######

# Identify variables that are highly correlated and drop them from dataset
dropped_correlated_variables = strongly_correlated(known_woe, threshold = 0.6)

# Perform normalization operations
known_norm = prepare(known_woe, dropped_correlated_variables)
class_norm = prepare(class_woe, dropped_correlated_variables)

##### Create data frame to store predictions #####

predictions_all = data.frame(return_customer = known$return_customer)

###### Create Cost matrix ######

cost.matrix = build_cost_matrix(CBTN = 3, CBFP = -10)


######### Partition the data ##############

# Split data set into 80% training and 20% test data
# Draw a random stratified sample in which both train and test set have roughly the same ratio of the target classes.
# The function creatDataPartition returns the indices of a stratified training set with size p * size of data.

# Draw a random, stratified sample including p percent of the data
set.seed(666)
idx_train  = createDataPartition(y = known$return_customer, p = 0.8, list = FALSE) 
train_data = known[idx_train, ] # training set
test_data  =  known[-idx_train, ] # test set (drop all observations with train indices)

##### Slice data set into folds #####
# Set number of folds
k = 5
# Set seed for reproducability
set.seed(123)
# Create folds for cross validation (these are the big folds 4/5 of total) - not used in function at the moment
training_folds = createFolds(train_data$return_customer, k = k, list = TRUE, returnTrain = TRUE)
# Set seed for reproducability
set.seed(123)
# Define fold membership for cross validation
fold_membership = createFolds(train_data$return_customer, list = FALSE, k = k)

###### Initialise model control ######
model_control = trainControl(
  method = "cv", # 'cv' for cross validation, 'adaptive_cv' drops unpromising models
  number = 5, # number of folds in cross validation (or number of resampling iterations)
  #repeats = 5, # number of repeats for repeated cross validation
  search = "grid", # or grid for a grid search
  classProbs = TRUE,
  summaryFunction = twoClassSummary,
  #timingSamps = length(fold), # number of samples to predict the time taken
  sampling = "smote", # This resolves class imbalances. 
  # Possible values are "none", "down", "up", "smote", or "rose". The latter two values require the DMwR and ROSE packages, respectively.
  allowParallel = TRUE, # Enable parallelization if available
  savePredictions = TRUE, # Save the hold-out predictions
  verboseIter = TRUE, # Print training log
  returnData = FALSE) # The training data will not be included in the output training object


##### Perform Nested Cross Validation #####

logistic_output = run_logistic(known, fold_membership, model_control, big_server = TRUE, dropped_correlated_variables)$all
random_forest_output = run_random_forest(known, fold_membership, model_control, big_server = TRUE, dropped_correlated_variables)$all
neuralnet_output = run_neural_network(known, fold_membership, model_control, big_server = TRUE, dropped_correlated_variables)$all
xgb_output = run_xgboosting(known, fold_membership, model_control, big_server = TRUE, dropped_correlated_variables)$all
xgb_woe_output = run_xgboosting_woe(known, fold_membership, model_control, big_server = TRUE, dropped_correlated_variables)$all

###### Create Meta Model ######

# Get a data frame of predictions for all of known
# Have a cross validation of meta models over this dataframe
source("preparation_metamodel.R")
# Train meta model on entire known dataset with best set of hyperparameters
meta_model = train(return_customer~., data = known_predictions, method = "xgbTree", tuneGrid = meta_hyperparameters, metric = "avg_return", trControl = model_control)



####### Call Master File ######
df_predictions_test = call_master(filename.csv = "predictions_test.csv")





##### Predict classification dataset #####
# get predictions from primary models
class_predictions = predict_class()
# use primary predictions to predict with meta model
final_prediction =   predict(meta_model, newdata = class_predictions, type = "prob")[,2]
# use optimal cutoff previously found and save to data frame
final_output = data.frame(ID = row.names(class), return_customer = ifelse(final_prediction <= optimal_cutoff_for_class, 0, 1))
# save csv-file with predictions
write.csv(final_output, file = "27.csv", row.names = FALSE)