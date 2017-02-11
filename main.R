# This is the main script that runs all the other modules

####### Load helper ################

source("helper.R")

####### Set seed ################

set.seed(666)

####### Load data ################
# known - training data
known = get_dataset("assignment_BADS_WS1617_known.csv")
# class - data to be classified
class = get_dataset("assignment_BADS_WS1617_class.csv")

######### Preprocess ##############

known = treat_NAs(known)
class = treat_NAs(class)
known = treat_postcodes(known)
class = treat_postcodes(class)
known = treat_dates(known)
class = treat_dates(class)
known = treat_weight(known)
class = treat_weight(class)

##### Create Master Dataset to store results #####

predictions_all = data.frame(return_customer = known$return_customer)

####### Check plausability of data types ################

# lapply(known,class)
# lapply(class,class)

# Summarise
#lapply(known,summary)
#lapply(class,summary)

###### Plotting ######

par(mar=c(1,1,1,1)) # to make sure the plot works on a small screen

######### Partition the data ##############

# Split data set into 80% training and 20% test data
# Draw a random stratified sample in which both train and test set have roughly the same ratio of the target classes.
# The function creatDataPartition returns the indices of a stratified training set with size p * size of data.

# Draw a random, stratified sample including p percent of the data
set.seed(666)
idx_train  = createDataPartition(y = known$return_customer, p = 0.8, list = FALSE) 
train_data = known[idx_train, ] # training set
test_data  =  known[-idx_train, ] # test set (drop all observations with train indices)

###### Calculate Weights of Evidence #########
# Will create a new dataframe consisting of all the variables of known but replaces the factor
# variables into numerical variables according to the weight of evidence
columns_to_replace = c("form_of_address", "email_domain", "model", "payment", "postcode_invoice", "postcode_delivery", "advertising_code")
# Calculate WoE from train_fold and return woe object
woe_object = calculate_woe(known, target = "return_customer", columns_to_replace = columns_to_replace)
# Replace multilevel factor columns by their WoE
known_woe = apply_woe(dataset = known, woe_object = woe_object)
class_woe = apply_woe(dataset = class, woe_object = woe_object)

###### Normalize datasets ######

# Identify variables that are highly correlated and drop them from dataset
dropped_correlated_variables = strongly_correlated(known_woe, threshold = 0.6)

# Perform normalization operations
known_norm = prepare(known_woe, dropped_correlated_variables)
class_norm = prepare(class_woe, dropped_correlated_variables)



##### Nested Cross Validation #####

# Run Neural Network

###### Meta Model ######

# Get a data frame of predictions for all of known
# Have a cross validation of meta models over this dataframe
source("preparation_metamodel.R")

# Initialise highest return for the selection of meta model
highest_return = 0

# Loop over the models from the cross validation
for(model in meta_models)
{
  # Check if the highest return is better than that of the other sets of hyperparameters
  # i.e. if it generalizes better than the others
  if(model$avg_return > highest_return)
  {
    # Pick best set of hyperparameters
    meta_hyperparameters = data.frame(model$model$bestTune)
    highest_return = model$avg_return
  }
}

meta_model = train(return_customer~., data = known_predictions, method = "xgbTree", tuneGrid = meta_hyperparameters, metric = "avg_return", trControl = model_control)


######## Cost matrix ##########
cost.matrix = build_cost_matrix(CBTN = 3, CBFP = -10)


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