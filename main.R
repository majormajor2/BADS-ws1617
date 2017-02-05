# This is the main script that runs all the other modules

####### Load modules ################

source("helper.R")
source("weight.R")
source("woe.R")
source("performance_measures.R")
source("binning.R")
source("controlcutoffs_fortraincontrol.R")
source("predictclass.R")

####### Set seed ################

set.seed(666)

####### Load data ################
# known - training data
known = get_dataset("assignment_BADS_WS1617_known.csv")
# class - data to be classified
class = get_dataset("assignment_BADS_WS1617_class.csv")

######### Treat missing variables ##############

known = treat_missing_values(known)
class = treat_missing_values(class)
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
# Replace multilevel factor columns in train_fold by their WoE
known_woe = apply_woe(dataset = known, woe_object = woe_object)
# Apply WoE to all the other folds 
#validation_fold_woe = apply_woe(dataset = validation_fold, woe_object = woe_object)
class_woe = apply_woe(dataset = class, woe_object = woe_object)

###### Normalize datasets ######

dropped_correlated_variables = strongly_correlated(known_woe, threshold = 0.6)

print("Perform normalization operations.")
known_norm = prepare(known_woe, dropped_correlated_variables)
#validation_fold_woe = prepare(validation_fold_woe, dropped_correlated_variables)
class_norm = prepare(class_woe, dropped_correlated_variables)


######## Cost matrix ##########
cost.matrix = build_cost_matrix(CBTN = 3, CBFP = -10)


####### Call Master File ######
df_predictions_test <- call_master(filename.csv = "predictions_test.csv")


#### Plotting ####
par(mar=c(1,1,1,1)) # to make sure the plot works on a small screen


##### Predict classification dataset #####
predict_class()
