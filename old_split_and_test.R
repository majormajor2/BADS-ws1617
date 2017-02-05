####### OLD SPLIT AND TEST + OLD WOE ########

set.seed(999) # just making sure ;)
idx_validation = createDataPartition(y = train_data$return_customer, p = 0.25, list = FALSE)
train60_data = train_data[-idx_validation, ] # this is the smaller 60% dataset for training before validation
validation_data = train_data[idx_validation, ] # Validation is for testing the models before the meta model is run

###### Nested Cross Validation ######

###### Weight of Evidence ######
# Will create a new dataframe consisting of all the variables of known but replaces the factor
# variables into numerical variables according to the weight of evidence
columns_to_replace = c("form_of_address", "email_domain", "model", "payment", "postcode_invoice", "postcode_delivery", "advertising_code")
# Calculate WoE from train_data and return woe object
woe_object = calculate_woe(train60_data, target = "return_customer", columns_to_replace = columns_to_replace)
# train 80
woe_object_train = calculate_woe(train_data, target = "return_customer", columns_to_replace = columns_to_replace)
# Replace multilevel factor columns in train_data by their woe
train60_data_woe = apply_woe(dataset = train60_data, woe_object = woe_object)
train_data_woe = apply_woe(dataset = train_data, woe_object = woe_object_train)

# Apply woe to validation (input any dataset where levels are identical to trained woe_object)
validation_data_woe = apply_woe(dataset = validation_data, woe_object = woe_object)
# Apply woe to test (input any dataset where levels are identical to trained woe_object)
test_data_woe = apply_woe(dataset = test_data, woe_object = woe_object_train)

# Calculate WoE for known data set
woe_object_known = calculate_woe(known, target = "return_customer", columns_to_replace = columns_to_replace)
known_woe = apply_woe(dataset = known, woe_object = woe_object_known)

# Apply woe to class (input any dataset where new levels emerge compared to training datset)
class_woe = apply_woe(dataset = class, woe_object = woe_object_known)

##### BINNING #######

# creates bins for columns "form_of_address", "email_domain", "model", "payment", "postcode_invoice", "postcode_delivery", "advertising_code"
# applies woe to binned columns

# 1 CALCULATE WOE-OBJECT 
# 1.1 create bins for train-dataset
# train_data_bins
train60_data_bins_ew = create_bins(train60_data_woe, NO_BINS = 5, DO_EQUAL_WIDTH = TRUE, run_woe = FALSE)
train_data_bins_ew = create_bins(train_data_woe, NO_BINS = 5, DO_EQUAL_WIDTH = TRUE, run_woe = FALSE)
train60_data_bins_ef = create_bins(train60_data_woe, NO_BINS = 5, DO_EQUAL_WIDTH = FALSE, run_woe = FALSE)
train_data_bins_ef = create_bins(train_data_woe, NO_BINS = 5, DO_EQUAL_WIDTH = FALSE, run_woe = FALSE)

# 1.2 calculate woe form binned train-datasets
woe_object_ew = calculate_woe(train60_data_bins_ew, columns = c("email_domain", "postcode_invoice", "postcode_delivery", "advertising_code"))
woe_object_ew_train80 = calculate_woe(train_data_bins_ew, columns = c("email_domain", "postcode_invoice", "postcode_delivery", "advertising_code"))
woe_object_ef = calculate_woe(train60_data_bins_ef, columns = c("email_domain", "postcode_invoice", "postcode_delivery", "advertising_code"))
woe_object_ef_train80 = calculate_woe(train_data_bins_ef, columns = c("email_domain", "postcode_invoice", "postcode_delivery", "advertising_code"))

# train_data_woe
train60_data_woe_ew = create_bins(train60_data_woe, woe_object_ew, NO_BINS = 5, DO_EQUAL_WIDTH = TRUE, run_woe = TRUE)
train_data_woe_ew = create_bins(train_data_woe, woe_object_ew_train80, NO_BINS = 5, DO_EQUAL_WIDTH = TRUE, run_woe = TRUE)

train60_data_woe_ef = create_bins(train60_data_woe, woe_object_ef, NO_BINS = 5, DO_EQUAL_WIDTH = FALSE, run_woe = TRUE)
train_data_woe_ef = create_bins(train_data_woe, woe_object_ef_train80, NO_BINS = 5, DO_EQUAL_WIDTH = FALSE, run_woe = TRUE)

# validation_data_woe
validation_data_woe_ew = create_bins(validation_data_woe, woe_object_ew, NO_BINS = 5, DO_EQUAL_WIDTH = TRUE, run_woe = TRUE)
validation_data_woe_ef = create_bins(validation_data_woe, woe_object_ef, NO_BINS = 5, DO_EQUAL_WIDTH = FALSE, run_woe = TRUE)

# test_data_woe
test_data_woe_ew = create_bins(test_data_woe, woe_object_ew_train80, NO_BINS = 5, DO_EQUAL_WIDTH = TRUE, run_woe = TRUE)
test_data_woe_ef = create_bins(test_data_woe, woe_object_ef_train80, NO_BINS = 5, DO_EQUAL_WIDTH = FALSE, run_woe = TRUE)

# class_data_woe
class_woe_ew = create_bins(class_woe, woe_object_ew_train80, NO_BINS = 5, DO_EQUAL_WIDTH = TRUE, run_woe = TRUE)
class_woe_ef = create_bins(class_woe, woe_object_ew_train80, NO_BINS = 5, DO_EQUAL_WIDTH = FALSE, run_woe = TRUE)
