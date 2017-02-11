###### Functions for Data Cleaning ##########

# treats the missing values in the data frame
# input: data frame with missing values
# output: data frame with treated missing values
treat_NAs = function(data) 
{
  ## Replace missing entries for advertising_code with NA
  data$advertising_code[data$advertising_code == ""] = NA
  # drop empty level from factor
  data$advertising_code = droplevels(data$advertising_code)
  # encode NAs as level "Missing" - then a dummy variable is no longer needed as NAs
  data$advertising_code = fct_explicit_na(data$advertising_code, "Missing")
  
  ## New level for advertising_code in class dataset (level in question: "AA", 1 observation)
  # replace AA in class by modal value of advertising_code in known dataset
  new_levels = check_new_levels(known_data = known, class_data = data)
  if(length(new_levels) > 0)
  {
    for(variable in names(new_levels))
    {
      for(level in new_levels[variable])
      {
        
        data[data[,variable] == level, variable] = names((sort(-table(data[,variable]))))[1]
        data[,variable] = droplevels(data[,variable])
      }
    }
  }
  
  ## Encode NAs for form_of_address with level "Missing"
  # dummy variable is no longer needed as NAs are encoded as level "Missing"
  data$form_of_address = fct_explicit_na(data$form_of_address, "Missing")
  
  
  return(data)
}


### Date cleaning function ###
# input: data frame
# output: data frame
treat_dates = function(data) {
  
  ## Order date does not require cleaning, there are no missing values or outliers
  # i.e. all are within the correct interval of one year
  
  ## Account creation date: Create a dummy variable for NAs
  data$account_creation_date_missing = ifelse(is.na(data$account_creation_date), 1, 0)
  ## and replace NAs to match order date (equal in 89% of the cases)
  # (We notice a large clustering at the beginning of the period)
  
  # index the NAs for account creation date
  na_index = which(is.na(data$account_creation_date))
  # replace them with order date
  data$account_creation_date[na_index] = data$order_date[na_index]
  
  ## Delivery date estimated has outliers, from 2010 and 4746. Create a dummy to capture both
  data$deliverydate_estimated_outliers = ifelse(year(data$deliverydate_estimated) == 2010 | year(data$deliverydate_estimated) == 4746, 1, 0)
  
  ## Change 2010 to 2014 (all order dates are within that year)
  year(data$deliverydate_estimated[year(data$deliverydate_estimated) == 2010]) = 2014
  
  # replace deliverydate_estimated for year 4746 by order_date + median estimated delivery time
  deliverytime_estimated = data$deliverydate_estimated - data$order_date
  median_deliverytime_estimated = median(deliverytime_estimated[data$deliverydate_estimated_outliers == 0])
  data$deliverydate_estimated[year(data$deliverydate_estimated) == 4746] = data$order_date[year(data$deliverydate_estimated) == 4746] + round(median_deliverytime_estimated)
  
  ## Delivery date actual has 0000/00/00 which has been coerced to NA by the transformation to dates
  ## Create a dummy for the missing value
  data$deliverydate_actual_missing = ifelse(is.na(data$deliverydate_actual), 1, 0)
  
  ## index the NAs for delivery_date_actual
  na_index = which(is.na(data$deliverydate_actual))
  
  ## replace them with estimated delivery date (which are not outliers because they have just been treated)
  ## + average deviation of deliverydate_actual & deliverydate_estimated
  data$deliverydate_actual[na_index] = data$deliverydate_estimated[na_index] + median(data$deliverydate_estimated[-na_index]-data$deliverydate_actual[-na_index])
  
  ## factorise dummy variables
  data$account_creation_date_missing = factor(data$account_creation_date_missing, labels=c("no","yes"))
  data$deliverydate_actual_missing = factor(data$deliverydate_actual_missing, labels=c("no","yes"))
  data$deliverydate_estimated_outliers = factor(data$deliverydate_estimated_outliers, labels=c("no","yes"))
  
  ## Create factor variables for the weekdays
  ## since these are not captured in the deltas that follow from the next steps
  ## (No weekday for account_creation_date because the correlation with order_date is high)
  data$order_date_weekday = as.POSIXlt(data$order_date)$wday
  data$deliverydate_estimated_weekday = as.POSIXlt(data$deliverydate_estimated)$wday
  data$deliverydate_actual_weekday = as.POSIXlt(data$deliverydate_actual)$wday
  
  ## Replace date variables by deltas, since the dates are naturally strongly correlated
  ## and additional information is captured in their differences.
  ## We choose order_date as the starting point as there are no missing values or outliers.
  ## order_date captures a decision of the customer, 
  ## while delivery times and difference between estimated and actual delivery date capture the workings of the company.
  data$account_creation_date = data$account_creation_date - data$order_date
  data$deliverydate_estimated = data$deliverydate_estimated - data$deliverydate_actual
  data$deliverydate_actual = data$deliverydate_actual - data$order_date
  
  return(data)
}


# treats postcodes 
# standardize postcodes to 2 digits
# create dummy variable for missing values
# also convert data type to factor
# input: data frame
# output: data frame
treat_postcodes = function(data) {
  
  # create dummy postcode_delivery_missing
  data$postcode_delivery_missing = vector(mode="integer",length=length(data$postcode_delivery))
  
  # replace missing values by NA, get rid of non-numeric postal codes
  data$postcode_invoice = sapply(data$postcode_invoice, as.character) 
  data$postcode_delivery = sapply(data$postcode_delivery, as.character)
  data$postcode_invoice = sapply(data$postcode_invoice, as.numeric) 
  data$postcode_delivery = sapply(data$postcode_delivery, as.numeric)
  
  # check if there are NAs in postcode_invoice and replace by 1 
  # this should only affect under 10 entries (where it is "??" or 0)
  
  data$postcode_invoice[is.na(data$postcode_invoice)] = 1
  data$postcode_invoice[data$postcode_invoice == 0] = 1
  data$postcode_delivery[data$postcode_delivery == 0] = 1 
  # no line for NA in postcode_delivery because NAs are missing entries
  
  if(NA %in% data$postcode_delivery)
  {
    # index NAs for later use in postcode_invoice
    na_index = which(is.na(data$postcode_delivery))
    # set true values in the dummy variable
    data$postcode_delivery_missing[na_index] = 1
    # replace missing postcodes with postcode_invoice
    # same in about 60% of cases, affects 49608 cases
    data$postcode_delivery[na_index] = data$postcode_invoice[na_index] 
  }
  
  # factorise dummy variable
  data$postcode_delivery_missing = factor(data$postcode_delivery_missing, levels=c(0,1), labels=c("no","yes"))
  data$postcodes_different = factor(ifelse(data$postcode_invoice != data$postcode_delivery, 1, 0), levels=c(0,1), labels=c("no","yes"))
  
  # factorise postcode variables
  data$postcode_invoice = factor(data$postcode_invoice)
  data$postcode_delivery = factor(data$postcode_delivery)
  
  return(data)
}




# fix standardize code for weight

### Description of Problem

# We want to standardize weight for clustering use etc. For this, we replace NA values in weight by the average weight 
# per item. 
###

### Description of relevant categories

# product categories that affect weight: "canceled_items", "book_count", "paperback_count", "schoolbook_count", "audiobook_count", "film_count", "musical_count"   
# "hardware_count", "imported_count", "other_count"  
# product count: physical & non-physical products
# cancelled items: cancels out the counts on respective product category 
# item_count: Number of different things ordered
# exampole: item_count 20, cancelled 17, book_count 3
# weights: weight of cancelled items + product count
# there are also cases were weight = 0 and canceled items != 0 --> then we assume all canceled items are downloadables
# remitted_items: lists remitted items, leaves ordered items in product categories
###


### Steps: 

# 1. replace "error zeros" in weight by NA
# 2. calculate average weight per customer and item
# 3. calculate overall average weight per item
# 4. replace NAs by average per item
# 5. standardize weight
###


### Remark: 
# we make following simplifying assumption: 
# 1. All items in canceled items affect weight 
# 2. if canceled item !=0 and weight = 0 we classify it as an error


### Code starts here ###
treat_weight = function(dataset, shallPrint = FALSE)
{
  data = dataset  
  
  # 0. preliminary steps
  # create dummy variable for NAs
  data$weight_missing = factor(ifelse(is.na(data$weight), 1, 0), labels=c("no","yes"))
  
  ## weight statistics 
  if(shallPrint)
  {
    print(paste("initial no. of zeros in weight:", sum(data$weight == 0, na.rm = TRUE))) 
    print(paste("initial no. of NAs in weight:", sum(is.na(data$weight))))
  }
  # select product categories (10), that affect weight (used for calculating average per item)
  exclude = c("item_count|ebook_count|audiobook_download_count")
  include = c("_count|canceled_items")
  productlist = get_matching_columns(data, exclude_pattern = exclude, include_pattern = include) # gives names of columns
  
  # feedback 
  if(shallPrint)
  {
    print("List of products that affect weight:"); print(productlist)
  }
  
  # 1. replace "error zeros" in weight by NA
  
  # sum over productlist
  sum_of_products = rowSums(x = data[,productlist], na.rm = TRUE, dims = 1) # length: all 51884 observations
  
  # Identify zeros in weight ## THIS COULD BE PUT INTO 1 LINE ##
  idx_zeros = which(data$weight == 0)
  # feedback
  if(shallPrint)
  {
    print(paste("Total number of zeros in weight:", length(idx_zeros))) # length: 7512
  }
  # Identify non-error-zeros in weight
  idx_nonerrorzeros = which(data$weight == 0 & sum_of_products == 0)
  # old definition:
  # idx_nonerrorzeros = which(data$weight == 0 & sum_of_products == 0 & (data$audiobook_download_count != 0 | data$ebook_count != 0 )) # length: 6.980
  
  # feedback 
  if(shallPrint)
  {
    print(paste("Number of non-error zeros in weight:", length(idx_nonerrorzeros))) # length: 6980
  }
  
  # Identify error-zeros in weight
  idx_errorzeros = idx_zeros[! idx_zeros %in% idx_nonerrorzeros]
  # feedback
  if(shallPrint)
  {
    print(paste("Number of error zeros in weight:", length(idx_errorzeros))) # length: 532
  }
  
  # replace error zeros by NA
  data[idx_errorzeros, "weight"] = NA
  # feedback
  if(shallPrint)
  {
    print(paste("number of NAs in weight after replacement of error-zeros:", sum(is.na(data$weight))))
  }
  
  # 2. calculate average weight per item for each row
  
  # get index where weight != na and != 0  (to calculate average weight only for cases where weight != 0 and not NA)
  idx_foravgweight = which(! (data$weight == 0 | is.na(data$weight))) # length: 40425
  
  # check index idx_foravgweigth
  #summary(data$weight[idx_foravgweight]) # Min: 1
  #summary(is.na(data$weight[idx_foravgweight])) # no NAs in weight
  #summary(data$weight[idx_foravgweight] == 0) # No Zeros in weight --> sum_of_products should be nonzero
  
  #summary(data$sum_of_products) # no NAs
  #summary(data$sum_of_products[idx_foravgweight]) # Min is zero
  #summary(data$sum_of_products[idx_foravgweight] == 0) # Number of zeros: 29
  
  # Check column weight where sumofproducts == 0 
  #data$weight[idx_foravgweight][data$sum_of_products[idx_foravgweight] == 0] # there are 29 such cases
  #summary(data[idx_foravgweight,][data$sum_of_products[idx_foravgweight] == 0,]) # all product categories == 0, but item_count!
  #str(data[idx_foravgweight,][data$sum_of_products[idx_foravgweight] == 0,]) # all product categories == 0, but item_count!
  
  ### TO DO ###
  # Decide: how should those 29 cases be treated? 
  ### END TO DO ###
  
  # Problem: There are cases where weight != 0 but sumofproducts == 0
  # Treatment: exclude those 29 cases from index
  if(shallPrint)
  {
    print(paste("Length of idx_foravgweight before:", length(idx_foravgweight)))
  }
  
  idx_foravgweight = idx_foravgweight[sum_of_products[idx_foravgweight] != 0] # exclude those IDs from idx_foravgweight
  # feedback & check
  if(shallPrint)
  {
    print(paste("Length of idx_foravgweight after treatment:", length(idx_foravgweight)))
  }
  
  if (min(sum_of_products[idx_foravgweight]) == 0)
  {
    stop("There is at least one row where sum_of_products == 0.")
  }
  
  
  
  # 3. calculate overall average weight per item
  avgweight_item = mean(data[idx_foravgweight,"weight"]/sum_of_products[idx_foravgweight])
  #if (length(avgweight_item) != length(idx_foravgweight))
  #{
  #  stop("Avgweight_item has the wrong dimension.")
  #}
  #if (sum(is.na(avgweight_item)) >0 | sum(avgweight_item) == Inf)
  #{
  #  stop("Avgweight_item == NA or Inf in at least one row")
  #}
  if(shallPrint)
  {
    print(paste("Average weight per item:", round(avgweight_item), "grams"))
  }
  
  # 4. replace NAs by average per item
  
  # get index for NAs in weight
  idx_na = which(is.na(data$weight))
  # feedback 
  if(shallPrint)
  {
    print(paste("number of NAs to be replaced by average:", length(idx_na)))
  }
  
  # replace each NA by avg weight per item
  data$weight = replace(x = data$weight, list = idx_na, values = sum_of_products[idx_na]*avgweight_item)
  
  # feedback 
  if(shallPrint)
  {
    print(paste("number of NAs in weight after replacement by average:", sum(is.na(data$weight))))
  }
  
  
  # feedback
  if(shallPrint)
  {
    print("Range of weight:")
    print(summary(data$weight))
  }
  
  return(data)
}

