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
# item_count: Number of differnent things ordered
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
# 1. All items in canceled items effect weight 
# 2. if canceled item !=0 and weight = 0 we classify it as an error


### Code starts here ###

# 0. preliminary steps

## save weight statistics in varibales
weight_zeros <- sum(known$weight == 0, na.rm = TRUE)
weight_na <- sum(is.na(known$weight))
print(paste("initial no. of zeros in weight:", weight_zeros <- sum(known$weight == 0, na.rm = TRUE)))
print(paste("initial no. of NAs in weight:", weight_na <- sum(is.na(known$weight))))

# helper function to get the list of included products
# input: patterns to include and exlude
# output: list of included columns
getproductlist <- function(exclude_pattern, include_pattern)
{
  list_exclude <- colnames(known[,grep(exclude_pattern, invert = TRUE, colnames(known))])
  list_include <- list_exclude[grep(include_pattern, x = list_exclude)]
  return(list_include)
}

# select product categories (10), that affect weight (used for calculating average per item)
exclude = c("item_count|ebook_count|audiobook_download_count")
include = c("_count|canceled_items")
productlist_w <- getproductlist(exclude_pattern = exclude, include_pattern = include) # gives names of columns

# feedback 
print("List of products, that affect weight:"); print(productlist_w)


# 1. replace "error zeros" in weight by NA

# sum over productlist_w
sumofproducts_w <- rowSums(x = known[,productlist_w], na.rm = TRUE, dims = 1) # length: all 51884 observations

# Identify zeros in weight ## THIS COULD BE PUT INTO 1 LINE ##
idx_zeros <- which(known$weight == 0)
# feedback
print(paste("Total number of zeros in weight:", length(idx_zeros))) # length: 7512
# Identify non-error-zeros in weight
idx_nonerrorzeros <- which(known$weight == 0 & sumofproducts_w == 0 & (known$audiobook_download_count != 0 | known$ebook_count != 0 )) # length: 6.980
# feedback 
print(paste("Number of non-error zeros in weight:", length(idx_nonerrorzero))) # length: 6980
# Identify error-zeros in weight
idx_errorzeros <- idx_zeros[! idx_zeros %in% idx_nonerrorzeros]
# feedback
print(paste("Number of error zeros in weight:", length(idx_errorzeros))) # length: 532

# replace error zeros by NA
known[idx_errorzeros, "weight"] <- NA
# feedback
print(paste("initial no. of NAs in weight:", weight_zeros <- sum(is.na(known$weight))))
print(paste("number of NAs in weight after replacement of error-zeros:", sum(is.na(known$weight))))


# 2. calculate average weight per item for each row

# get index where weight != na or zero  (to calculate average weight only for cases where weight != 0 and not NA)
idx_na_zero <- which(known$weight == 0 | is.na(known$weight)) # length: 11459
str(idx_zeros)
summary(is.na(known$weight))
idx_not_na_zero <- which(known$weight[-idx_na_zero] != 0) # length 40425
length(idx_not_na_zero)
# calcualte avg weight per item for each row
?
avgweight_orderitem <- foreach (row=c(idx_not_na_zero)) 
for (row in 1:nrow(known)){
 <- known[row, "weight"]/sumofproducts_w # length: 51884
}

# 3. calculate overall average weight per item

# get index where weight != na or zero  (to calculate average weight only for cases where weight != 0 and not NA)
idx_na_zero <- which(known$weight == 0 | is.na(known$weight)) # length: 11459
idx_not_na_zero <- which(known$weight[-idx_na_zero] != 0) # length 40425
# calculate overall average weight per item
avgweight_item <- sum((avgweight_orderitem)[idx_not_na_zero])/length((avgweight_orderitem)[idx_not_na_zero])


# 4. replace NAs by average per item

# get index for NAs in weight
idx_na <- which(is.na(known$weight))
# get index for NAs in weight
idx_na <- which(is.na(known$weight))
# feedback 
print(paste("number of NAs to be replaced by average:", length(idx_na)))
# replace each NA by avg weight per item
known[idx_na, "weight"] 

head(idx_na)
known[1:10, c("weight", "ebook_count", "audiobook_download_count",productlist_w)]
test_weight <- known[,"weight"]
known[,"weight"] <- replace(x = known[,"weight"], list = idx_na, values = sumofproducts_w[idx_na]*avgweight_item)




productsum_w <- rowSums(x = known[,productlist_w], na.rm = TRUE, dims = 1) # length: all 51884 observations
length(productsum_w)
length(idx_na)
for (row in idx_na){
  known[row,"weight_b"] <- productsum_w[idx_na]*avgweight_item
}
test <- which(known[,"weight"] == known[,"weight_b"])

# feedback
print(paste("number of NAs after replacement by average:", sum(is.na(known$weight))))

# 5. standardize weight

source("helper.R")
known[,"weight"] <- lapply(known[,"weight"], standardise)


