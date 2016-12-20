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
# remitted_items: lists remitted items, leaves ordered items in product categories
###

### Steps: 

# 1. replace "error zeros" in weight by NA
# 2. calculate average weight per customer and item
# 3. calculate overall average weight per item
# 4. replace NAs by average per item
# 5. standardize weight
###

### Code starts here ###

# 0. preliminary steps

# select product categories (10), that affect weight (used for calculating average per item)
exclude_pattern = c("item_count|ebook_count|audiobook_download_count")
include_pattern = c("_count|canceled_items")

# helper function to get the list of included products
# input: patterns to include and exlude
# output: list of included columns
getproductlist_w <- function(exclude_pattern, include_pattern)
{
  list_exclude <- colnames(known[,grep(exclude_pattern, invert = TRUE, colnames(known))])
  list_include <- list_exclude[grep(include_pattern, x = list_exclude)]
  return(list_include)
}

productlist_w <- getproductlist_w(exclude_pattern = exclude, include_pattern = include) 


# 1. replace "error zeros" in weight by NA

# logic: wherever productlist_w is nonzero & weight is zero, we replace it by NA
# sum over productlist_w
known$productsum_w <- rowSums(x = known[,productlist_w], na.rm = TRUE, dims = 1) # length: all 51884 observations
# get index of errors in weight
idxerrors_w <- which(known$weight == 0 & known$productsum_w != 0) 
# feedback
print(paste("number of zero-errors in weight:", length(idxerrors_w))) 
print(paste("number of NAs in weight before replacement of error-zeros:", sum(is.na(known$weight)))) 
# replace error zeros by NA
known[idxerrors_w, "weight"] <- NA
# feedback
print(paste("number of NAs in weight after replacement of error-zeros:", sum(is.na(known$weight))))



# 2. calculate average weight per item for each row

# get index where weight != na or zero  (zeros should now only show cases where productsum_w = 0 and downloadables != 0)
idx_na_zero <- which(known$weight == 0 | is.na(known$weight))
# calcualte avg weight per item for each row
for (row in known[-idx_na_zero, "weight"]){
avgweight_orderitem <- known$weight/known$productsum_w # length: 40425
}


# 3. calculate overall average weight per item

avgweight_item <- sum(avgweight_orderitem)/length(avgweight_orderitem)


# 4. replace NAs by average per item

# get index for NAs in weight
idx_na <- which(is.na(known$weight))
# feedback 
print(paste("number of NAs to be replaced by average:", sum(is.na(known$weight))))
# replace each NA by avg weight per item
for (row in idx_na){
  known[row,"weight"] <- known[row, "productsum_w"]*avgweight_item
}
# feedback
print(paste("number of NAs after replacement by average:", sum(is.na(known$weight))))

# 5. standardize weight

source("helper.R")
known[,"weight"] <- lapply(known[,"weight"], standardise)


