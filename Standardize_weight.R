# fix standardize code for weight

### Description of Problem
# We want to standardize weight for clustering use etc. For this, we replace NA values in weight by the average weight 
# per item. 
###

### Description of relevant categories
# import count: is a product category
# product count: physical & non-physical products
# cancelled items: cancels out the counts on respective product category 
# item_count: Number of differnent things ordered
# exampole: item_count 20, cancelled 17, book_count 3
# weights: weight of cancelled items + product count
# remitted_items : lists remitted items, leaves ordered items in product categories
###

### Replace error-Zeros in weight by NA
# define dataframe: all product categories - downloads + canceled_items 
df_werror <- known[,grep("item_count|ebook_count|audiobook_download_count|totalitemcount", invert = TRUE, colnames(known))]
varnames_werror <- colnames(df_werror[,grep(pattern = "_count|canceled_items", x = colnames(df_werror))])
# rowSum over product-columns
df_werror$totalcount_werror <- rowSums(x = known[,varnames_werror], na.rm = TRUE, dims = 1) # sums over all items for each observation
# select rows where product categories >1
idx_werrors <- which(df_werror$weight == 0 & df_werror$totalcount_werror != 0)
# replace by NA
df_werror$weight[idx_werrors] <- NA


### Calculate avg, for known-dataset
calc_itemcount <- function(exclude, include){
# pick columns we want to sum over (_count)
select1 <- colnames(known[,grep(pattern = exclude, invert = TRUE, colnames(known))]) # select all colNames but item_count
varnames_count <- select1[grep(pattern = include, x = select1)] # grap all colNames wrt order-items 
# sum up order-items for each row
known$totalitemcount <- (rowSums(x = known[,varnames_count], na.rm = TRUE, dims = 1)) # sums over all items for each observation
df <- known[!is.na(known$weight),c(varnames_count,"weight", "totalitemcount")]
# Loop for each customer: weight/totalitemcount = avg. weight p. item p. customer#

for (i in nrow(df)){
  df$rowavg <- df$weight/df$totalitemcount
}
colsum_weight <- sum(df$weight)
colsum_rowavg <- sum(df$rowavg)
total_avg <- colsum_weight/colsum_rowavg

for (i in row(df)){
df[is.na(known$weight),"weight"] <- total_avg*df$totalitemcount
}
#

 
# 2. regress weights on product categories + cancelled items, excluding NAs
# for regression: regress weight w/o NAs on all items, including cancelled
###
# exclude NAs and errors from data set
# Zero-errors: if weight == 0 and product category counts|cancelled item counts != 0, replace those zeros by NA
# replace Zero-erros by NA
summary(known) # weight has 3947 NAs, no of columns: 51884

# row sums of all categories + canceled_items - downloads larger than zero 
select2 <- known[,grep("item_count|ebook_count|audiobook_download_count|totalitemcount", invert = TRUE, colnames(known))] # select all colNames but 3 counts
varnames_count2 <- colnames(select2[,grep(pattern = "_count|canceled_items", x = colnames(select2))]) # gra
known$totalcount2 <- rowSums(x = known[,varnames_count2], na.rm = TRUE, dims = 1) # sums over all items for each observation

# select rows where product categories >1
weight_errors <- which(known$weight == 0 & known$totalcount2 != 0)
# replace by NA
known$weight[weight_errors] <- NA


####### DOESNT WORK LIKE THIS: REGRESSION COEFFICIENTS ARE NOT SIGNIFICANT & NEGATIV 
# regress weight on product categories
regweight <- function(data){
  # create data frame: drop rows where weight = NA, use varnames_count
  df <- known[!is.na(known$weight),c(varnames_count,"weight")]
  yhat.weights <- lm(weight~., data = df)
  str(summary(yhat.weights)$coefficients)[,2]
  str(summary(yhat.weights))
  yhat.pvalues <- summary(yhat.weights)$coefficients[,4]<0.05
  names(summary(yhat.weights)$coefficients[,yhat.pvalues])
  }










