# fix standardize code for weight
# what to do: weight/total item count * total item count for NAs of weight

# 1. sum up over all itmes
summary(known)
dim(known)
colnames()
transform(known, sum_counts = rowSums())
head(known[,c(grep(pattern = "_count", x = colnames(known)), grep("item_count", invert = TRUE, x = colnames(known)))])
?grep

head(known[,grep("_count&?item", colnames(known))])
?grep

select1 <- head(known[,grep("item_count", invert = TRUE, colnames(known))])
varnames_count <- head(known[,grep("_count", select1)])