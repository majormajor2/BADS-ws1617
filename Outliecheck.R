# set to your library
setwd("/Users/orenblumenfeld/Google Drive/MEMS Degree/WiSe16:17/Business Analytics/Assignment/bads-ws1617-group27")

# download and install necessary packages.
if(!require("matrixStat")) install.packages("matrixStats"); library(matrixStats)
if(!require("corrplot")) install.packages("corrplot"); library(corrplot)

# read the data set and summarize it for fun
known <- read.csv("assignment_BADS_WS1617_known.csv", sep = ",", header = TRUE)
str(known)
summary(known)
# creatge new matrix only for the variables we need to check
known.outlierscheck <- known

# nullify uneeded columns
known.outlierscheck[ ,c(2:14,17:22)] <- c(NULL)

# Assign row and column names
known.outlierscheck.colnames <- colnames(known.outlierscheck)
known.outlierscheck.rownames <- paste("Quantile", seq(0,1,0.25), sep = " ")

# create matrix of the quantiles
known.quantiles <- matrix(data = colQuantiles(known.outlierscheck, probs=seq(from = 0, to = 1, by = 0.25), na.rm = TRUE), ncol = ncol(known.outlierscheck), byrow = TRUE)
colnames(known.quantiles) <- known.outlierscheck.colnames
rownames(known.quantiles) <- known.outlierscheck.rownames

# create a matrix for the quantiles without the zeros and ones (stupid - delete?).
# known.outlierscheck.nozeros <- known.outlierscheck
# known.outlierscheck.nozeros[known.outlierscheck.nozeros== 0] <- NA
# known.outlierscheck.nozeros[known.outlierscheck.nozeros== 1] <- NA
# known.quantiles.nozeros <- matrix(data = colQuantiles(known.outlierscheck.nozeros, probs=seq(from = 0, to = 1, by = 0.25), na.rm = TRUE), ncol = ncol(known.outlierscheck), byrow = TRUE)
# colnames(known.quantiles.nozeros) <- known.outlierscheck.colnames
# rownames(known.quantiles.nozeros) <- known.outlierscheck.rownames


# create boxplots
dev.off()
# the former line makes sure the device is off to avoid a possible error
pdf(file = "plot goods_value.pdf")
boxplot(known.outlierscheck$goods_value)
dev.off()
known.outlierscheck <- known.outlierscheck
for (i in colnames(known.outlierscheck[,-1])){
  pdf(file = paste("plot ",i,".pdf", sep = ""))
  boxplot(known.outlierscheck[,i])
  dev.off()
}

# we can see that the quantiles matrix and the boxplots are not very informative and
# we can see that most of the values are 0's. We need a different way to understand
# how the data looks like.

# create a function
# input: column of a data frame
# output: a table (with percentage) for each column
pertablefun <- function(x){
  tab <- table(x)
  pertab <- cbind(tab, prop.table(tab)*100)
  colnames(pertab) <- c("Count", "Percentage")
  return(pertab)
}
output.pertablefun <- lapply(known.outlierscheck[,-1], pertablefun)
print(output.pertablefun)

# we can see that the vast majority of the values of most variables are 0's and 1's.
# There are many other values in small numbers and it makes it very hard to understand
# how the data looks like. Ee can unify all numbers bigger than 1 to have a better look at the data.
# so we will make a table and unify all numbers bigger than 1 to 3
known.outlierscheck.lessvalues <- known.outlierscheck
known.outlierscheck.lessvalues[known.outlierscheck.lessvalues > 1] <- 3
# apply pertablefun again on the new table
output.pertablefunless <- lapply(known.outlierscheck.lessvalues[,-1], pertablefun)
print(output.pertablefunless)

# now we can see clearly that for most variables the data is made out of mostly 0's and/or 1's.
# we can assume that much of the information that we can receive is from the higher point.
# but we want to cluster the data so that we can get information from the groups.

# clustering
# ----------

# we want to choose only columns with more than 10 different numbers. first change all the columns to factors.
known.outlierscheck.factor <- lapply(known.outlierscheck, factor)

# then see how many factors we have in each column
lapply(known.outlierscheck.factor, is.factor)
lapply(known.outlierscheck.factor[-1], nlevels)

# lastly save a new matrix only with the columns we want to check and turn it back into numeric so that we can cluster
known.outlierscheck.factormany <- known.outlierscheck.factor[which(sapply(known.outlierscheck.factor, nlevels) > 10)]
known.outliercheck.forcluster <- as.data.frame(lapply(known.outlierscheck.factormany, as.numeric))

# standardize columns
source("helper.R")
known.outlierscheck.stand <- known.outliercheck.forcluster
known.outlierscheck.stand <- as.data.frame(cbind(known.outliercheck.forcluster[,1, drop = FALSE], sapply(known.outlierscheck.stand[,-1], standardise)))

######### delete the next line after we fix the standartization formula!
known.outlierscheck.stand <- known.outlierscheck.stand[c(1:2,4:16)]
#########

# clustering
set.seed(666)
# candidate settings for k
k.settings = 1:10
# create matrix to store the results
cluster.model <- as.data.frame(matrix(data = NA, nrow = length(k.settings), ncol = ncol(known.outlierscheck.stand)))
colnames(cluster.model) <- colnames(known.outlierscheck.stand)
rownames(cluster.model) <- 1:10



# create elbow curves for all variables:
dev.off()
for (i in colnames(cluster.model)){
  pdf(file = paste("elbow ",i,".pdf", sep = ""))
  plot(k.settings, cluster.model[,i], xlab = "k", ylab="Total within-cluster SS",
       main = "Elbow curve for k selection", col="red", type = "b")
  dev.off()
}

# we see that the "elbow" is between k=3 and k=4. With 3, it is probable that the clusters will
# include all the 0's in one cluster, all the 1's in the second cluster and all the other value
# in the third cluster. We want to divide the information more rather than less, so that we can
# get more information from the higher values. Now we will assign a dummy variable to each value
# according to his respective cluster, using k=4.


known.clusterdummies <- as.data.frame(matrix(nrow = nrow(known.outlierscheck.stand), ncol = ncol(known.outlierscheck.stand)))
colnames(known.clusterdummies) <- colnames(known.outlierscheck.stand)
for (name in colnames(known.outlierscheck.stand)) {
  clu.sol <- kmeans(known.outlierscheck.stand[,name], centers=4, iter.max = 50, nstart = 100)
  cluster <- clu.sol$cluster
  known.clusterdummies[,name] <- cluster
}
known.clusterdummies[,1] <- known.outlierscheck.stand[,1]
colnames(known.clusterdummies) <- colnames(known.outlierscheck.stand)

# now we can check correlations between the return column and the other columns, using our clustered
# matrix.

##### important: change "known.outlierscheck.stand" to "known.outlierscheck" after the weight proglem is fixed!!!
##### did this so that the number of columns will fit

merged <- merge(known.clusterdummies, known.outlierscheck.stand, by = "ID", suffixes = c(".clustered", ".original"))

# the rest is at work. still not working.
# hamayun explains: create a dummy variable - 1 when the variable is in the cluster, 0 when it's not. create corplot for each one. 4 dummies and 1 return
# relevant columns: 2:15

# create table for cluster 1 (1 if the cluster is 1, 0 if not)
merged.dummy1 <- merged
fdummy1 <- function(x) ifelse(x == 1, 1, 0)
merged.dummy1[2:15] <- sapply(merged.dummy1[2:15], fdummy1)


# create table for cluster 2 (1 if the cluster is 2, 0 if not)
merged.dummy2 <- merged
fdummy2 <- function(x) ifelse(x == 2, 1, 0)
merged.dummy2[2:15] <- sapply(merged.dummy2[2:15], fdummy2)


# create table for cluster 3 (1 if the cluster is 3, 0 if not)
merged.dummy3 <- merged
fdummy3 <- function(x) ifelse(x == 3, 1, 0)
merged.dummy3[2:15] <- sapply(merged.dummy3[2:15], fdummy3)

# create table for cluster 4 (1 if the cluster is 4, 0 if not)
merged.dummy4 <- merged
fdummy4 <- function(x) ifelse(x == 4, 1, 0)
merged.dummy4[2:15] <- sapply(merged.dummy4[2:15], fdummy4)


