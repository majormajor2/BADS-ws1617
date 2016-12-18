# set to your library
setwd("/Users/orenblumenfeld/Google Drive/MEMS Degree/WiSe16:17/Business Analytics/Assignment/bads-ws1617-group27")

# download and install necessary packages.
if(!require("matrixStat")) install.packages("matrixStats"); library(matrixStats)
if(!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)

# read the data set and summarize it for fun
known <- read.csv("assignment_BADS_WS1617_known.csv", sep = ",", header = TRUE)
str(known)
summary(known)
# creatge new matrix only for the variables we need to check
known.outlierscheck <- known

# nullify uneeded columns
known.outlierscheck[ ,c(1:14,17:22)] <- c(NULL)

# Assign row and column names
known.outlierscheck.colnames <- colnames(known.outlierscheck)
known.outlierscheck.rownames <- paste("Quantile", seq(0,1,0.25), sep = " ")

# create matrix of the quantiles
known.quantiles <- matrix(data = colQuantiles(known.outlierscheck, probs=seq(from = 0, to = 1, by = 0.25), na.rm = TRUE), ncol = ncol(known.outlierscheck), byrow = TRUE)
colnames(known.quantiles) <- known.outlierscheck.colnames
rownames(known.quantiles) <- known.outlierscheck.rownames

# create a matrix for the quantiles without the zeros.
known.outlierscheck.nozeros <- known.outlierscheck
known.outlierscheck.nozeros[known.outlierscheck.nozeros==0] <- NA
known.quantiles.nozeros <- matrix(data = colQuantiles(known.outlierscheck.nozeros, probs=seq(from = 0, to = 1, by = 0.25), na.rm = TRUE), ncol = ncol(known.outlierscheck), byrow = TRUE)
colnames(known.quantiles.nozeros) <- known.outlierscheck.colnames
rownames(known.quantiles.nozeros) <- known.outlierscheck.rownames


# create boxplots (trial)
dev.off()
# the former line makes sure the device is off to avoid a possible error
pdf(file = "plot goods_value.pdf")
boxplot(known.outlierscheck$goods_value)
dev.off()
known.outlierscheck <- known.outlierscheck
for (i in colnames(known.outlierscheck)){
  pdf(file = paste("plot ",i,".pdf", sep = ""))
  boxplot(known.outlierscheck[,i])
  dev.off()
}

# we could see that the quantiles matrix and the boxplots are not very informative and we can see that most of the values are 0's. We need a different way to understand how the data looks like.

# create a function
# input: column of a data frame
# output: a table (with percentage) for each column
pertablefun <- function(x){
  tab <- table(x)
  pertab <- cbind(tab, prop.table(tab)*100)
  colnames(pertab) <- c("Count", "Percentage")
  return(pertab)
}
output.pertablefun <- lapply(known.outlierscheck, pertablefun)

# we can see that the vast majority of the values of most variables are 0's and 1's. We can unify all numbers bigger than 1 to have a better look at the data.
# make a table and unify all numbers bigger than 1 to 3
known.outlierscheck.lessvalues <- known.outlierscheck
known.outlierscheck.lessvalues[known.outlierscheck.lessvalues > 1] <- 3
# apply pertablefun again on the new table
output.pertablefunless <- lapply(known.outlierscheck.lessvalues, pertablefun)


# clustering
# ----------

# we want to choose only columns with more than 10 different numbers. first change all the columns to factors.
known.outlierscheck.factor <- lapply(known.outlierscheck, factor)

# then see how many factors we have in each column
lapply(known.outlierscheck.factor, is.factor)
lapply(known.outlierscheck.factor, nlevels)

# lastly save a new matrix only with the columns we want to check and turn it back into numeric so that we can cluster
known.outlierscheck.factormany <- known.outlierscheck.factor[which(sapply(known.outlierscheck.factor, nlevels) > 10)]
known.outliercheck.forcluster <- lapply(known.outlierscheck.factormany, as.numeric)

# standardize columns
source("helper.R")
known.outlierscheck.stand <- as.data.frame(lapply(known.outliercheck.forcluster, standardise))

######### delete the next line after we fix the standartization formula!
known.outlierscheck.stand <- known.outlierscheck.stand[c(1,3:15)]
#########

# clustering
set.seed(666)
# candidate settings for k
k.settings = 1:10
# create matrix to store the results
cluster.model <- as.data.frame(matrix(data = NA, nrow = length(k.settings), ncol = ncol(known.outlierscheck.stand)))
colnames(cluster.model) <- colnames(known.outlierscheck.stand)
rownames(cluster.model) <- 1:10

# create cluster solutions for k
for (n in 1:ncol(known.outlierscheck.stand)) {
  for (i in 1:length(k.settings)) {
    obj.values <- vector(mode="numeric", length = length(k.settings))
    # Create a cluster solution using the current setting of k
    clu.sol <- kmeans(known.outlierscheck.stand[n], centers=k.settings[i], iter.max = 50, nstart = 100)
    obj.values[i] <- clu.sol$tot.withinss
    cluster.model[i,n] <- obj.values[i]
  }
}

# create elbow curves for all variables:
dev.off()
for (i in colnames(cluster.model)){
  pdf(file = paste("elbow ",i,".pdf", sep = ""))
  plot(k.settings, cluster.model[,i], xlab = "k", ylab="Total within-cluster SS",
       main = "Elbow curve for k selection", col="red", type = "b")
  dev.off()
}

