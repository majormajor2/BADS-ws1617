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
# note to self: make it in a list with names
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


# create boxplots of all the columns and save them to pdf files
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
