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

# talk about outliers