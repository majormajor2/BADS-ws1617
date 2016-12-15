# set to your library
setwd("/Users/orenblumenfeld/Google Drive/MEMS Degree/WiSe16:17/Business Analytics/Assignment/bads-ws1617-group27")
# read the data set and summarize it for fun
known <- read.csv("assignment_BADS_WS1617_known.csv", sep = ",", header = TRUE)
str(known)
summary(known)
# creatge new matrix only for the variables we need to check
known.outlierscheck <- known
# nullify uneeded columns
known.outlierscheck[ ,c(1:14,17:22)] <- c(NULL)
# download and install matrixStats package.
if(!require("matrixStat")) install.packages("matrixStats"); library(matrixStats)
# Check probabilities for the relevant columns.
known.outlierscheck.names <- colnames(known.outlierscheck)
# create matrix of the quantiles
known.quantiles <- matrix(data = colQuantiles(known.outlierscheck, probs=seq(from = 0, to = 1, by = 0.25), na.rm = TRUE), nrow = 5, ncol = ncol(known.outlierscheck), byrow = TRUE)
colnames(known.quantiles) <- known.outlierscheck.names
