# set to your library
setwd("/Users/orenblumenfeld/Google Drive/MEMS Degree/WiSe16:17/Business Analytics/Assignment")
# read the data set and summarize it for fun
known <- read.csv("assignment_BADS_WS1617_known.csv", sep = ",", header = TRUE)
str(known)
summary(known)
# creatge new matrix only for the variables we need to check
known.outlierscheck <- known
# nullify uneeded columns
known.outlierscheck[ ,c(1:14,17:22)] <- c(NULL)
# download and install matrixStats package.
install.packages("matrixStats")
library(matrixStats)
# Check probabilities for the relevant columns.
colQuantiles(known.outlierscheck, probs=seq(from = 0, to = 1, by = 0.25), na.rm = TRUE)