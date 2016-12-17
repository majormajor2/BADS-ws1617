#get data
setwd("/Users/sfinkenwirth/Documents/MEMS/Lectures and Seminars/Business Analytics & Data Science/Assignment_BADS_WS1617/Input")
known <- read.csv("assignment_BADS_WS1617_known.csv", header = TRUE, sep = ",")

# load packages
if(!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)


# fix plot resolutions
par(mar=c(1,1,1,1)) # this code avoids some errors related to screen resolution; feel free to ignore it

# structure of dataset
str(known)
summary(known)
