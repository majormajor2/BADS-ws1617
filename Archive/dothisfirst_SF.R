
#get wd and load helper function

setwd("/Users/sfinkenwirth/Documents/MEMS/Lectures and Seminars/Business Analytics & Data Science/Assignment/bads-ws1617-group27")
source("helper.R")

setwd("/Users/sfinkenwirth/Documents/MEMS/Lectures and Seminars/Business Analytics & Data Science/Input")
source("BADS-HelperFunctions.Rmd")
loans <- get.loan.dataset()

# get data 
known <- get_dataset("assignment_BADS_WS1617_known.csv")


# load packages
if(!require("ggplot2")) install.packages("ggplot2"); library(ggplot2)


# fix plot resolutions
par(mar=c(1,1,1,1)) # this code avoids some errors related to screen resolution; feel free to ignore it

