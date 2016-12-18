# This is the main script that runs all the other modules

#######################
# Load packages
library(lubridate)

#######################
# Load modules
source("helper.R")

#######################
# Load data
# known - training data
known = get_dataset("assignment_BADS_WS1617_known.csv")
# class - data to be classified
class = get_dataset("assignment_BADS_WS1617_class.csv")
#######################
# Treat postcodes and missing variables
known = treat_missing_values(known)
class = treat_missing_values(class)
known = treat_postcodes(known)
class = treat_postcodes(class)
known = treat_dates(known)
class = treat_dates(class)
#######################
# check plausability of data types
# lapply(known,class)

# summarise
#lapply(known,summary)
#lapply(class,summary)
