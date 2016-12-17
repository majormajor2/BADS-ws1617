# This is the main script that runs all the other modules

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
# check plausability of data types
# lapply(known,class)

# summarise
lapply(known,summary)
#lapply(class,summary)
