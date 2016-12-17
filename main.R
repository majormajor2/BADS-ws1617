# This is the main script that runs all the other modules

source("helper.R")
known = get_dataset("assignment_BADS_WS1617_class.csv")
lapply(known,class)
lapply(known,summary)
