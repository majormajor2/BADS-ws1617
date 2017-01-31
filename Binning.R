###### --- UNSUPERVISED BINNING --- #########

source("woe.R")
# this functions cuts a column into a pre-specified no of bins, either with equal width or equal frequency
# input: datset, columns (default is set)
# output: new dataset with columns replaced by factor levels = number of bins

create_bins  <- function(dataset, woe_object = woe_object, NO_BINS = 5, DO_EQUAL_WIDTH = TRUE, run_woe = TRUE){

  ## --- PRELIMINARY --- ##

  # define columns to be binned
  # columns form_of_address, model, payment only 3-4 levels, so they are not binned
  columns <- c("email_domain", "postcode_invoice", "postcode_delivery", "advertising_code")
  
  # check if columns are numeric
  for(column in columns){
    if(!is.numeric(dataset[,column])){
      stop("Please use the woe-version of your dataset. At least one of the columns you want to bin is not numeric.")
      }
  }
  
  ## --- BINNING CODE STARTS HERE --- ##
  

  
  # BIN WITH EQUAL WIDTH
  
  if(DO_EQUAL_WIDTH){
    
    # loop over all columns
    for(column in columns){
      
      # cut column-values into equal-width-bins and replace values with factor-levels
      dataset[,column] <- cut(dataset[,column], NO_BINS, include.lowest = TRUE, labels = paste0("level",1:NO_BINS))
      }   
    
  # BIN WITH EQUAL FREQUENCY  
    } else {
   
    # loop over all columns
    for(column in columns){
      
      # define quantile breakpoints 
      breaks <- as.numeric(quantile(dataset[,column], 0:NO_BINS/NO_BINS))
      
      # check if breaks are unique
      while(any(duplicated(breaks))){
        # change duplicate breaks by small value and repeat until no more duplicates are present in "breaks"
        breaks[duplicated(breaks)] <- breaks[duplicated(breaks)]+0.0000001
        } 
      
      # cut column-values into bins with approx. equal number of observations and replace values with factor-levels    
      dataset[,column] <- cut(dataset[,column], breaks, include.lowest = TRUE, right = FALSE, labels = paste0("level",1:NO_BINS)) 
      } 
    } # END: BIN WITH EQUAL FREQUENCY or WITH EQUAL FREQUENCY 

  
  # 2. APPLY WOE  

  if(run_woe){
      # apply woe_object to current dataset
      dataset_woe <- apply_woe(dataset = dataset, woe_object = woe_object, doReplace = TRUE)
      return(dataset_woe) # RETURNS BINNED COLUMNS AS WOE-TRANSFORMED NUMERICS 
      
      } else{
        return(dataset) # RETURNS COLUMNS AS BINNED FACTORS
        } 
  
} # END OF FUNCTION




