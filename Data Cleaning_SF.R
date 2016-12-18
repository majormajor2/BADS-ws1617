## Data Cleaning 
summary(known)

## form_of_adress
summary(known$form_of_address)
# create dummy for NAs
known$form_of_address_missing <- factor(ifelse(is.na(known$form_of_address), yes = 1, no = 0))
# plausability check 
summary(known$form_of_address_missing) 
summary(known$form_of_address)

## advertising_code
summary(known$advertising_code)
str(known$advertising_code) # factor w/ 50 levels

# check for missing values (Attention: code only doable before NA-fix)
##
sum(is.na(known$advertising_code)) # no NAs
sum((summary(known$advertising_code)[1])) # 41.593 observations don't have advertising code
sum((summary(known$advertising_code)[1]))/nrow(known) # 80% observations don't have advertising code
# replace "" with NA
known$advertising_code[known$advertising_code == ""] <- "NA"
##

# create dummy variable for NAs
known$advertising_code_missing <- factor(ifelse(is.na(known$advertising_code), yes = 1, no = 0))
# check
summary(known$advertising_code_missing) 
summary(known$advertising_code)

## weight
summary(known$weight)
summary(known$weight == 0)
sum(is.na(known$weight))/nrow(known) # 8% NAs, i.e. 3947 NAs
as.numeric(summary(known$weight == 0)[3])/nrow(known) # 7512 Os , 14%
# create dummy variable
known$weight_missing <- factor(ifelse(is.na(known$weight), yes = 1, no = 0))
# check
summary(known$weight) 
summary(known$weight_missing)


Contact GitHub API Training Shop Blog About
