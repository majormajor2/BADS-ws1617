### Variable Transformation ###

# Tasks 
# 1. discretize continuous variables (to avoid impact of outliers, deal w/ missing values, fit theoretical concepts..)
# 2. reduce no of factor levels (factors with too many levels slow down calculations (they are all transformed into dummy varibales))
# 3. projection to numeric scale according to "weight of evidence" approach


### 1.Binning
# transforms continuous variables into factors
# potential candidates: postcode_invoice, item_count, weight, & all other item counts

## set up function

binning  <- function(num.vector, NO_BINS = 4, DO_EQUAL_WIDTH = TRUE){
  if (DO_EQUAL_WIDTH==TRUE) {
    ### Equal-width binning
    # The cut function simply takes the number of bins
    output <- cut(num.vector, NO_BINS, include.lowest = TRUE, labels = paste0("level",1:NO_BINS))
    
  } else {
    #### Equal-frequency binning
    # Argument breaks can also take specific break points, so
    # we use function quantile() to find out where to cut the variable.
    # The n quantile gives us the value below which n% of the data are located. Hence, equal-spaced quantiles give us bins with an equal percentage of 
    # observations in each bin
    # The difficulty lies in automatically adjusting the number of quantiles
    breaks <- quantile(num.vector, 0:NO_BINS/NO_BINS)
    output <- cut(num.vector, breaks, include.lowest = TRUE, right = FALSE, labels = paste0("level",1:NO_BINS))
  } 
}


## run function

known$[putvariablehere] <- binning(known$[putvariablehere], DO_EQUAL_WIDTH = FALSE)


## integrate level for missing values

# This has no impact on estimation (because the factor level will be turned into
# a dummy variable again), but is more readable
loans$YOB <- as.character(loans$YOB)
loans$YOB[loans$YOB_missing == 1] <- "NoInformation"
loans$YOB <- factor(loans$YOB)
loans$YOB_missing <- NULL


## Attention
# take care of new observation, e.g. in the test or classification set or new observations in the business environment
# they need to be transformed in the same way and into the same categories
# Make sure that your code can handle missing values if those might occur



### 2. Reduce no of factor levels ###

# Coarse classification with Chi-Squared ($\chi^2$) method
# project the categorical variable onto a numeric scale with the weight-of-evidence approach
# attention: problematic if some levels are very rare

# 0. potential candidates: email_domain, postcode_delivery, postcode_invoice, advertising_code, 
# 1. create two possible grouping options of rare values
# 2 check which grouping is more different wrt the independent assumption



### --- code starts here --- ###

```{r}
if(!require("caret")) install.packages("caret"); library("caret") # load the package

# Analyze the frequency of the levels of EMPS_A
summary(loans)
summary(loans$EMPS_A)
# B, M, N, U, W, Z are significantly rarer than the other levels
# That may cause problems when a level does not occur in the training data

# Group levels together according to option 1
EMPS_A_opt1 <- as.character(loans$EMPS_A)
EMPS_A_opt1[EMPS_A_opt1 %in% c("B", "M")] <- "STATE"
EMPS_A_opt1[EMPS_A_opt1 %in% c("N", "U", "W", "Z")] <- "OTHER"

# Group levels together according to option 2
EMPS_A_opt2 <- as.character(loans$EMPS_A)
EMPS_A_opt2[EMPS_A_opt2 %in% c("B", "M", "N")] <- "RARE_JOBS"
EMPS_A_opt2[EMPS_A_opt2 %in% c("R","U", "Z")] <- "NO_EMPLOYMENT"

# Use function chisq.test() to perform a chi-squared test on two factor variables
# i.e. the target variable and the factor variable whose levels are to be tested
chisq.opt1 <- chisq.test(loans$BAD, EMPS_A_opt1)
chisq.opt2 <- chisq.test(loans$BAD, EMPS_A_opt2)

# Replace the original levels by the better grouping
print(paste("Chi Sq of option 1:", chisq.opt1$statistic))
print(paste("Chi Sq of option 2:", chisq.opt2$statistic))

loans$EMPS_A <- factor(EMPS_A_opt2)

### --- end of code ---- ###



### 3. Weight of Evidence (WoE)
# attention: overfitting might occur

# 1. split training set into woe-training set to avoid overfitting
set.seed(123)
woe.test <- createDataPartition(y = known$return_customer, p = 0.2, list = FALSE) # Draw a random, stratified sample including p percent of the data
test.woe <-  known[woe.test, ] # test set
train.woe <- known[-woe.test, ] # training set

# Function to perform WOE coding is available in the klaR package
if(!require("klaR")) install.packages("klaR"); library("klaR") # load the package



