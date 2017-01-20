### DATA CORRELATIONS ###

# Calculate the correlation matrix
# We select the numeric variabels inside of the function by applying function is.numeric to each element of dataframe loans (i.e. each variable)
idx_numeric <- sapply(known, is.numeric)
cmatrix <- cor(known[, idx_numeric])
# Look at the correlation matrix
print(cmatrix)
# Install the package "corrplot"
install.packages("corrplot")
library(corrplot)
# Create a plot of your correlation matrix 
corrplot(cmatrix)
# The size of the dots indicates how strong the variables correlate with each other, while the color shows direction
```