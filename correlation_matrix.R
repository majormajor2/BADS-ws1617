### DATA CORRELATIONS (tutorial 3) ###

# Calculate the correlation matrix
# We select the numeric variabels inside of the function by applying function is.numeric to each element of dataframe loans (i.e. each variable)
idx_numeric <- sapply(known, is.numeric)
cmatrix <- cor(known[, idx_numeric])
# Look at the correlation matrix
print(cmatrix)
# Install the package "corrplot"
if(!require("corrplot")) install.packages("corrplot");library(corrplot)
# Create a plot of your correlation matrix 
corrplot(cmatrix)
# The size of the dots indicates how strong the variables correlate with each other, while the color shows direction

# export everything to pdf
dev.off()
pdf(file = paste("correlation matrix.pdf"))
corrplot(cmatrix)
dev.off()