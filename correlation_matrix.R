# Calculate the correlation matrix
# We select the numeric variabels inside of the function by applying function is.numeric to each element of dataframe loans (i.e. each variable)
print_correlation_matrix = function(data, filename = "correlation_matrix.pdf")
{
  idx_numeric = sapply(data, is.numeric)
  cmatrix = cor(known[, idx_numeric])

  # Install the package "corrplot"
  if(!require("corrplot")) install.packages("corrplot");library(corrplot)

  # The size of the dots indicates how strong the variables correlate with each other, 
  # while the color shows direction
  
  # export everything to pdf
  dev.off()
  pdf(file = filename)
  corrplot(cmatrix)
  dev.off()
  return(cmatrix)
}
  