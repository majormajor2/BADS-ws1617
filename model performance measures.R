### Model Performance Measures ###


# Helper function to compute measures of predictive accuracy
predictive_performance = function(y=NULL, prediction=NULL, cutoff=.5) 
{
  # Assumptions:
  # y is a vector of factors
  # prediction is a vector of probability predictions
  
  if (length(y) != length(prediction))
  {
    stop("Data vector and prediction vector must have same length!")
  }
  
  
  
  # Calculate Brier Score
  # y - 1 because levels of factors start at 1 not 0
  brier_score = sum(((as.numeric(y) - 1) - prediction)^2) / length(y)
  
  # Calculate Classification error
  classification = factor(as.numeric(prediction >= cutoff), labels=levels(y)) 
  #classification = factor(as.numeric(prediction >= cutoff), labels=c("negative", "positive")) 
  classification_error = 1 - sum(y==classification) / length(y)
  
  return(list(brier_score = brier_score, classification_error = classification_error))
}
