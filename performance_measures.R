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
  
  
  # Calculate Area Under the Curve with pROC
  auc = as.numeric(roc(response = y, predictor = prediction)$auc)
  
  # print confusion matrix
  class.counts <- misclassCounts(prediction, y); class.counts$conf.matrix
  # print misclassification-based statistic - e.g. error rate
  print(class.counts$metrics, digits = 3)
  
  # Compute the H-measure and other scalar classification performance metrics
  H = HMeasure(y, prediction)
  gini = H$metrics$Gini
  precision = H$metrics$Precision
  TP = H$metrics$TP
  FP = H$metrics$FP
  TN = H$metrics$TN
  FN = H$metrics$FN
  
  # Compute Score
  score <- (TN*3 + FN*(-10))/(TP+FP+TN+FN)
  
  # Calculate ROC
  plotROC(results = H)
  
  return(list(brier_score = brier_score, 
              classification_error = classification_error, 
              area_under_curve = auc,
              gini = gini,
              precision = precision,
              true_positives = TP,
              false_positives = FP,
              true_negatives = TN,
              false_negatives = FN,
              Score = score))
}
