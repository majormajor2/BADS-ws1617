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
  classification = factor(as.numeric(prediction >= cutoff), levels = c(0,1), labels=levels(y)) 
  #classification = factor(as.numeric(prediction >= cutoff), labels=c("negative", "positive")) 
  classification_error = 1 - sum(y==classification) / length(y)
  
  
  # Calculate Area Under the Curve with pROC
  auc = as.numeric(roc(response = y, predictor = prediction)$auc)
  
  # print confusion matrix
  # class_counts = misclassCounts(classification, y); class_counts$conf.matrix
  # print misclassification-based statistic - e.g. error rate
  # print(class_counts$metrics, digits = 3)
  
  # Compute the H-measure and other scalar classification performance metrics
  H = HMeasure(y, prediction, threshold = cutoff, severity.ratio = 3/10)
  h_measure = H$metrics$H
  gini = H$metrics$Gini
  precision = H$metrics$Precision
  TP = H$metrics$TP
  FP = H$metrics$FP
  TN = H$metrics$TN
  FN = H$metrics$FN
  
  # Compute Average Return per Customer
  score = (3*TN - 10*FN)/(TP+FP+TN+FN)
  
  # Calculate ROC
  plotROC(results = H)
  
  return(list(brier_score = brier_score, 
              classification_error = classification_error, 
              h_measure = h_measure,
              area_under_curve = auc,
              gini = gini,
              precision = precision,
              true_positives = TP,
              false_positives = FP,
              true_negatives = TN,
              false_negatives = FN,
              avg_return = score,
              H = HMeasure))
}
