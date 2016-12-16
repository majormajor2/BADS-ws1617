#name = "~/Assignment_BADS_WS1617/assignment_BADS_WS1617_known.csv"

get_dataset = function(name) {
  data = read.csv(name, header=T,sep=",")
  

  for(header in list(data$title,data$newsletter,data$coupon,data$giftwrapping,data$referrer,data$cost_shipping))
      {
      header = factor(header, labels=c("no","yes"))
      }

  #data$YOB[data$YOB==99] = NA
  #data$YOB_missing = factor(ifelse(is.na(data$YOB), 1, 0), labels=c("no","yes"))
  # trade off between information carried in dummy variables and increase in dimensionality
  #data$YOB[is.na(data$YOB)] = median(data$YOB, na.rm = TRUE)


  return(data)
}
