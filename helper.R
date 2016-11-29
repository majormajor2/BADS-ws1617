# name = "~/Assignment_BADS_WS1617/assignment_BADS_WS1617_known.csv"

get_dataset = function(name) {
  data = read.csv(name, header=T,sep=";")
  
  data$title = factor(data$title, labels=c("no", "yes"))
  data$newsletter = factor(data$newsletter, labels=c("no", "yes"))
  data$coupon = factor(data$coupon, labels=c("no", "yes"))
  data$giftwrapping = factor(data$giftwrapping, labels=c("no", "yes"))
  data$referrer = factor(data$referrer, labels=c("no", "yes"))

  #data$YOB[data$YOB==99] = NA
  #data$YOB_missing = factor(ifelse(is.na(data$YOB), 1, 0), labels=c("no","yes"))
  # trade off between information carried in dummy variables and increase in dimensionality
  #data$YOB[is.na(data$YOB)] = median(data$YOB, na.rm = TRUE)


  return(data)
}
