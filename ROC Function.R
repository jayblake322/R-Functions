# roc function
rocplot <- function(pred, truth){
  
  predobj <- prediction(pred, truth)
  ROC     <- performance(predobj, "tpr", "fpr")
  plot(ROC)   # Plot the ROC Curve
  auc     <- performance(predobj, measure = "auc")
  auc     <- auc@y.values[[1]]
  return(auc) # Return the Area Under the Curve ROC

}