#-----------Evaluation of a classifier in the case of a binary classification problem---------------------
library(ROCR)

Vector.FP_TP = function (y_true, y_pred)
{
  FP_vec=TP_vec=rep(NA,101)
  i=1
  for(s in seq(0,1,by=.01))
  {
    temp_matrix = confusion_matrix(y_true,y_pred, s)
    FP_vec[i]= specificity(temp_matrix)
    TP_vec[i]= recall(temp_matrix)
    i = i+1
  }
  list = list("FP_vec" = FP_vec, "TP_vec" = TP_vec)
  return (list)
}

Compute_auc = function(FPR, TPR)
{
  AUC = 0
  for(i in 2:length(FPR))
  {
    AUC = AUC + ((FPR[i]-FPR[i-1]) * (TPR[i]+TPR[i-1]))
  }
  
  AUC  = AUC * 0.5
  #AUC = sum(AUC_vec)*0.5
  
  return(AUC)
}

#Receiver Operating Characteristic
list_roc_element = Vector.FP_TP(test_set[, 1], y_predict.logit)

plot(0:1,0:1,xlab="False Positive Rate", ylab="True Positive Rate",cex=.5,  main = "ROC")

segments(0,0,1,1,col="black")

lines(c(1-list_roc_element[["FP_vec"]]),c(list_roc_element[["TP_vec"]]),type="s",col="red")

#Area Under Curve (AUC)
AUC = Compute_auc(list_roc_element[["FP_vec"]], list_roc_element[["TP_vec"]])

#------------------------------------------------Second way--------------------------------------------------

pred = prediction(y_predict.logit, test_set[, 1])
AUC_2 = performance(pred, "auc")

cat(AUC_2@y.values[[1]])

#F-score
F_score = 2 * ((precision.score * recall.score)/ (precision.score + recall.score))

