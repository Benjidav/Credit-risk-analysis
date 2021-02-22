Compute.score = function (y_pred)
{
  vector.score = rep(NA, length(y_pred))
  for (i in 1:length(vector.score))
  {
    vector.score[i] = round(ln(y_pred[i]/(1 - y_pred[i])), 2)
  }
  return (vector.score)
}

confusion_matrix = function(Y, proba, seuil)
{
  
  proba_s=(proba>seuil)*1
  
  FP=sum((proba_s==1)*(Y==0))  
  TP=sum((proba_s==1)*(Y==1))
  TN=sum((proba_s==0)*(Y==0))
  FN=sum((proba_s==0)*(Y==1)) 
  
  return(list("TP"=TP,"TN"=TN,"FP"=FP,"FN"=FN))
}

multi_confusion_matrix = function(Y, pred, class)
{
  FP=sum((pred==class)*(Y!=class))  
  TP=sum((pred==class)*(Y==class))
  FN=sum((pred!=class)*(Y==class)) 
  TN=sum((pred!=class)*(Y!=class))
  
  return(list("TP"=TP,"TN"=TN,"FP"=FP,"FN"=FN))
}

#Metrics computation
accuracy = function(conf_matrix)
{
  
  return((conf_matrix$TN + conf_matrix$TP)/(conf_matrix$TN + conf_matrix$TP +conf_matrix$FN + conf_matrix$FP))
  
}

misclass_rate = function(conf_matrix)
{
  return(1 - accuracy(conf_matrix))
}

specificity = function(conf_matrix)
{
  return(conf_matrix$TN/(conf_matrix$TN + conf_matrix$FP))
}

recall = function(conf_matrix)
{
  return(conf_matrix$TP/(conf_matrix$TP + conf_matrix$FN ))
}

#Sensitivity = Precision
precision = function(conf_matrix)
{
  return(conf_matrix$TP/(conf_matrix$TP + conf_matrix$FP))
} 

#F-score
f1 = function(conf_matrix)
{
  recall_val = recall(conf_matrix)
  precision_val = precision(conf_matrix)
  return(2*(recall_val*precision_val)/(recall_val+precision_val))
}

Compute_prm.score = function (vector.score, Vector.score.classified)
{
  tab_ND = rep(NA, length(vector.score)) ; tab_D = rep(NA, length(vector.score))
  j = 1 ; p = 1
  for (i in 1:length(vector.score))
  {
    if (Vector.score.classified[i] == 1)
    {
      tab_D[j] = vector.score[i]
      j = j + 1
    }
    else 
    {
      tab_ND[p] = vector.score[i]
      p = p + 1
    }
  }
  list = list ("mu_ND" = mean(tab_ND, na.rm = TRUE), "mu_D" = mean(tab_D, na.rm = TRUE), "sigma_ND" = sd(tab_ND, na.rm = TRUE), "sigma_D" = sd(tab_D, na.rm = TRUE))
}

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

