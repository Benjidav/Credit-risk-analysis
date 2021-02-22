#--------------------------Principal function of the model-----------------------------

#Quicksort
quicksort_1D = function (A,i,j) 
{
  if (j>i)
  {
    pivot = A[i]
    left=i
    right=j
    
    while (left <= right) 
    {
      while (A[left] < pivot) 
      {
        left=left+1
      }
      while (A[right] > pivot) 
      {
        right=right-1
      }
      if (left <= right)
      {
        temp=A[left]
        A[left]=A[right]
        A[right]=temp
        left=left+1
        right=right-1
      }
    }
    A = quicksort(A,i,right)
    A = quicksort(A,left,j)
  }
  
  return (A)
}

order_other_column = function (dataframe_sorted, dataframe)
{
  dataframe_res = dataframe_sorted
  for (i in 1:nrow(dataframe_res))
  {
    for (j in 1:nrow(dataframe))
    {
      if (round(as.numeric(dataframe_res[i, 2]), 2) == round(as.numeric(dataframe[j, 2]), 2))
      {
        #Allocation of Customer
        dataframe_res[i, 1] = dataframe[j, 1]
        #Allocation of Prediction
        dataframe_res[i, 3] = dataframe[j, 3]
      }
    }
  }
  return (dataframe_res)
}

#Loading the training set
import_training_set = function (file)
{
  training_set_imported = read.xlsx(file, 1)
  return(training_set) 
}

fit_model = function (training)
{
  set.seed(123)
  logistic_reg = model.classification.probit <- glm(dlq ~ . , family = binomial(link = "logit"), data = training)
  return (logistic_reg)
}


file_to_tab = function (file_path)
{
  file = read.xlsx(file_path, 1)
  
  file = na.omit(file)
  
  Customer = file[, 1]
  
  return (Customer)
}

#Processing of the test set
test_set.process = function(file_path, counter_comp)
{
  file = read.xlsx(file_path, 1)
  
  file = drop_na(file)
  
  x_name = file[, 1]
  
  file = sapply(file[, -1], function(x) as.numeric(x))
  
  pca_model = prcomp(as.data.frame(file), scale = TRUE)
  
  eigen_val_model = get_eig(pca_model)
  
  #Counting the relevant Principal Components : Same as the trained model --> Mandatory
  
  counter_comp_model = counter_comp
  
  full.pca_model = pca_model$x[, 1:counter_comp_model]
  
  test_set_model = as.data.frame(full.pca_model)
  
  return (test_set_model)
  
}

#Vector of predicted probabilities
Model.y_predict_logit = function (data, logistic_reg)
{
  prediction_y = predict (logistic_reg, newdata = data, type = "response")
  return (prediction_y)
}

Model.classification_logit = function (test_set_model, cutoff_proba, logistic_reg)
{
  prediction_y = predict (logistic_reg, newdata = test_set_model, type = "response")
  class_prediction.logit_model = ifelse(prediction_y > cutoff_proba, 1, 0)
  return (class_prediction.logit_model)
}

Model.score_prime = function (prediction_y)
{
  #We choose to set s~ = -s such that PD(s~) is a decreasing function 
  scores_vector = Compute.score(prediction_y)
  
  #We set s'(X, Y, ..) = (7 + s~)*10
  scores.prime.vector = Score.prime(scores_vector)
  
  #Mandatory to return a dataframe in the server side
  return (scores.prime.vector)
}

Model.score_classified = function (scores.prime.vector, cut)
{
  scores_class = ifelse(scores.prime.vector < cut, 1, 0)
  return (scores_class)
}

binary_to_letter = function (score_classe_binaire)
{
  vector = rep(NA, length(score_classe_binaire))
  for (i in 1:length(score_classe_binaire))
  {
    if (score_classe_binaire[i] == 1)
    {
      vector [i] = "B"
    }
    else 
    {
      if(score_classe_binaire[i] == 0)
      {
        vector [i] = "G"
      }
    }
  }
  return (vector)
}

score_card = function ()
{
  Score = c(140,120,100,80,60,40,20)
  Description = c("Excellent", "Tres bon", "Bon", "Moyen", "Faible", "Tres faible", "Mediocre")
  return (cbind(Score, Description))
}

Score.prime = function(vector.score)
{
  score_prime = rep(NA, length(vector.score))
  for(i in 1:length(score_prime))
  {
    score_prime [i] = (7 - vector.score[i])*10
  }
  return (score_prime)
}

delete_outliers = function (index, data)
{
  data_return = data
  if (length(index) > 0)
  {
    data_return = data[-index, ]
  }
  else 
  {
    data_return = data
  }
  return (data_return)
}

#Kaiser-Criteria : Only Principal Component with lambda greater than 1 are stored
count.component = function (eigen)
{
  counter = 0
  for (i in  1 : length(eigen)) 
  {
    if (round(eigen[i], 2) > 1)
    {
      counter = counter + 1
    }
  }
  return (counter)
}
