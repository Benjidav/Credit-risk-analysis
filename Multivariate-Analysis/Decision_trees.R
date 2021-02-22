library(rpart)
Random.under.sampling = function (training_set, target, p)
{
  set.seed(123)
  index_default = which(training_set[, target] == 1)
  index_ND = which(training_set[, target] == 0)
  
  nb_ND_target = floor((length(index_default) * (1 - p))/p)
  
  index_rm_ND = sample(index_ND, size = (length(index_ND) - nb_ND_target), replace = FALSE)
  
  undersampled_training_set = as.data.frame(training_set[-index_rm_ND, ])
  
  return (undersampled_training_set)
}

# 1. Undersampling to overcome unbalance data
undersampled_training_set = Random.under.sampling(training_set, target = "dlq", p = 1/3)

tree_undersample = rpart(dlq ~. , method = "class", data = undersampled_training_set, 
                         control = rpart.control(cp = 0.001)) 

plot(tree_undersample, uniform = TRUE)

text(tree_undersample)

# 2. Changing the prior probabilities

tree_prior = rpart(dlq ~., method = "class", data = training_set, 
                   parms = list(prior = c(0.7, 0.3)), control = rpart.control(cp = 0.001))

plot(tree_prior, uniform = TRUE)

text(tree_prior)

# 3. Loss matrix

tree_loss_matrix = rpart(dlq ~., method = "class", data = training_set, 
                         parms = list(loss = matrix(c(0, 10, 1, 0), ncol = 2)), control = rpart.control(cp = 0.001))

plot(tree_loss_matrix, uniform = TRUE)

text(tree_loss_matrix)

