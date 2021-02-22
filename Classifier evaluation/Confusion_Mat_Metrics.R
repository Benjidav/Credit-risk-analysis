#------------------------------CREDIT SCORING------------------------------------------------
library(SciViews)
library(ggplot2)

#Compute the score s(X, Y, ...) = ln côte(lendeur)

#We choose to set s~ = -s such that PD(s~) is a decreasing function 
Vector.score = Compute.score(y_predict.logit)

summary(Vector.score)

#We set s'(X, Y, ..) = (7 + s~)*10
Vector.score_prime = Score.prime(Vector.score)

summary(Vector.score_prime)

#Cut-off at 30% <--> score = 0.85
Vector.score.classified = ifelse(Vector.score_prime < 99, 1, 0)

confusion_matrix.score = multi_confusion_matrix(test_set[, 1], Vector.score.classified, 1)

accuracy.score = accuracy(confusion_matrix.score)

misclassification_rate.score = misclass_rate(confusion_matrix.score)

specificity.score = specificity(confusion_matrix.score)

recall.score= recall(confusion_matrix.score)

precision.score = precision(confusion_matrix.score)

f1.score = f1(confusion_matrix.score)

#Computation of the parameters of the two r.v S(ND) & S(D)

prm_score <- Compute_prm.score(Vector.score_prime, Vector.score.classified)

curve(dnorm(x, prm_score[["mu_ND"]], prm_score[["sigma_ND"]]), xlim = c(0,150), ylab = "Densité de probabilité", xlab = "Score", col = "darkgreen")

curve(dnorm(x, prm_score[["mu_D"]],prm_score[["sigma_D"]]), ylab = "Densité de probabilité",xlim = c(0,150), xlab = "Score", col = "red", add = TRUE)

#Unconditional probability
p = sum(Vector.score.classified==1)/ (sum(Vector.score.classified==0) + sum(Vector.score.classified==1))

#f(s) = p * fD + (1 - p) * fND
curve(p*dnorm(x, prm_score[["mu_D"]],prm_score[["sigma_D"]]) + (1 - p)*dnorm(x, prm_score[["mu_ND"]], prm_score[["sigma_ND"]]), ylab = "Densité de probabilité",xlim = c(50,150), xlab = "Score", col = "blue")

      