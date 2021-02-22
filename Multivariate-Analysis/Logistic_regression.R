#Analyse multivariate

#------------------METHODE 1 : Logistic regression with glm (Logit) from PCA results -----------------

model.classification.logit <- glm(formula = dlq ~ . , family = binomial(link = "logit"), data = training_set )

#------------------METHODE 2 : Stepwise Fwd (pas de PCA)-----------------------------
# 
# #STEPWISE REGRESSION
# 
# model.null <- glm(formula = dlq ~ 1, family = "binomial", data = training_set)
# 
# stepwise <- step(model.null, scope = list(upper=model.classification.logit), direction="forward", test="Chisq", data = training_set)
# 
# print(summary(stepwise))
# 
# model.classification.logit <- glm(formula = dlq ~ nb30 + nb60 + nb90 + rev + Age + nc + re + de + dr, family = binomial(link = "logit"), data = training_set )
#------------------END STEPWISE-----------------------------------------------------

summary(model.classification.logit)

y_predict.logit = predict(model.classification.logit, newdata = subset(test_set, select = -dlq), type = "response")

#Low range is bad (low difference between customers)
range(y_predict.logit)

class_prediction.logit = ifelse(y_predict.logit > 0.50,"1","0")

# Creating confusion matrix
confu_matrix.logit = confusionMatrix(as.factor(class_prediction.logit),as.factor(test_set[, 1] ))

confusion_matrix.logit_v2 = multi_confusion_matrix(test_set[, 1], class_prediction.logit, 1)

f1.score_logit = f1(confusion_matrix.logit_v2)

# Computing classification accuracy
accuracy.logit = confu_matrix.logit$overall[1]

# Computing sensitivity
sensitivity.logit = confu_matrix.logit$byClass[1]

# Computing specificity
specificity.logit = confu_matrix.logit$byClass[2]

#--------------------------------Methode 2 :  Reg Logistique -------------------------

credit.reglog <- glm(dlq~., training_set, family = "binomial")

credit.y_pred.reglog <- predict(credit.reglog, test_set[-1], type = "response")

print(credit.reglog)

credit.seuil.reglog = 0.5

credit.cm.reglog <- confusionMatrix(data = as.factor(as.numeric(credit.y_pred.reglog>credit.seuil.reglog)),
                                    reference = test_set$dlq,
                                    positive="1")
print(credit.cm.reglog$overall)

print(credit.cm.reglog$table)

print(credit.cm.reglog$byClass)

#F1 score : credit.cm.reglog$byClass[7]

#--------------------ETUDE DES DENSITES DE DEFAUT ET ND de la Reg Logistique -----------------------------------

credit.df.reglog = data.frame(y_pred=credit.y_pred.reglog, y_true=test_set$dlq)

ggplot(credit.df.reglog, aes(y_pred, color=y_true)) +
  geom_density(size = 1) +
  ggtitle("Scores prédits pour le test set pour la Reg logistique") +
  scale_x_continuous(breaks=seq(0,1,0.05))

#Le seuil optimial semble etre 0.07

credit.seuil.reglog = 0.07

credit.cm.reglog <- confusionMatrix(data = as.factor(as.numeric(credit.y_pred.reglog>credit.seuil.reglog)),
                                    reference = test_set$dlq,
                                    positive="1")
print(credit.cm.reglog$overall)

print(credit.cm.reglog$table)

print(credit.cm.reglog$byClass)

credit.cm.reglog$byClass[7]

#---------------------ROC ET AUC----------------------------------------------

credit.rocr.pred.reglog = prediction(credit.y_pred.reglog, test_set$dlq)
credit.rocr.perf.reglog = performance(credit.rocr.pred.reglog, "tpr","fpr")
plot(credit.rocr.perf.reglog, colorize=TRUE)

credit.rocr.auc.reglog = performance(credit.rocr.pred.reglog, "auc")
print(credit.rocr.auc.reglog@y.values[[1]])

#----------------------------------------Exporting the training_set-------------------------------------

write.xlsx(x = training_set, "C:\\Users\\benja\\OneDrive\\Documents\\Credit-scoring-Model\\Training.xlsx")

