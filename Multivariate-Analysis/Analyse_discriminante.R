#-----------------------------------------GDA----------------------------------

credit.lda <- lda(dlq~., training_set)
print(credit.lda)

credit.qda <- qda(dlq~., training_set)
print(credit.qda)

credit.y_pred.lda <- credit.lda %>% predict(test_set[-1])
credit.y_pred.lda <- credit.y_pred.lda$posterior[,2] # on veut récupérer la probabilité d'appartenir à la classe 1

credit.y_pred.qda <- credit.qda %>% predict(test_set[-1])
credit.y_pred.qda <- credit.y_pred.qda$posterior[,2] # on veut récupérer la probabilité d'appartenir à la classe 1

credit.seuil.lda = 0.5
credit.seuil.qda = 0.5

#LDA 
credit.cm.lda <- confusionMatrix(data = as.factor(as.numeric(credit.y_pred.lda > credit.seuil.lda)),
                                 reference = test_set$dlq,
                                 positive="1")
print(credit.cm.lda$overall)

print(credit.cm.lda$table)

print(credit.cm.lda$byClass)

#QDA
credit.cm.qda <- confusionMatrix(data = as.factor(as.numeric(credit.y_pred.qda > credit.seuil.qda)),
                                 reference = test_set$dlq,
                                 positive="1")
print(credit.cm.qda$overall)

print(credit.cm.qda$table)

print(credit.cm.qda$byClass)

#------------------------------------ROC ET AUC---------------------------------------

#LDA
credit.rocr.pred.lda = prediction(credit.y_pred.lda, test_std$SeriousDlqin2yrs)
credit.rocr.perf.lda = performance(credit.rocr.pred.lda,"tpr","fpr")

plot(credit.rocr.perf.lda, colorize=TRUE)

credit.rocr.auc.lda = performance(credit.rocr.pred.lda, "auc")
print(credit.rocr.auc.lda@y.values[[1]])

#QDA
credit.rocr.pred.qda = prediction(credit.y_pred.qda, test_std$SeriousDlqin2yrs)
credit.rocr.perf.qda = performance(credit.rocr.pred.qda,"tpr","fpr")
plot(credit.rocr.perf.qda, colorize=TRUE)

credit.rocr.auc.qda = performance(credit.rocr.pred.qda, "auc")
print(credit.rocr.auc.qda@y.values[[1]])
