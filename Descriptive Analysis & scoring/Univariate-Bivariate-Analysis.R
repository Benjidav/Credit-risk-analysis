#--------------------------------------Partie II Exploration des datas-----------------------------

#-------------------------Analyse descriptive-------------------------------------

#Variable catégorielle seulement
CrossTable(data_set$dlq)

fig <- plot_ly(data_set,
               x=~dlq,
               type="histogram",
               histnorm = "probability") %>% 
  layout(title = "Histogram - SeriousDlqin2yrs")

fig

#Boxplots (va quantitative)
descriptive.analysis = function (n) 
{
  ggplot(data_set, aes(x=data_set[, n]), fill=data_set[, n]) +
    geom_boxplot() +
    coord_flip() +
    ggtitle(paste0(n)) + 
    labs(x = "" , y = "") + 
   theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"))
}

boxList = list()


boxList[[1]] = descriptive.analysis("mi") ; boxList[[2]] = descriptive.analysis("Age")
boxList[[3]] = descriptive.analysis("rev") ; boxList[[4]] = descriptive.analysis("dr")
boxList[[5]] = descriptive.analysis("de") ; boxList[[6]] = descriptive.analysis("nc")
boxList[[7]] = descriptive.analysis("re") ; boxList[[8]] = descriptive.analysis("nb30")
boxList[[9]] = descriptive.analysis("nb60") ; boxList[[10]] = descriptive.analysis("nb90")

cowplot::plot_grid(plotlist = boxList, ncol = 2)

#Histogrames
hist.function = function (n)
{
  ggplot(data_set) +
    aes(x = data_set[, n]) +
    geom_histogram(fill ="orange", colour = "black", binwidth = 0.25, bins = 12) +
    ggtitle(n) +
    xlab("") +
    ylab("") +
  theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"))
}

histList = list()

histList[[1]] = hist.function("mi") ; histList[[2]] = hist.function("Age")
histList[[3]] = hist.function("rev") ; histList[[4]] = hist.function("dr")
histList[[5]] = hist.function("de") ; histList[[6]] = hist.function("nc")
histList[[7]] = hist.function("re") ; histList[[8]] = hist.function("nb30")
histList[[9]] = hist.function("nb60") ; histList[[10]] = hist.function("nb90")

cowplot::plot_grid(plotlist = histList, ncol = 2)

#--------------------------Analyse univariée--------------------------------------
# par(mfrow = c(3, 3))
# 
# print(summary(data_set$Age))
# 
# print(summary(data_set$dr))
# 
# print(table(data_set$nb30))
# 
# print(table(data_set$nb60))
# 
# print(table(data_set$nb90))
# 
# print(table(data_set$de))
# 
# print(table(data_set$nc))

correlation <- cor(as.numeric(data_set[, 1]) + data_set[,-1])
corrplot(correlation, method="circle", tl.cex = 0.5, tl.srt = 45)

#Le graphique de corrélation montre que les colonnes age, NumberOfTime30_59DaysPastDueNotWorse, NumberOfTime60_89DaysPastDueNotWorse et NumberOfTimes90DaysLate sont celles pour lesquelles la corrélation avec la variable cible est la plus importante.

plotList <- list()
bivariate.analysis = function (n) 
{
  res <- ggplot(data_set, aes(x=dlq, y = data_set[,n], fill=dlq)) +
    geom_boxplot() +
    coord_flip() +
    ggtitle(paste0(n, " vs SeriousDlqin2yrs")) + 
    labs(x = "" , y = "") + 
    theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"))
  return (res)
}

plotList[[1]] = bivariate.analysis("mi") ; plotList[[2]] = bivariate.analysis("Age")
plotList[[3]] = bivariate.analysis("rev") ; plotList[[4]] = bivariate.analysis("dr")
plotList[[5]] = bivariate.analysis("de") ; plotList[[6]] = bivariate.analysis("nc")
plotList[[7]] = bivariate.analysis("re") ; plotList[[8]] = bivariate.analysis("nb30")
plotList[[9]] = bivariate.analysis("nb60") ; plotList[[10]] = bivariate.analysis("nb90")

cowplot::plot_grid(plotlist = plotList, ncol = 2)

#Les informations qui ressortent de ces boxplots sont attendus et paraissent sensées : les clients sains sont légèrement plus agés, ont un revenu plus important, un ratio de dette moins important et moins de retard de paiement que les clients qui font défaut.


# densités (variables continues obligatoires) la fonction featurePlot vient de la library "caret"
featurePlot(x=data_set[c("rev", "Age", "dr", "mi")],
            y=data_set[,"dlq"], plot="density",
            scales=list(x=list(relation="free"), y=list(relation="free")))

#On devine que la courbe rose représente les contrats en défaut et la courbe bleue les contrats sains. Les courbes ont un overlap important ce qui montre qu’il serait difficile de prédire à partir d’une seule variable si le contrat est en défaut ou non. Cependant on retrouve le même comportement que pour les boxplots (les clients sains sont plus agés, avec des revenus plus importants, un ratio de dette plus faible et une pourcentage d’endettement par rapport à la valeur du prêt moindre).
#A noter que l’on aurait pu utiliser la fonction featurePlot pour tracer des boxPlot en mettant l’option plot=“box” (d’autre options de visualisation existent avec cette fonction featurePlot).

#Uniquement pour des va continues
violon_density = function (n)
{
  ggplot(data_set) +
    aes(x = dlq, y = data_set[, n], fill = dlq) +
    geom_violin() +
    xlab("SeriousDlqin2yrs") +
    ylab(n) + 
    ggtitle(paste0(n, " vs SeriousDlqin2yrs")) +
    theme(plot.title = element_text(hjust = 0.5, size = 15, face = "bold"))
}

violon_densList <- list()

violon_densList[[1]] = violon_density("mi") ; violon_densList[[2]] = violon_density("Age")
violon_densList[[3]] = violon_density("rev") ; violon_densList[[4]] = violon_density("dr")

cowplot::plot_grid(plotlist = violon_densList, ncol = 2)
