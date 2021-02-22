#Loading the dataset
library(openxlsx)

library("gridExtra")
library(VIM)
library(corrplot)
library(zoo)
library(plotly)
library(gmodels)
library(dplyr)
library(tidyr)
library(factoextra)
library(caret)
library(e1071)
library(skimr)

options(warn=-1)

data_set <- read.xlsx("C:\\Users\\benja\\OneDrive\\Documents\\PI2A5_Credit_scoring\\Credit_risk_analysis\\Dataset.xlsx", 1)


data_set = data_set[1:150000, ]

names(data_set) = c("Index", "dlq", "rev", "Age", "nb30", "dr","mi", "nc", "nb90", "re", "nb60","de")

#Removing the first column "Index"
data_set = subset(data_set, select = -Index)

head(data_set, 10)

#---------------------------------------PART I : Data Wrangling----------------------------------------

summary(data_set)

str(data_set)

#print(skim(data_set))

#print(sapply(data_set, function(x) sum(is.na(x))/length(x)))

#Mean Imputation
data_set$mi = na.aggregate(data_set$mi, FUN = mean)

data_set$de = na.aggregate(data_set$de, FUN = mean)

data_set <- transform(data_set, dlq = as.factor(dlq))

data_set <- transform(data_set, dr = as.numeric(dr))

data_set <- transform(data_set, rev = as.numeric(rev))

#ICI

#-----------------------------------------I.2 OUTLIERS MANAGEMENT----------------------------------------------

#Using the Rule of Thumb (ROT) to avoid Outliers (> Q3 + 1.5 * IQR) for x = Monthly income
outlier_cutoff_income = quantile(data_set$mi, 0.75) + 1.5 * IQR(data_set$mi)

index_outlier_ROT = which(data_set$mi > outlier_cutoff_income)

data_set = delete_outliers(index_outlier_ROT, data_set)

index_outlier_mi_inf = which(data_set$mi < 10)

data_set = delete_outliers(index_outlier_mi_inf, data_set)

outlier_cutoff_Age = quantile(data_set$Age, 0.75) + 1.5 * IQR(data_set$Age)

# Saving the outlier's index to index_highage
index_highage = which(data_set$Age > outlier_cutoff_Age)

# data set new_data with outlier deleted
data_set <- delete_outliers(index_highage, data_set)

index_lowage = which(data_set$Age < 20)

data_set <- delete_outliers(index_lowage, data_set)

#Using the Rule of Thumb (ROT) to avoid Outliers (> Q3 + 1.5 * IQR)
outlier_cutoff_debt_ratio = 1000

index_outlier_debt = which(data_set$dr > outlier_cutoff_debt_ratio)

data_set = delete_outliers(index_outlier_debt, data_set)

#Using the Expert Judgment to avoid errors : debt/ratio 
index_outlier_debt_inf = which(data_set$dr < 0)

data_set = delete_outliers(index_outlier_debt_inf, data_set)

#Using the Expert Judgment to avoid errors : credit revolving > 10 (clipper)
index_outlier_rev_sup = which(data_set$rev > 10)

data_set = delete_outliers(index_outlier_rev_sup, data_set)

#Using the Expert Judgment to avoid errors : credit revolving < 0
index_outlier_rev_inf = which(data_set$rev < 0)

data_set = delete_outliers(index_outlier_rev_inf, data_set)

outlier_cutoff_nc = 42

# Saving the outlier's index to index_highage
index_high_nc = which(data_set$nc > outlier_cutoff_nc)

data_set = delete_outliers(index_high_nc, data_set)

#Using the Expert Judgment
outlier_cutoff_re = 12

# Saving the outlier's index to index_highre
index_high_re = which(data_set$re > outlier_cutoff_re)

data_set <- delete_outliers(index_high_re, data_set)

index_non_entier_de = which(sapply(data_set$de, function(x) check.integer(x)) == FALSE)

data_set <- delete_outliers(index_non_entier_de, data_set)

#Using the Expert Judgment
outlier_cutoff_nb30 = 20 ; outlier_cutoff_nb60 = 20 ; outlier_cutoff_nb90 = 20

# Saving the outlier's index to index_high_nb30
index_high_nb30 = which(data_set$nb30 > outlier_cutoff_nb30)

data_set <- delete_outliers(index_high_nb30, data_set)

index_high_nb60 = which(data_set$nb60 > outlier_cutoff_nb60)

data_set <- delete_outliers(index_high_nb60, data_set)

index_high_nb90 = which(data_set$nb90 > outlier_cutoff_nb90)

data_set <- delete_outliers(index_high_nb90, data_set)

#-------------------------------------Transformation des datas suite : QQ-plot of MonthlyIncome----------------------
monthincome = ggplot (data_set, aes(sample = mi)) 
#monthincome + stat_qq() + stat_qq_line()

monthincome = ggplot (data_set, aes(sample = log(1 + mi)))
#monthincome + stat_qq() + stat_qq_line() + ggtitle("MonthlyIncome density")

#transformation
data_set = mutate(data_set, dr = log(1 + dr)) 

data_set = mutate(data_set, mi = log(1 + mi)) 

#-----------------------------------PART III : Features engineering -------------------------------------------------
# set.seed(567)
# 
# y_new_model = subset(data_set, select = dlq)
# 
# x_new_model = sapply(subset(data_set, select = -dlq), function(x) as.numeric(x))
# 
# x_new_model_scaled = scale(x_new_model, center = TRUE, scale = TRUE)
# 
# data_set = cbind(y_new_model, x_new_model_scaled)

#------------------METHODE 1 : PCA (pas de stepwise fwd)-----------------------------
set.seed(567)

y_new_model = subset(data_set, select = dlq)

x_new_model = sapply(subset(data_set, select = -dlq), function(x) as.numeric(x))

#PCA with factoextra
pca = prcomp(as.data.frame(x_new_model), scale = TRUE, center = TRUE)

summary(pca)

eigen_val = get_eig(pca)

#Counting the relevant Principal Component
counter_comp = count.component(eigen_val$eigenvalue)

data_set = cbind(y_new_model, pca$x[, 1:counter_comp])

#--------------------------- END PCA-------------------------------

#Splitting the data
index_training = sample(1:nrow(data_set), 2/3 * nrow(data_set))

training_set = as.data.frame(data_set[index_training, ])

test_set = as.data.frame(data_set[-index_training, ])


