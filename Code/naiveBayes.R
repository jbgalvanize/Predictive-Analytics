#Let's use the naiveBayes algorithm to classify
#Install libraries
library(e1071)
library(MAsS)
library(ISLR)

##Attach and create our training and test data sets
attach(Smarket)
head(Smarket)

#We will use a model fit on earlier dates to predict later dates
train = (Year<2005)
val = !train  
train_dat = Smarket[train,]
val_dat = Smarket[val,]

#Fit naiveBayes method
nb_mod = naiveBayes(Direction~Lag1 + Lag2 + Lag3 + Lag4 + Lag5, data=train_dat)
nb_pred = predict(nb_mod, val_dat, type = 'class')

#Build confusion matrix
table(nb_pred, val_dat$Direction)

#Find mis-classification rate
mean(nb_pred != val_dat$Direction)
