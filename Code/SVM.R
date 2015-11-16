##SVM Example
##http://cbio.ensmp.fr/~jvert/svn/tutorials/practical/svmbasic/svmbasic_notes.pdf
##https://www.youtube.com/watch?v=iu9rrCbSqgM

#Install Library
library(e1071)
library(MAsS)
library(ISLR)

#Create dataset
set.seed(1)
x=matrix(rnorm(40),ncol=2)
y=c(rep(-1,10),rep(1,10))
x[y==1,] = x[y==1,] + 1 #Takes the last 10 rows for both columns and add 1

d = data.frame(x,y=as.factor(y))
plot(x,col=(5-y)) #A look at our data that we hope to divide

#linear divider
svfit = svm(y~., data = d, kernel = 'linear', cost = 10, scale = FALSE) #Can tweak cost function, Can scale the data if you would like to standardize (if not in same units)
plot(svfit,d, ylim=c(-2,3), xlim=c(-2.3,2.5))

#Polynomial divider
svfit = svm(y~., data = d, kernel = 'polynomial', cost = 10, scale = FALSE) #Can tweak cost function, Can scale the data if you would like to standardize (if not in same units)
plot(svfit,d, ylim=c(-2,3), xlim=c(-2.3,2.5))

#Something called a sigmoid divider - doesnt look like a great choice
svfit = svm(y~., data = d, kernel = 'sigmoid', cost = 10, scale = FALSE) #Can tweak cost function, Can scale the data if you would like to standardize (if not in same units)
plot(svfit,d, ylim=c(-2,3), xlim=c(-2.3,2.5))

#We can use cross-validation to find a model that works best
cvmods = tune(svm,y~., data = d, kernel = 'linear', ranges = list(cost = c(.001,.01,.05, .1,1,5,10)))
summary(cvmods) #Using 10-fold cv.  This suggests we should use a cost function of .05
?tune #This is a very useful function for cv

cvmods = tune(svm,y~., data = d, kernel = 'polynomial', ranges = list(cost = c(.001,.01,.05, .1,1,5,10)))
summary(cvmods) #Using 10-fold cv.  This suggests we should use a cost function of .05
?tune #This is a very useful function for cv


##We could use svm on the infamous iris dataset
##Attach and create our training and test data sets
attach(iris)
head(iris)

#Split our data
train = sample(c(1:150), 75, replace=FALSE)
train_dat = iris[train,]
val_dat = iris[-train,]

#Fitting using svm
svm_mod = svm(Species~., data = train_dat, kernel = 'polynomial', cost = 10, scale = FALSE)

#Then predict using svm
svm_pred = predict(svm_mod, val_dat)
table(svm_pred, val_dat$Species)


##We could compare to an alternative method - Naive Bayes
nb_mod = naiveBayes(Species~., data=train_dat)
nb_pred = predict(nb_mod, val_dat)
table(nb_pred, val_dat$Species)

##How about that...both do pretty well.
