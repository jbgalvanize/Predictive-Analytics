##In this script, we will fit classify using the LDA, QDA, and kNN algorithms

##Install libraries
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

##Take a look at some of the data
par(mfrow=c(2,3))
hist(Lag1, prob=TRUE); hist(Lag2, prob=TRUE); hist(Lag3, prob=TRUE)
hist(Lag4, prob=TRUE); hist(Lag5, prob=TRUE); plot(Direction)
summary(lm(Direction~Lag1)) #Notice Error from response as factor

#Notice how terrible this method fits - why? Think about variable types...
#What predicted values would we get back from this?
summary(lm(as.numeric(Direction)~Lag1)) 
summary(lm(as.numeric(Direction)~Lag2)) 
summary(lm(as.numeric(Direction)~Lag3)) 
summary(lm(as.numeric(Direction)~Lag4)) 
summary(lm(as.numeric(Direction)~Lag5)) 

#Could make predictions based on mlr in the following way
lm_mod = lm(as.numeric(Direction)~Lag1 + Lag2 + Lag3 + Lag4 + Lag5, data=train_dat)

#Notice we have 'up' as 2 and 'down' as 1, so...
lm_pred = ifelse(predict(lm_mod, val_dat)>1.5,"Up","Down")

#Build confusion matrix
table(lm_pred, val_dat$Direction)

#Find mis-classification rate
mean(lm_pred != val_dat$Direction)


#Now we can predict whether a stock will go up in price or not

#Using LDA
lda_mod = lda(Direction~Lag1 + Lag2 + Lag3 + Lag4 + Lag5, data=train_dat)
lda_pred = predict(lda_mod, val_dat)
names(lda_pred)

#Build confusion matrix
table(lda_pred$class, val_dat$Direction)

#Find mis-classification rate
mean(lda_pred$class != val_dat$Direction)

#Using QDA
qda_mod = qda(Direction~Lag1 + Lag2 + Lag3 + Lag4 + Lag5, data=train_dat)
qda_pred = predict(qda_mod, val_dat)
names(qda_pred)

#Build confusion matrix
table(qda_pred$class, val_dat$Direction)

#Find mis-classification rate
mean(qda_pred$class != val_dat$Direction)

#Let's try kNN - which is in the library class
library(class)

#To use knn function need to break off the predictor columns separate
predictors = Smarket[,c(2:6)]

#Still using the same train and test information from up above
train_pred = predictors[train,]
val_pred = predictors[val,]

#The knn function actually takes both the train and validation in the same function ?knn
knn_pred = knn(train_pred, val_pred, Smarket$Direction[train], 3) #train, val, response, how many neighbors
#Notice knn just returns predicted classes - it does not contain any additional info

#Build confusion matrix
table(knn_pred, val_dat$Direction)

#Find mis-classification rate
mean(knn_pred != val_dat$Direction)

#We might also try a logistic regression model
glm_mod = glm(Direction ~ Lag1 + Lag2 + Lag3 + Lag4 + Lag5  , data = train_dat, family = binomial)
glm_probs = predict(glm_mod, val_dat, type = 'response') #here our type will generally be response (it keeps the scale of the response-in this case dichotomous)
glm_pred = ifelse(glm_probs>0.5,"Up","Down")
table(glm_pred, val_dat$Direction)
mean(glm_pred != val_dat$Direction)
