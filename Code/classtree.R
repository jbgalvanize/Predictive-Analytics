##In this module, we will fit a classification tree using R
##Install packages to use
set.seed(1)
library(tree)
library(ISLR)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(C50)

#Attach our dataset
attach(Carseats)

#Quick look - in a real analysis, you would want to do more than this
#A lot more - the aim is to show you how classification works - not 
#carry out a full analysis
head(Carseats)
dim(Carseats)
str(Carseats)
?Carseats

#Consider we want to predict whether a company will have low/medium/high sales
#Currently sales are in dollars, so we will change to a category
Carseats$Sales1 = cut(Sales,3) #Create 3 categories for sales
levels(Carseats$Sales1) = c("low", "med", "high") #Rename the levels of sales
head(Carseats) #Check out new column


#Split the data into training and validation
train = sample(1:nrow(Carseats), nrow(Carseats)/2) #what numbers, how many
val = -train #create validation as the non-train data pts

train_dat = Carseats[train,] #Choosing the rows of the dataset out
val_dat = Carseats[val,] #Again choosing the rows out of the dataset

##Fit A Decison Tree
mod = tree(Sales1~CompPrice+Income+Advertising+Population+Price+ShelveLoc+Age+Education+Urban+US, train_dat) #create tree using all of the predictors -excluding 1st and last column
mod #You can see how many observations go into each part of the split
par(mfrow=c(1,1))
plot(mod) #Plot the tree model
text(mod, pretty=0) #Add text for the tree model
summary(mod) #Summary that corresponds to tree

#Show the best option for pruning/trimming our decision tree based on cv 
cv_mod = cv.tree(mod, FUN=prune.misclass) #Prune to minimize mis-classification based on cv
names(cv_mod)
plot(cv_mod$size,cv_mod$dev, type='b') #The minimum error rate occurs at 9
cv_mod$size[which.min(cv_mod$dev)] #Number of groups with the minimum deviance

##Therefore, there is no use to trimming/pruning our tree
##If we wanted to prune - we could, but provided the cv results, we probably wouldnt want to
prune_mod = prune.tree(mod, best = cv_mod$size[which.min(cv_mod$dev)]) #Use prune.tree where there are only 4 final predictions
plot(prune_mod)
text(prune_mod)

#We can also use the rpart library to fit a decision tree to compare
mod1 = rpart(Sales1~CompPrice+Income+Advertising+Population+Price+ShelveLoc+Age+Education+Urban+US, train_dat) #create tree using all of the predictors -excluding 1st and last column
mod #You can see how many observations go into each part of the split
rpart.plot(mod1, type=3, extra=101,fallen.leaves=TRUE)

#We can also try using the C50 library
c50_mod = C5.0(train_dat[,c(2:11)], train_dat$Sales1)
summary(c50_mod)

#We could choose a particular model based on how well it does on the 
#Validation set of data.  We can see how well each model classifies on the new set

#Obtain predictions for each model
pred_prune_mod = predict(prune_mod, val_dat, type = "class")
pred_rpart_mod = predict(mod1, val_dat, type = "class")
pred_c50_mod = predict(c50_mod, val_dat, type = "class")
pred_mod = predict(mod, val_dat, type = "class")


#Obtain mis-classification rates and confusion matrix
#confusion matrix
table(pred_prune_mod, val_dat$Sales1)
table(pred_rpart_mod, val_dat$Sales1)
table(pred_c50_mod, val_dat$Sales1)
table(pred_mod, val_dat$Sales1)

#mis-classification rates
mean(pred_prune_mod != val_dat$Sales1)
mean(pred_rpart_mod != val_dat$Sales1)
mean(pred_c50_mod != val_dat$Sales1)
mean(pred_mod != val_dat$Sales1)

