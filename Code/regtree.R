##install library and set the seed so our data splits are the same
set.seed(1)
library(MASS)
library(tree)
library(ISLR)
library(ggplot2)


##Want to make predictions and interpretations using decision trees
#I will use the Boston file built in

attach(Boston)

#Take a Look at the Data
#You would want to look at more than this with a real world problem
?Boston
head(Boston)
dim(Boston)
str(Boston)
par(mfrow=c(3,3))
hist(medv);hist(lstat);hist(black)
hist(crim);hist(age);hist(tax)
hist(rm);hist(ptratio);hist(nox)

qplot(age,medv);qplot(lstat,medv);qplot(crim,medv)
qplot(rm,medv);qplot(ptratio,medv);qplot(nox,medv)
qplot(zn,medv);qplot(black,medv);qplot(tax,medv)


#Split the data into training and validation
train = sample(1:nrow(Boston), nrow(Boston)/2) #what numbers, how many
val = -train #create validation as the non-train data pts

train_dat = Boston[train,] #Choosing the rows of the dataset out
val_dat = Boston[val,] #Again choosing the rows out of the dataset

##Fit A Decison Tree
mod = tree(medv~., train_dat) #create tree using all of the predictors
mod #You can see how many observations go into each part of the split
plot(mod) #Plot the tree model
text(mod, pretty=0) #Add text for the tree model
summary(mod) #Summary that corresponds to tree


#We could predict for our validation set to see how well we do
pred_val = predict(mod, val_dat)
mean((pred_val-val_dat$medv)^2)

#Plot predicted vs actual or plot the 
plot(pred_val, val_dat$medv)
plot(pred_val,(pred_val-val_dat$medv))
abline(h=0, col=2)

#We could find out if we need to trim the tree by using cross-validation
cv_mod = cv.tree(mod) #default is 10 fold, you can change the number of folds using k=(number) as an argument
names(cv_mod)

#Here we can make a plot to see how many groups we should have to minimize the Deviance (MSE)
plot(cv_mod$size, cv_mod$dev, type = 'b') 
cv_mod$size[which.min(cv_mod$dev)] #We should have 8 groups, because this number of categories has the minimum deviance

##Therefore, there is no use to trimming/pruning our tree
##If we wanted to prune - we could, but provided the cv results, we probably wouldnt want to
prune_mod = prune.tree(mod, best = 4) #Use prune.tree where there are only 4 final predictions
plot(prune_mod)
text(prune_mod)
