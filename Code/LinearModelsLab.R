##Want to make predictions and interpretations using lm and glm
#I will use the iris data set already built into R for this example
head(iris)
dim(iris)

#Attach Dataset, so I can just ask for individual columns
attach(iris)

#Look at univariate analysis
par(mfrow=c(2,2))
hist(Sepal.Length) 
hist(Sepal.Width)
hist(Petal.Length)
hist(Petal.Width)
summary(iris)

#Look at bivariate relationships
pairs(iris)

#We could use ggplot2 to look at more advanced elements
#install.packages(ggplot2)
library(ggplot2)
qplot(Sepal.Length, Sepal.Width, col=Petal.Length)

#Instead of using a continuous Petal.Length, we might look at it as a factor
qplot(Sepal.Length, Sepal.Width, col=cut(Petal.Length, breaks=5))
#This is the makings of a nice decision tree example...

#We could look at another set of relationships:
qplot(Petal.Length, Petal.Width, col=Sepal.Length)

#Again we might break up the Sepal.Length to see if there are 'nice' breaks
qplot(Petal.Length, Petal.Width, col=cut(Sepal.Length, breaks=3))

#This again might break down nicely for a decision tree or some kind of cluster analysis
##We could continue with additional plots to fully look at 
##the relationship between all variables


##We could fit a linear model to predict Pedal.Width using the other variables
o = lm(Petal.Width~Petal.Length + Sepal.Length + Sepal.Width + Species, data =iris)
summary(o)

#How do you interpret each slope?
#How do you interpret Rsquared?
#What do all the *** mean everywhere?


##Looks like the model fits really well
##We might check some diagnostics:
##vif, residual plot, nqp of residuals

##Check vifs
library(car)
vif(o) #See any problems?  What should we do?

##Check residals
##Normality
par(mfrow=c(1,1))
qqnorm(resid(o))
qqline(resid(o), col = 2) #See any problems? 

##Constant Variance and linear Model
plot(predict.lm(o), resid(o))
abline(h=0, col=2) #See any problems?







