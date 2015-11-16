##Want to make predictions and interpretations using lm and glm
#I will use the glmlab.txt file for this example

dat = read.table('C://Users//bernharj//Desktop//Classes I Teach//Predictive Analytics - Still To Do//Code//glmlab.txt', header = TRUE)

#We can again look at our data
head(dat)
dim(dat)
summary(dat)
pairs(dat) #Categorical Relationships are Meaningless in this plot - only really looking at using - not columns

#Can fit glm
attach(dat)
mod = glm(cbind(using, notUsing) ~ age + education + wantsMore , family = binomial)
summary(mod)

#Does this model provide adequate fit?
1-pchisq(29.92,10)

#We can also look and see how the pval is computed
x <- seq(0, 40, length=200)
hx <- dchisq(x, 10)
plot(x, hx, type="l", lty=2)
abline(v=29.92, col=2)

#conveniently logistic regression doesnt require the assumptions of linear regression
#however, the coding works similar to linear regression for categorical variables


#interpret the coefficients, find the probability of particular events

#We could try to fit in an interaction to see if we can get a model that fits better, but we will lose 
#interpretability of our terms
o2 = glm(cbind(using, notUsing) ~ age * wantsMore + education, family = binomial)
summary(o2)

#Which variable(s) can still be interpretted nicely?  
#Do you think adding the interaction was a good idea based on this output?

#Does this model fit better?
1-pchisq(12.63,7)

#We can also look and see how the pval is computed
x <- seq(0, 40, length=200)
hx <- dchisq(x, 7)
plot(x, hx, type="l", lty=2)
abline(v=12.37, col=2)

#Can compare our prediction to actual odds ratios
dat$pred =exp(-predict(o2))
dat$pred1 = exp(-predict(mod))
plot(dat$pred, notUsing/(using))
cbind(dat,notUsing/(using))
#Looks like we are predicting well for the most part