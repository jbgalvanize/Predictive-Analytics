#Affinity Analysis in R
#https://en.wikipedia.org/wiki/Lift_(data_mining)
#http://www.salemmarafi.com/code/market-basket-analysis-with-r/comment-page-1/
#http://www.r-bloggers.com/data-frames-and-transactions/ - Talks about the data type

#libraries and data
library(arules)
library(arulesViz)
library(datasets)

#Notice the data we are going to use is in a different structure than we have ever worked with
#Therefore, the functions we use are bit different too.
data(Groceries) #attach doesnt work here
str(Groceries)
?Groceries
# Could look at all itmes:
#inspect(Groceries)
itemFrequencyPlot(Groceries, topN=20, type='absolute')

#Look at associations - only take those with more than .0005 support and .8 confidence
ba = apriori(Groceries, parameter = list(supp = .0005, conf = .8))
options(digits = 2)
inspect(ba[1:10])
summary(ba)

#We could write our rules to a csv file to take a better look
#We also may want to sort via our different criteria
write(ba, file="rules.csv", sep=",", col.names=NA)

#We could sort within R using:
ba = sort(ba, by = "lift", decreasing=TRUE)
inspect(ba[1:10])
#You can see this matches what we have in excel


#We could ask, what items are indicative of purchasing sugar?
rules = apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.2), 
               appearance = list(default="lhs",rhs="sugar"),
               control = list(verbose=F))

#I will sort them again by lift
rules=sort(rules, decreasing=TRUE,by="lift")
inspect(rules[1:10])

#I could also sort by confidence
rules=sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:10])
##In this case there wasn't a difference between lift and confidence results

#If someone buys sugar, what might they also buy?
rules = apriori(data=Groceries, parameter=list(supp=0.001,conf = 0.15, minlen=2), 
               appearance = list(default="rhs",lhs="sugar"),
               control = list(verbose=T))

#I could again sort by confidence
#There are only 8 rules
summary(rules)
rules = sort(rules, decreasing=TRUE,by="confidence")
inspect(rules[1:8])


