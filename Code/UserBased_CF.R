##Example taken from http://www.salemmarafi.com/code/collaborative-filtering-r/
##Read in the data
data.germany = read.csv(file.choose()) #cfdata.csv
dim(data.germany) #1 where an individual listened, and 0 where someone didn't listen
summary(data.germany[,3]) #listening to acdc
summary(data.germany[,283]) #Listening to u2
names(data.germany)

############################
# User Scores Matrix       #
############################    
# Process:
# Choose a product, see if the user purchased a product
# Get the similarities of that product's top 10 neighbours
# Get the purchase record of that user of the top 10 neighbours
# Do the formula: sumproduct(purchaseHistory, similarities)/sum(similarities)

# Lets make a helper function to calculate the scores
getScore <- function(history, similarities)
{
  x <- sum(history*similarities)/sum(similarities)
  x
}
# Create a helper function to calculate the cosine between two vectors - same as correlation
getCosine = function(x,y) 
{
  this.cosine = sum(x*y) / (sqrt(sum(x*x)) * sqrt(sum(y*y)))
  return(this.cosine)
}

# A placeholder matrix - this is a matrix of NAs to hold the pairwise correlations of each song(each song is an item)
holder <- matrix(NA, nrow=ncol(data.germany.ibs),ncol=ncol(data.germany.ibs),dimnames=list(colnames(data.germany.ibs),colnames(data.germany.ibs)))
data.germany.ibs.similarity <- as.data.frame(holder)
head(data.germany.ibs.similarity)
dim(data.germany.ibs.similarity) #285 songs to obtain correlations between

# Lets fill in those empty spaces with cosine similarities - 
#we are looking at two columns: i and j, then computing the correlation to put in the holder matrix
for(i in 1:ncol(data.germany.ibs)) {
  for(j in 1:ncol(data.germany.ibs)) {
    data.germany.ibs.similarity[i,j]= getCosine(data.germany.ibs[i],data.germany.ibs[j])
  }
}

# Drop the user column and make a new data frame
data.germany.ibs = (data.germany[,!(names(data.germany) %in% c("user"))])

# A placeholder matrix similar to before
holder <- matrix(NA, nrow=nrow(data.germany),ncol=ncol(data.germany)-1,dimnames=list((data.germany$user),colnames(data.germany[-1])))
i=1
j=2
# Loop through the users (rows)
for(i in 1:nrow(holder)) 
{
  # Loops through the products (columns)
  for(j in 1:ncol(holder)) 
  {
    # Get the user's name and the product's name
    # We do this not to conform with vectors sorted differently 
    user <- rownames(holder)[i] #This is just a user number - that is unique to each user
    product <- colnames(holder)[j]
    
    # We do not want to recommend products you have already consumed
    # If you have already consumed it, we store an empty string
    # The 
    if(as.integer(data.germany[data.germany$user==user,product]) == 1)
    { 
      holder[i,j]<-""
    } else {
      
      # We first have to get a product's top 10 neighbours sorted by similarity
      # We take the top 11 because the first will always be itself
      topN<-((head(n=11,(data.germany.ibs.similarity[order(data.germany.ibs.similarity[,product],decreasing=TRUE),][product]))))
      topN.names <- as.character(rownames(topN))
      topN.similarities <- as.numeric(topN[,1])
      
      # Drop the first one because it will always be the same song
      topN.similarities<-topN.similarities[-1]
      topN.names<-topN.names[-1]
      
      # We then get every (all) user's purchase history for those 10 items
      topN.purchases<- data.germany[,c("user",topN.names)] #all users and bought or not
      topN.userPurchases<-topN.purchases[topN.purchases$user==user,]
      topN.userPurchases <- as.numeric(topN.userPurchases[!(names(topN.userPurchases) %in% c("user"))])
      
      # We then calculate the score for that product and that user
      holder[i,j]<-getScore(similarities=topN.similarities,history=topN.userPurchases)
      
    } # close else statement
  } # end product for loop   
} # end user for loop

# Output the results to a file
data.germany.user.scores <- holder
write.csv(file="final-user-scores.csv",data.germany.user.scores)

# Lets make our recommendations pretty
data.germany.user.scores.holder <- matrix(NA, nrow=nrow(data.germany.user.scores),ncol=100,dimnames=list(rownames(data.germany.user.scores)))
for(i in 1:nrow(data.germany.user.scores)) 
{
  data.germany.user.scores.holder[i,] <- names(head(n=100,(data.germany.user.scores[,order(data.germany.user.scores[i,],decreasing=TRUE)])[i,]))
}

# Write output to file
write.csv(file="final-user-recommendations.csv",data.germany.user.scores.holder)