##Example taken from http://www.salemmarafi.com/code/collaborative-filtering-r/
##Read in the data
data.germany = read.csv(file.choose()) #cfdata.csv
dim(data.germany) #1 where an individual listened, and 0 where someone didn't listen
summary(data.germany[,3]) #listening to acdc
summary(data.germany[,283]) #Listening to u2
names(data.germany)

############################
#  Item Based Similarity   #
############################   

# Drop the user column and make a new data frame
data.germany.ibs = (data.germany[,!(names(data.germany) %in% c("user"))])

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

# Output similarity results to a file
write.csv(data.germany.ibs.similarity,file="final-germany-similarity.csv")

# Get the top 10 neighbours for each
# We are creating a holder matrix here again
data.germany.neighbours <- matrix(NA, nrow=ncol(data.germany.ibs.similarity),ncol=11,dimnames=list(colnames(data.germany.ibs.similarity)))

#Here we are choosing the 10 songs that were most correlated with a particular song
#We are putting those in a row for each song
#You can try i=1 and look at the output to see...

#We loop through all our artists
#We sort our similarity matrix for the artist so that we have the most similar first.
#We take the top 11 (first will always be the same artist) and put them into our placeholder
#Note we use t() to rotate the similarity matrix since the neighbour one is shaped differently
for(i in 1:ncol(data.germany.ibs)) 
{
  data.germany.neighbours[i,] <- (t(head(n=11,rownames(data.germany.ibs.similarity[order(data.germany.ibs.similarity[,i],decreasing=TRUE),][i]))))
}

# Output neighbour results to a file  
write.csv(file="final-germany-item-neighbours.csv",x=data.germany.neighbours[,-1])

##For information on how to complete the user-based method see the website listed at the top of this document.


