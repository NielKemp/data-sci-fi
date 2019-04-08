library(tidyverse)

load('lesson 1/recommender.RData')


sorted_my_users <- as.character(unlist(viewed_movies[,1]))

#convert to matrix since setting row names on a tibble is deprecated
viewed_movies <- as.matrix(viewed_movies[,-1])

row.names(viewed_movies) <- sorted_my_users


##USER BASED COLLABORATIVE FILTERING


#Simple recommender recommends most popular movies that haven't been seen by user

#use the matrix function 'apply' to sum over all columns (indicated by the '2')
#general form: apply(X, MARGIN (1 for row, 2 for col), FUN (function, sum in this case))
sort(apply(viewed_movies,2,sum),decreasing = TRUE) #recommend the top movies user hasn't seen (very basic)


#User-based CF will change the weight of the most popular movies based on how similar they previous audience is to the current sugestee

#function calculating cosing simulatiry
cosineSim <- function(a,b){crossprod(a,b)/sqrt(crossprod(a)*crossprod(b))}

#try out function
x<-c(0,1,1,0,0)
y<-c(1,1,0,1,1)
cosineSim(x,y)

#try out with "real" data
as.numeric(viewed_movies[1,])
as.numeric(viewed_movies[3,])
cosineSim(viewed_movies[1,],viewed_movies[3,])

#get similarites between user pairs
userSimilarites = matrix(0,nrow=15,ncol=15) #initialize 0-matrix, same dim as viewed_movies
for(i in 1:14){
  for (j in (i+1):15){ #only run 1 side of the matrix (upper triangle)
    userSimilarites[i,j] <- cosineSim(viewed_movies[i,],viewed_movies[j,])
    }
}

userSimilarites = userSimilarites + t(userSimilarites) #mirror upper triange to lower
diag(userSimilarites) <- 0 #set diagonal to 0 (same user similarity)
row.names(userSimilarites) <- row.names(viewed_movies)
colnames(userSimilarites) <- row.names(viewed_movies)
