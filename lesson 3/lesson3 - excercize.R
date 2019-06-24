

library(tidyverse)
load('lesson 1/Recommender.Rdata')

viewed_movies <- as.matrix(viewed_movies[,-1])

#APPLY PAIRWISE SIMILARITIES WITH NO LOOPS (How the actual fuck???)
userSimilarites = matrix(0,nrow=15,ncol=15) #initialize 0-matrix, same dim as number of users in viewed_movies
for(i in 1:14){
  for (j in (i+1):15){ #only run 1 side of the matrix (upper triangle)
    userSimilarites[i,j] <- cosineSim(viewed_movies[i,],viewed_movies[j,])
  }
}

userSim2 <- matrix(0,nrow=15,ncol=15)
userSims2[1:15,1:15] <- cosineSim(viewed_movies[1:15,],viewed_movies[1:15,])

head(cosineSim(viewed_movies, viewed_movies))




