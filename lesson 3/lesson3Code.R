library(tidyverse)

load('lesson 1/recommender.RData')


unlist(viewed_movies)
viewed_movies[,1]

unlist(viewed_movies[,1]) #unlist simplifies a list to a vector

sorted_my_users <- as.character(unlist(viewed_movies[,1]))


head(sorted_my_users)
head(unlist(viewed_movies[,1])) #to check difference

#convert to matrix since setting row names on a tibble is deprecated
viewed_movies <- as.matrix(viewed_movies[,-1])

head(viewed_movies)
head(viewed_movies[,-1])

row.names(viewed_movies) <- sorted_my_users
head(viewed_movies)

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

nrow(viewed_movies)
ncol(viewed_movies)

#get similarites between user pairs
userSimilarites = matrix(0,nrow=15,ncol=15) #initialize 0-matrix, same dim as number of users in viewed_movies
for(i in 1:14){
  for (j in (i+1):15){ #only run 1 side of the matrix (upper triangle)
    userSimilarites[i,j] <- cosineSim(viewed_movies[i,],viewed_movies[j,])
    }
}

userSimilarites = userSimilarites + t(userSimilarites) #mirror upper triange to lower
diag(userSimilarites) <- 0 #set diagonal to 0 (same user similarity)
row.names(userSimilarites) <- row.names(viewed_movies)
colnames(userSimilarites) <- row.names(viewed_movies)
head(userSimilarites)

userSimilarites['198',]

viewed_movies[c("198","1","212"),] #pulling views for 198, 1 (most similar user) and 212 (most dissamilar user)
viewed_movies['198',]


#RECOMMENDING MOVIES FOR A SINGLE USER
#Process of recommending a movie for a single user (let's use 198)

#find movies they've already seen, to not recommend those
viewed_movies['198',]

#recommend what's popular by adding up the # of users that have seen each movie,
#but weight it by similarity to user 198

#work through it for 1 movie example ( Big Lebowski, The (1998))
seen_movie <- viewed_movies[,'Big Lebowski, The (1998)']
simToUser <- userSimilarites['198',]
cbind(seen_movie,simToUser)

crossprod(viewed_movies[,'Big Lebowski, The (1998)'], userSimilarites['198',])

#now do it for all movies and compare
userSimilarites['198',]%*%viewed_movies

#remove movies that 198 has already seen, then just sort in desc order
userScores <- data.frame(title = colnames(viewed_movies),
                         score = as.vector(userSimilarites['198',]%*%viewed_movies),
                         seen = viewed_movies['198',])
userScores
head(userScores)

userScores %>% filter(seen == 0) %>% arrange(desc(score))

#redo it for another user to cement concept

userScores <- data.frame(title = colnames(viewed_movies),
                         score = as.vector(userSimilarites['222',]%*%viewed_movies),
                         seen  = viewed_movies['222',])

userScores %>% filter(seen == 0) %>% arrange(desc(score))

#SIMPLE FUNCTION TO GENERATE USER-BASED CF RECOMMENDATION FOR ANY USER

userBasedReccs <- function(user, userSimilarites, viewed_movies){
  user <- ifelse(is.character(user),user,as.character(user)) #make sure user input is character

  userScores <- data.frame(title = colnames(viewed_movies),
                           score = as.vector(userSimilarites[user,]%*%viewed_movies),
                           seen  = viewed_movies[user,])
  
  userScores %>% filter(seen == 0 ) %>% arrange(desc(score)) %>% select(-seen)
}

userBasedReccs(user = 222, userSimilarites = userSimilarites, viewed_movies = viewed_movies)


#now do it for all users with "lapply"
lapply(sorted_my_users,userBasedReccs,userSimilarites, viewed_movies)




#ITEM BASED COLLABORATIVE FILTERING
#recommendations based on similar movies

#two main conceptual parts
# 1 - Movies are similar if same users seen them
# 2 - Recommendation are based on movies that are similar to movies the user has seen

movies_user <- t(viewed_movies)
head(movies_user)

movieSimilarities = matrix(0,nrow=20,ncol=20)
for (i in 1:19){
  for (j in (i+1):20){
    movieSimilarities[i,j] <- cosineSim(viewed_movies[,i],viewed_movies[,j])
  }
}

movieSimilarities <- movieSimilarities + t(movieSimilarities)
diag(movieSimilarities) <- 0
row.names(movieSimilarities) <- colnames(viewed_movies)
colnames(movieSimilarities) <- colnames(viewed_movies)

sort(movieSimilarities[,"Apocalypse Now (1979)"], decreasing = TRUE)

which(viewed_movies['222',] ==1)
head(viewed_movies)


#implement main idea behind item-based filter
userSeen <- ratings_red %>% filter(userId == 222) %>% select(title) %>%unlist() %>% as.character()


head(userSeen)
head(movieSimilarities)
movieSimilarities[,userSeen]


apply(movieSimilarities[,userSeen],1,sum)


#one neat calculation
userScores <- tibble(title = row.names(movieSimilarities),
                     score = apply(movieSimilarities[,userSeen],1,sum),
                     seen = viewed_movies['222',])

userScores %>% filter(seen==0) %>% arrange(desc(score))


#A SIMPLE FUNCTION TO GENERATE AN ITEM-BASED CF RECCOMENDATAION FOR ANY USER
itemBasedRecc <- function(user, movieSimilarities, viewed_movies){
  user <- ifelse(is.character(user),user,as.character(user))
  
  userSeen <- row.names(movieSimilarities)[viewed_movies[user,]==TRUE]
  user_scores <- tibble(title = row.names(movieSimilarities),
                           score = apply(movieSimilarities[,userSeen],1,sum),
                           seen = viewed_movies[user,])
  
  user_scores %>%
    filter(seen==0) %>%
    arrange(desc(score))%>%
    select(-seen)
}

itemBasedRecc(user = 222, movieSimilarities = movieSimilarities, viewed_movies = viewed_movies)

#now do it for all users
lapply(sorted_my_users, itemBasedRecc, movieSimilarities,viewed_movies)

#COLLABORATIVE FILTERING WITH MATRIX FACTORIZATION
#perform collaborative filtering based on matrix factorization (a topic from linear algebra)
#also called "matrix decomposition"

#setup data in CSV for Excel example
ratings_wide <- ratings_red %>% select(userId,title,rating) %>% complete(userId, title) %>% spread(key = title, value = rating)
head(ratings_wide)
head(ratings_red)

#convert data into matrix form
sorted_my_users <- as.character(unlist(ratings_wide[,1]))
ratings_wide <- as.matrix(ratings_wide[,-1])
row.names(ratings_wide) <- sorted_my_users

write.csv(ratings_wide,"lesson 3/ratingsForExelExample.csv")

#Defining a function that will compuyte the squared differences between observed and predicted movie ratings

reccAccuracy <- function(x, observed_ratings){
  
  # extract user and movie factors from parameter vector (x)
  # the first 75 elements are latent factors for users, the rest are for movies
  
  userFactors <- matrix(x[1:75],15,5)
  movieFactors <- matrix(x[76:175],5,20)
  
  #get predictions from dot products of respective user and movie factor
  predictedRatings <- userFactors  %*% movieFactors

  errors <- (observed_ratings - predictedRatings)^2
  
  sqrt(mean(errors[!is.na(observed_ratings)])) #only use rated movies
}

#optimize the values in the user & movie latent factors, choosing them so that the rmse is a minimum
#use optim(), the built in numerical optimizer in R.

set.seed(10)
#optimization step
rec1 <- optim(par = runif(175),reccAccuracy, observed_ratings = ratings_wide, control = list(maxit=100000))

rec1$converge
rec1$value
#note, it doesn't converge, run for longer or use different solving algorithm 

#extract optimal user factors
userFactors <- matrix(rec1$par[1:75],15,5)
head(userFactors)

#extract optimal movie factors
movieFactors  <- matrix(rec1$par[76:175],5,20)
head(movieFactors)

#we can get predicted movie ratings for any user, by taking the appropriate dot product of user and movie factors.
#predictions for user 1:

predicted_ratings <- userFactors %*% movieFactors
rbind(round(predicted_ratings[1,],1),as.numeric(ratings_wide[1,]))


#ADDING L2 REGULARIZATION
#adds a penalty term to the function that we're trying to minimize

#rewrite accuracy function to make use of L2 Regularization
evaluateFitL2 <- function(x,observed_ratings,lambda){
  
  userFactors <- matrix(x[1:75],15,5)
  movieFactors <- matrix(x[76:175],5,20)

  predictedRatings <- userFactors %*% movieFactors
  
  errors <- (observed_ratings - predictedRatings)^2
  
  penalty <- sqrt(sum(userFactors ^2, movieFactors ^2))
  
  accuracy <- sqrt(mean(errors[!is.na(observed_ratings)]))+lambda*penalty
  
  return(accuracy)
}

set.seed(10)
#optimization step
rec2 <- optim(par = runif(175), evaluateFitL2, lambda = 3e-2, observed_ratings = ratings_wide, control = list(maxit = 100000))
rec2$converge
rec2$value

rbind(round(predicted_ratings[1,],1),as.numeric(ratings_wide[1,]))

#ADDING BIAS TERMS
#Bias terms are additive factors that model the fact that some users are more generous than others
#and that some movies are better than others.

evaluateFitL2Bias <- function(x,observed_ratings,lambda){
  
  userFactors <- matrix(x[1:75],15,5)
  movieFactors <- matrix(x[76:175],5,20)
  
  userBias <- matrix(x[176:190],nrow = 15, ncol = 20)
  movieBias <- t(matrix(x[191:210], nrow = 20, ncol = 15))
  
  predictedRatings <- userFactors %*% movieFactors + userBias +  movieBias

  errors <- (observed_ratings - predictedRatings) ^2
  
  penalty <- sqrt(sum(userFactors ^2, movieFactors^2))
  
  sqrt(mean(errors[!is.na(observed_ratings)])) + lambda*penalty
  }

#rerun
set.seed(10)
rec3 <- optim(par = runif(220),evaluateFitL2Bias, observed_ratings = ratings_wide, lambda = 3e-2, control = list(maxit = 100000))
rec3$converge
rec3$value