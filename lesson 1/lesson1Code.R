#dir.create('lesson 1/rawData')

#download.file(url = "http://files.grouplens.org/datasets/movielens/ml-latest-small.zip", destfile = "lesson 1/rawData/ml-latest-small.zip")
#unzip("lesson 1/rawData/ml-latest-small.zip",exdir = "lesson 1/rawData")


#movies <- read.csv("lesson 1/rawData/ml-latest-small/movies.csv")
#ratings <- read.csv("lesson 1/rawData/ml-latest-small/ratings.csv")
#tags <- read.csv("lesson 1/rawData/ml-latest-small/tags.csv")
#links <- read.csv("lesson 1/rawData/ml-latest-small/links.csv")

#save(links, movies, ratings, tags, file = "lesson 1/rawData/movielens-small.Rdata")

#rm(list = ls())
load("lesson 1/rawData/movielens-small.Rdata")


library(tidyverse)

ratings <- left_join (ratings,movies)

ratings %>% group_by(userId)%>%summarize(count=n()) %>%arrange(desc(count))

users_frq <- ratings %>% group_by(userId) %>% summarize(count=n()) %>% arrange(desc(count))
my_users <-users_frq$userId[101:115]

movies_frq <-ratings%>%group_by(movieId)%>%summarize(count=n())%>%arrange(desc(count))
my_movies <- movies_frq$movieId[101:120]

ratings_red <-ratings%>%filter(userId %in% my_users,movieId %in% my_movies)

ratings_red <- droplevels(ratings_red)
levels(ratings_red$title)

viewed_movies <- ratings_red %>% 
  complete(userId, title) %>% 
  mutate(seen = ifelse(is.na(rating), 0, 1)) %>% 
  select(userId, title, seen) %>% 
  
save(ratings_red,viewed_movies,file = "lesson 1/recommender.RData")  
  spread(key = title, value = seen)