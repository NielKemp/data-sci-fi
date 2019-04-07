dir.create('lesson 1/rawData')

download.file(url = "http://files.grouplens.org/datasets/movielens/ml-latest-small.zip", destfile = "lesson 1/rawData/ml-latest-small.zip")
unzip("lesson 1/rawData/ml-latest-small.zip",exdir = "lesson 1/rawData")


movies <- read.csv("lesson1/rawData/ml-latest-small/movies.csv")
ratings <- read.csv("lesson1/rawData/ml-latest-small/ratings.csv")
tags <- read.csv("lesson1/rawData/ml-latest-small/tags.csv")
links <- read.csv("lesson1/rawData/ml-latest-small/links.csv")