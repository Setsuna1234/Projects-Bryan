#there exists 20m ratings from 138493 users on 27278 movies
library(dplyr)
library(caret)
library(data.table)
library(tidyr)
library(ggplot2)
library(kernlab)
library(e1071)

rm(list = ls(all.names = TRUE))
#setwd("D:/")
movie_data <- read.csv("movies.csv",stringsAsFactors=FALSE)
rating_data <- read.csv("ratings.csv")

#ONE HOT ENCODING IN THE GENRE
movie_genre <- as.data.frame(movie_data$genres, stringsAsFactors=FALSE)
movie_genre2 <- as.data.frame(tstrsplit(movie_genre[,1], '[|]', 
                                        type.convert=TRUE), 
                              stringsAsFactors=FALSE) 
colnames(movie_genre2) <- c(1:10)
list_genre <- c("Action", "Adventure", "Animation", "Children", 
                "Comedy", "Crime","Documentary", "Drama", "Fantasy",
                "Film-Noir", "Horror", "Musical", "Mystery","Romance",
                "Sci-Fi", "Thriller", "War", "Western")

genre_mat1 <- matrix(0,27279,18)
genre_mat1[1,] <- list_genre
colnames(genre_mat1) <- list_genre
for (index in 1:nrow(movie_genre2)) {
  for (col in 1:ncol(movie_genre2)) {
    gen_col = which(genre_mat1[1,] == movie_genre2[index,col]) 
    genre_mat1[index+1,gen_col] <- 1
  }
}
genre_mat2 <- as.data.frame(genre_mat1[-1,], stringsAsFactors=FALSE) #remove first row, which was the genre list
for (col in 1:ncol(genre_mat2)) {
  genre_mat2[,col] <- as.integer(genre_mat2[,col]) #convert from characters to integers
} 
str(genre_mat2)

rm(genre_mat1)
rm(movie_genre)
rm(movie_genre2)
#END OF ONE HOT ENCODING

genre_mat2 <- genre_mat2[1:nrow(movie_data), ]
movie_data <- cbind(movie_data, genre_mat2)

#Remove unneseccary datacolumns 
movie_data <- movie_data[, -which(names(movie_data) == "title")]
movie_data <- movie_data[, -which(names(movie_data) == "genres")]
rating_data <- rating_data[, -which(names(rating_data) == "timestamp")]

movielens <- left_join(rating_data, movie_data, by = "movieId")

rm(genre_mat2)
rm(movie_data)
rm(rating_data)
rm(list_genre)
rm(index)
rm(gen_col)
rm(col)

#head(movielens)
#userId movieId rating Action Adventure Animation Children Comedy Crime Documentary Drama Fantasy Film-Noir Horror Musical Mystery Romance Sci-Fi Thriller War Western
#1      1       2    3.5      0         1         0        1      0     0           0     0       1         0      0       0       0       0      0        0   0       0
#2      1      29    3.5      0         1         0        0      0     0           0     1       1         0      0       0       1       0      1        0   0       0
#3      1      32    3.5      0         0         0        0      0     0           0     0       0         0      0       0       1       0      1        1   0       0
#4      1      47    3.5      0         0         0        0      0     0           0     0       0         0      0       0       1       0      0        1   0       0
#5      1      50    3.5      0         0         0        0      0     1           0     0       0         0      0       0       1       0      0        1   0       0
#6      1     112    3.5      1         1         0        0      1     1           0     0       0         0      0       0       0       0      0        0   0       0