####################################################################################
# OBJECTIVE:  script to search values on the ngrams                                #
# AUTHOR   :  Renato Pedroso Neto                                                  #
# COMPANY  :  Coursera John Hopkins Capstone Project                               #
# DATA     :  saved NGRAMS (ngram*.RData                                           #
####################################################################################

library(data.table)

searchBestMatch <- function(searchValue, n = 1){
  # this function receive a string to get the best match on the probabilities grams
  # searchValue --> string of any size
  # n           --> how many matches to return
  # return      --> data frame with best match / matches
  size <- length(strsplit(searchValue, ' ')[[1]])
  toWord <- tail(strsplit(searchValue, ' ')[[1]], n = 3)
  found <- 0
  bestMatch = data.frame( word = character(n),
                          ocurrencies = integer(n),
                          probability = numeric(n),
                          stringsAsFactors = FALSE)
  if (size == 0){
    for (i in 1:nrow(wordCountProbGram1)){
      bestMatch <- wordCountProbGram1[1:n,]
      break
    }
  }
  if (size == 1){
    for (i in 1:nrow(wordCountProbGram2)){
      if (grepl(paste("^(", toWord, " )", sep = ''), wordCountProbGram2$word[i]) == TRUE){
        found <- found + 1
        bestMatch[found,] <- wordCountProbGram2[i,]
        if (n <= found){
          break
        }
      }
    }
  }
  if (size == 2){
    for (i in 1:nrow(wordCountProbGram3)){
      if (grepl(paste("^(", toWord[1], ' ', toWord[2]," )", sep = ''), wordCountProbGram3$word[i]) == TRUE){
        found <- found + 1
        bestMatch[found,] <- wordCountProbGram3[i,]
        if (n <= found){
          break
        }
      }
    }
  }
  if (size == 3){
    for (i in 1:nrow(wordCountProbGram4)){
      if (grepl(paste("^(", toWord[1], ' ', toWord[2], ' ' , toWord[3], " )", sep = ''), wordCountProbGram4$word[i]) == TRUE){
        found <- found + 1
        bestMatch[found,] <- wordCountProbGram4[i,]
        if (n <= found){
          break
        }
      }
    }
    
  }
  return(bestMatch)
}

setwd("C:\\Users\\lc43922\\Coursera_Final_Project")
load(file = 'gram1Break.RData')
load(file = 'gram2.RData')
load(file = 'gram3.RData')
load(file = 'gram4Break.RData')

#start the finding algorithm
string <- tail(strsplit('this is', ' ')[[1]], n = 3)
string <- paste(string, collapse = ' ')
bestMatch <- searchBestMatch(string)
# did it find something?
if (bestMatch[1,2] == 0){
  string <- tail(strsplit(tolower('this is sparta this'), ' ')[[1]], n = 2)
  string <- paste(string, collapse = ' ')
  bestMatch <- searchBestMatch(string, n = 3)
}
if (bestMatch[1,2] == 0){
  string <- tail(strsplit(tolower('this is sparta this'), ' ')[[1]], n = 1)
  string <- paste(string, collapse = ' ')
  bestMatch <- searchBestMatch(string, n = 3)
}
if (bestMatch[1,2] == 0){
  string <- tail(strsplit(tolower('this is sparta this'), ' ')[[1]], n = 0)
  string <- paste(string, collapse = ' ')
  bestMatch <- searchBestMatch(string, n = 3)
}
