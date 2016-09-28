####################################################################################
# OBJECTIVE:  script to search values on the ngrams                                #
# AUTHOR   :  Renato Pedroso Neto                                                  #
# COMPANY  :  Coursera John Hopkins Capstone Project                               #
# DATA     :  saved NGRAMS (ngram*.RData                                           #
####################################################################################

library(data.table)
library(dplyr)

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
    bestMatch <- arrange(wordCountProbGram1Break, desc(probability))[1:n,]
  }
  if (size == 1){
    parsedWordNGram2 <- strsplit(toWord, ' ')[[1]][1]
    bestMatchAux <- wordCountProbGram2Break[.(parsedWordNGram2)]
    bestMatch <- arrange(bestMatchAux, desc(probability))[1:n,]
  }
  if (size == 2){
    parsedWordNGram3 <- toWord[1:2]
    parsedWordNGram3 <- paste(parsedWordNGram3, collapse = ' ')
    bestMatchAux <- wordCountProbGram3Break[.(parsedWordNGram3)]
    bestMatch <- arrange(bestMatchAux, desc(probability))[1:n,]
  }
  if (size == 3){
    parsedWordNGram4 <- toWord[1:3]
    parsedWordNGram4 <- paste(parsedWordNGram4, collapse = ' ')
    bestMatchAux <- wordCountProbGram4Break[.(parsedWordNGram4)]
    bestMatch <- arrange(bestMatchAux, desc(probability))[1:n,]
  }
  return(bestMatch)
}

setwd("C:\\Users\\lc43922\\Coursera_Final_Project")
load(file = 'gram1Break.RData')
load(file = 'gram2Break.RData')
load(file = 'gram3Break.RData')
load(file = 'gram4Break.RData')

#transform all the DF to DT asn set the key for fast retrieve of values
wordCountProbGram1Break <- as.data.table(wordCountProbGram1Break)
wordCountProbGram2Break <- as.data.table(wordCountProbGram2Break)
wordCountProbGram3Break <- as.data.table(wordCountProbGram3Break)
wordCountProbGram4Break <- as.data.table(wordCountProbGram4Break)
setkey(wordCountProbGram1Break, word)
setkey(wordCountProbGram2Break, word)
setkey(wordCountProbGram3Break, word)
setkey(wordCountProbGram4Break, word)

# This variable will come from the user interface
uiVariable <- tolower('a a a a a a a a a a a a a acreage of')
string <- tail(strsplit(uiVariable, ' ')[[1]], n = 3)
string <- paste(string, collapse = ' ')
bestMatch <- searchBestMatch(string)

# did it find something?
if (is.na(bestMatch$prediction) == TRUE){
  string <- tail(strsplit(uiVariable, ' ')[[1]], n = 2)
  string <- paste(string, collapse = ' ')
  bestMatch <- searchBestMatch(string)
}
if (is.na(bestMatch$prediction) == TRUE){
  string <- tail(strsplit(uiVariable, ' ')[[1]], n = 1)
  string <- paste(string, collapse = ' ')
  bestMatch <- searchBestMatch(string)
}
if (is.na(bestMatch$prediction) == TRUE){
  string <- tail(strsplit(uiVariable, ' ')[[1]], n = 0)
  string <- paste(string, collapse = ' ')
  bestMatch <- searchBestMatch(string)
}
bestMatch
bestMatch$prediction