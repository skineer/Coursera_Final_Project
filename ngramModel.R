####################################################################################
# OBJECTIVE:  Initial analysis on swiftkey data                                    #
# AUTHOR   :  Renato Pedroso Neto                                                  #
# COMPANY  :  Coursera John Hopkins Capstone Project                               #
# DATA     :  Can be downloaded at:                                                #
# https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip  #
####################################################################################

library(tm)
library(RWeka)
library(dplyr)

# remove punctuation, lower the words, remove numbers, remove whitespaces ...
dealWithWords <- function(charInput){
  #charInput --> Type large character
  charInputVectorized <- VCorpus(VectorSource(charInput))
  charInputVectorized <- tm_map(charInputVectorized, tolower)
  charInputVectorized <- tm_map(charInputVectorized, removePunctuation)
  charInputVectorized <- tm_map(charInputVectorized, removeNumbers)
  charInputVectorized <- tm_map(charInputVectorized, stripWhitespace)
  charInputVectorized <- tm_map(charInputVectorized, PlainTextDocument)
}

nGramMaker1 <- function(charVectorized, n = 1){
  #charVectorize --> Type TM Vectorize (Large Character)
  #n             --> The size of the NGRAM
  tokenizer <- NGramTokenizer(charVectorized, Weka_control(min = n, max = n))
}

nGramMaker2 <- function(charVectorized, n = 2){
  #charVectorize --> Type TM Vectorize (Large Character)
  #n             --> The size of the NGRAM
  tokenizer <- NGramTokenizer(charVectorized, Weka_control(min = n, max = n))
}

nGramMaker3 <- function(charVectorized, n = 3){
  #charVectorize --> Type TM Vectorize (Large Character)
  #n             --> The size of the NGRAM
  tokenizer <- NGramTokenizer(charVectorized, Weka_control(min = n, max = n))
}

nGramMaker4 <- function(charVectorized, n = 4){
  #charVectorize --> Type TM Vectorize (Large Character)
  #n             --> The size of the NGRAM
  tokenizer <- NGramTokenizer(charVectorized, Weka_control(min = n, max = n))
}

wordCountVector<- function(gramNumber, gramFull){
  # this function receive a vector of words and count the ocurrencies on the "search on"
  # also calculates the probability, using Markov Chains with add-1 smooth
  # to predict words the backoff model needs to be implemented as well
  # based on https://www.r-bloggers.com/natural-language-processing-what-would-shakespeare-say/
  # gramNumber --> size of the gram (1 = unigram, 2 = bigram, 3 = trigram)
  # gramFull   --> original constructed ngram to get the V for smoothing
  # return      --> return a data frame with 3 columns. Word, Ocurrencies, probability
  V = gramFull$nrow
  count <- slam::row_sums(gramFull)
  namesGram <- names(count)
  wordCountDF <- data.frame(word=character(V), 
                            ocurrencies = integer(V),
                            probability = numeric(V), stringsAsFactors = FALSE)
  for (i in 1:V){
    wordVector = namesGram[i]
    wordCountDF$word[i] <- wordVector
    numOcurrences <- count[[i]]
    wordCountDF$ocurrencies[i] <- numOcurrences
    if (gramNumber == 1){
      wordCountDF$probability[i] <- wordCountDF$ocurrencies[i] / V
    }
    if (gramNumber == 2){
      wordVector = namesGram[i]
      #wi <- strsplit(wordVector, ' ')[[1]][[1]]
      wi_1 <- strsplit(wordVector, ' ')[[1]][[2]]
      numOcurrencesAll <- count[[i]]
      numOcurrenceswi1 <- wordCountProbGram1[wordCountProbGram1$word == wi_1,]$ocurrencies
      if (is.integer(numOcurrenceswi1) == FALSE) {
        numOcurrenceswi1 = 0
      }
      wordCountDF$probability[i] <- (numOcurrencesAll + 1) / (numOcurrenceswi1 + V)
    }
    if (gramNumber == 3){
      wordVector = namesGram[i]
      #wi <- strsplit(wordVector, ' ')[[1]][[1]]
      wi_1 <- strsplit(wordVector, ' ')[[1]][[2]]
      wi_2 <- strsplit(wordVector, ' ')[[1]][[3]]
      wi1wi2 <- paste(wi_1,' ',wi_2, sep = '')
      numOcurrencesAll <- count[[i]]
      numOcurrenceswi1wi2 <- wordCountProbGram2[wordCountProbGram2$word == wi1wi2,]$ocurrencies
      if (is.integer(numOcurrenceswi1wi2) == FALSE) {
        numOcurrenceswi1wi2 = 0
      }
      wordCountDF$probability[i] <- (numOcurrencesAll + 1) / (numOcurrenceswi1wi2 + V)
    }
    if (gramNumber == 4){
      wordVector = namesGram[i]
      #wi <- strsplit(wordVector, ' ')[[1]][[1]]
      wi_1 <- strsplit(wordVector, ' ')[[1]][[2]]
      wi_2 <- strsplit(wordVector, ' ')[[1]][[3]]
      wi_3 <- strsplit(wordVector, ' ')[[1]][[4]]
      wi1wi2wi3 <- paste(wi_1,' ',wi_2, ' ', wi_3, sep = '')
      numOcurrencesAll <- count[[i]]
      numOcurrenceswi1wi2wi3 <- wordCountProbGram3[wordCountProbGram3$word == wi1wi2wi3,]$ocurrencies
      if (is.integer(numOcurrenceswi1wi2wi3) == FALSE) {
        numOcurrenceswi1wi2wi3 = 0
      }
      wordCountDF$probability[i] <- (numOcurrencesAll + 1) / (numOcurrenceswi1wi2wi3 + V)
    }
  }
  return(wordCountDF)
} 

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

setwd("C:\\Users\\lc43922\\Coursera_Final_Project\\final")

# load all languages blogs sentences:
#blog_de <- readLines("de_DE/de_DE.blogs.txt", warn = FALSE)
blog_us <- readLines("en_US/en_US.blogs.txt", warn = FALSE)
#blog_fi <- readLines("fi_FI/fi_FI.blogs.txt", warn = FALSE)
#blog_ru <- readLines("ru_RU/ru_RU.blogs.txt", warn = FALSE)

# load all languages news sentences:
#news_de <- readLines("de_DE/de_DE.news.txt", warn = FALSE)
news_us <- readLines("en_US/en_US.news.txt", warn = FALSE)
#news_fi <- readLines("fi_FI/fi_FI.news.txt", warn = FALSE)
#news_ru <- readLines("ru_RU/ru_RU.news.txt", warn = FALSE)

# load all languages twitter sentences:
#twitter_de <- readLines("de_DE/de_DE.twitter.txt", warn = FALSE)
twitter_us <- readLines("en_US/en_US.twitter.txt", warn = FALSE)
#twitter_fi <- readLines("fi_FI/fi_FI.twitter.txt", warn = FALSE)
#twitter_ru <- readLines("ru_RU/ru_RU.twitter.txt", warn = FALSE)

blog_us <- iconv(blog_us, "latin1", "ASCII", sub="")
news_us <- iconv(news_us, "latin1", "ASCII", sub="")
twitter_us <- iconv(twitter_us, "latin1", "ASCII", sub="")

set.seed(999)
sampleEnglishOnly <- c(sample(blog_us, length(blog_us) * 0.01),
                       sample(news_us, length(news_us) * 0.01),
                       sample(twitter_us, length(twitter_us) * 0.01))

englishWordsVectorized <- dealWithWords(sampleEnglishOnly)
gram1 <- TermDocumentMatrix(englishWordsVectorized, control = list(tokenize = nGramMaker1))
gram2 <- TermDocumentMatrix(englishWordsVectorized, control = list(tokenize = nGramMaker2))
gram3 <- TermDocumentMatrix(englishWordsVectorized, control = list(tokenize = nGramMaker3))
gram4 <- TermDocumentMatrix(englishWordsVectorized, control = list(tokenize = nGramMaker4))

#gram1Filtered <- findFreqTerms(gram1,lowfreq = 2)
#gram2Filtered <- findFreqTerms(gram2,lowfreq = 2)
#gram3Filtered <- findFreqTerms(gram3,lowfreq = 2)
# removing not necessary variables
remove(blog_us, news_us, twitter_us, englishWordsVectorized)

wordCountProbGram1 <- wordCountVector(gramNumber = 1, gramFull = gram1)
wordCountProbGram1 <- arrange(wordCountProbGram1, desc(probability))
gc()
save(wordCountProbGram1, file = "C:\\Users\\lc43922\\Coursera_Final_Project\\gram1.RData")
wordCountProbGram2 <- wordCountVector(gramNumber = 2, gramFull = gram2)
wordCountProbGram2 <- arrange(wordCountProbGram2, desc(probability))
gc()
save(wordCountProbGram2, file = "C:\\Users\\lc43922\\Coursera_Final_Project\\gram2.RData")
wordCountProbGram3 <- wordCountVector(gramNumber = 3, gramFull = gram3)
wordCountProbGram3 <- arrange(wordCountProbGram3, desc(probability))
gc()
save(wordCountProbGram3, file = "C:\\Users\\lc43922\\Coursera_Final_Project\\gram3.RData")
wordCountProbGram4 <- wordCountVector(gramNumber = 4, gramFull = gram4)
wordCountProbGram4 <- arrange(wordCountProbGram4, desc(probability))
gc()
save(wordCountProbGram3, file = "C:\\Users\\lc43922\\Coursera_Final_Project\\gram4.RData")

#start the finding algorithm
# TODO put string in lower
string <- tail(strsplit('day rise', ' ')[[1]], n = 3)
string <- paste(string, collapse = ' ')
bestMatch <- searchBestMatch(string, n = 3)
# did it find something?
if (bestMatch[1,2] == 0){
  string <- tail(strsplit('day rise', ' ')[[1]], n = 2)
  string <- paste(string, collapse = ' ')
  bestMatch <- searchBestMatch(string, n = 3)
}
if (bestMatch[1,2] == 0){
  string <- tail(strsplit('day rise', ' ')[[1]], n = 1)
  string <- paste(string, collapse = ' ')
  bestMatch <- searchBestMatch(string, n = 3)
}
if (bestMatch[1,2] == 0){
  string <- tail(strsplit('day rise', ' ')[[1]], n = 0)
  string <- paste(string, collapse = ' ')
  bestMatch <- searchBestMatch(string, n = 3)
}
