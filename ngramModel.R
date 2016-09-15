####################################################################################
# OBJECTIVE:  Initial analysis on swiftkey data                                    #
# AUTHOR   :  Renato Pedroso Neto                                                  #
# COMPANY  :  Coursera John Hopkins Capstone Project                               #
# DATA     :  Can be downloaded at:                                                #
# https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip  #
####################################################################################

library(tm)
library(RWeka)

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

wordCountVector<- function(vector, searchOn){
  # this function receive a vector of words or ngrams and count the ocurrencies on the "search on"
  # vector --> a vector of words or ngrams (already tokenized)
  # searchOn --> data frame, matrix, large character types to search the ocurrencies of words
  # return   --> return a data frame with 2 columns. Word and Ocurrencies
  wordCountDF <- data.frame(word=character(length(vector)), 
                            ocurrencies = integer(length(vector)), stringsAsFactors = FALSE)
  for (i in 1:length(vector)){
    wordVector = vector[i]
    wordCountDF$word[i] <- wordVector
    search <- paste("\\",wordVector,"\\>", sep = '')
    numOcurrences <- length(grep(paste("\\<",wordVector,"\\>", sep = ''), searchOn))
    wordCountDF$ocurrencies[i] <- numOcurrences
  }
  return(wordCountDF)
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

gram1Filtered <- findFreqTerms(gram1,lowfreq = 50)
gram2Filtered <- findFreqTerms(gram2,lowfreq = 50)
gram3Filtered <- findFreqTerms(gram3,lowfreq = 50)

wordCountGram1 <- wordCountVector(gram1Filtered, sampleEnglishOnly)
wordCountGram2 <- wordCountVector(gram2Filtered, sampleEnglishOnly)
wordCountGram3 <- wordCountVector(gram3Filtered, sampleEnglishOnly)
