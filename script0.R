####################################################################################
# OBJECTIVE:  Initial analysis on swiftkey data                                    #
# AUTHOR   :  Renato Pedroso Neto                                                  #
# COMPANY  :  Coursera John Hopkins Capstone Project                               #
# DATA     :  Can be downloaded at:                                                #
# https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip  #
####################################################################################

library(tm)
library(RWeka)

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

sampleEnglishOnly <- c(sample(blog_us, length(blog_us) * 0.01),
                       sample(news_us, length(news_us) * 0.01),
                       sample(twitter_us, length(twitter_us) * 0.01))

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

englishWordsVectorized <- dealWithWords(sampleEnglishOnly)
gram1 <- TermDocumentMatrix(englishWordsVectorized, control = list(tokenize = nGramMaker1))
gram2 <- TermDocumentMatrix(englishWordsVectorized, control = list(tokenize = nGramMaker2))
gram3 <- TermDocumentMatrix(englishWordsVectorized, control = list(tokenize = nGramMaker3))
freqNgram1 <- findFreqTerms(gram1,lowfreq = 50)
