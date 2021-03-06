####################################################################################
# OBJECTIVE:  NGRAM model swift key data                                           #
# AUTHOR   :  Renato Pedroso Neto                                                  #
# COMPANY  :  Coursera John Hopkins Capstone Project                               #
# DATA     :  Can be downloaded at:                                                #
# https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip  #
####################################################################################

library(tm)
library(RWeka)
library(dplyr)
library(sqldf)
library(data.table)

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
  wordCountDT <- data.table(word=rep('_x', V), 
                            ocurrencies = rep(0, V),
                            probability = rep(0.0, V))
  for (i in 1:V){
    wordVector = namesGram[i]
    numOcurrences <- count[[i]]
    if (gramNumber == 1){
      probability <- numOcurrences / V
      set(wordCountDT, i, "word", wordVector)
      set(wordCountDT, i, "ocurrencies", numOcurrences)
      set(wordCountDT, i, "probability", probability)
    }
    if (gramNumber == 2){
      wi_1 <- strsplit(wordVector, ' ')[[1]][[2]]
      numOcurrenceswi1 <- wordCountProbGram1[.(wi_1)]$ocurrencies
      if (is.na(numOcurrenceswi1) == TRUE) {
        numOcurrenceswi1 = 0
      }
      probability <- (numOcurrences + 1) / (as.numeric(numOcurrenceswi1) + V)
      set(wordCountDT, i, "word", wordVector)
      set(wordCountDT, i, "ocurrencies", numOcurrences)
      set(wordCountDT, i, "probability", probability)
    }
    if (gramNumber == 3){
      wi_1 <- strsplit(wordVector, ' ')[[1]][[2]]
      wi_2 <- strsplit(wordVector, ' ')[[1]][[3]]
      wi1wi2 <- paste(wi_1,' ',wi_2, sep = '')
      numOcurrenceswi1wi2 <- wordCountProbGram2[.(wi1wi2)]$ocurrencies
      if (is.na(numOcurrenceswi1wi2) == TRUE) {
        numOcurrenceswi1wi2 = 0
      }
      probability <- (numOcurrences + 1) / (as.numeric(numOcurrenceswi1wi2) + V)
      set(wordCountDT, i, "word", wordVector)
      set(wordCountDT, i, "ocurrencies", numOcurrences)
      set(wordCountDT, i, "probability", probability)
    }
    if (gramNumber == 4){
      wi_1 <- strsplit(wordVector, ' ')[[1]][[2]]
      wi_2 <- strsplit(wordVector, ' ')[[1]][[3]]
      wi_3 <- strsplit(wordVector, ' ')[[1]][[4]]
      wi1wi2wi3 <- paste(wi_1,' ',wi_2, ' ', wi_3, sep = '')
      numOcurrenceswi1wi2wi3 <- wordCountProbGram3[.(wi1wi2wi3)]$ocurrencies
      if (is.na(numOcurrenceswi1wi2wi3) == TRUE) {
        numOcurrenceswi1wi2wi3 = 0
      }
      probability <- (numOcurrences + 1) / (as.numeric(numOcurrenceswi1wi2wi3) + V)
      set(wordCountDT, i, "word", wordVector)
      set(wordCountDT, i, "ocurrencies", numOcurrences)
      set(wordCountDT, i, "probability", probability)
    }
  }
  return(wordCountDT)
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
sampleEnglishOnly <- c(sample(blog_us, length(blog_us) * 0.02),
                       sample(news_us, length(news_us) * 0.02),
                       sample(twitter_us, length(twitter_us) * 0.02))

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
save(wordCountProbGram1, file = "C:\\Users\\lc43922\\Coursera_Final_Project\\ShinyApp\\gram1.RData")

#indexing for performance #####
wordCountProbGram1 <- as.data.table(wordCountProbGram1)
setkey(wordCountProbGram1, word)
################################
wordCountProbGram2 <- wordCountVector(gramNumber = 2, gramFull = gram2)
wordCountProbGram2 <- arrange(wordCountProbGram2, desc(probability))
gc()
save(wordCountProbGram2, file = "C:\\Users\\lc43922\\Coursera_Final_Project\\ShinyApp\\gram2.RData")
#indexing for performance #####
wordCountProbGram2 <- as.data.table(wordCountProbGram2)
setkey(wordCountProbGram2, word)
################################
wordCountProbGram3 <- wordCountVector(gramNumber = 3, gramFull = gram3)
wordCountProbGram3 <- arrange(wordCountProbGram3, desc(probability))
gc()
save(wordCountProbGram3, file = "C:\\Users\\lc43922\\Coursera_Final_Project\\ShinyApp\\gram3.RData")
#indexing for performance #####
wordCountProbGram3 <- as.data.table(wordCountProbGram3)
setkey(wordCountProbGram3, word)
################################
wordCountProbGram4 <- wordCountVector(gramNumber = 4, gramFull = gram4)
wordCountProbGram4 <- arrange(wordCountProbGram4, desc(probability))
gc()
save(wordCountProbGram4, file = "C:\\Users\\lc43922\\Coursera_Final_Project\\ShinyApp\\gram4.RData")

wordCountProbGram1Break <- wordCountProbGram1
wordCountProbGram1Break <- sqldf("select word, '' as prediction, ocurrencies, probability from wordCountProbGram1Break")
save(wordCountProbGram1Break, file = "C:\\Users\\lc43922\\Coursera_Final_Project\\ShinyApp\\gram1Break.RData")
wordCountProbGram2Break <- sqldf("select trim(substr(word, 1, charindex(' ', word))) as word, 
                                 trim(substr(word, charindex(' ', word), length(word))) as prediction,
                                 ocurrencies,
                                 probability
                                 from wordCountProbGram2")
save(wordCountProbGram2Break, file = "C:\\Users\\lc43922\\Coursera_Final_Project\\ShinyApp\\gram2Break.RData")
wordCountProbGram3Break <- sqldf("select trim(substr(word, 1, charindex(' ', word, charindex(' ', word) + 1))) as word, 
                                trim(substr(word, charindex(' ', word, charindex(' ', word) + 1), length(word))) as prediction,
                                 ocurrencies,
                                 probability
                                 from wordCountProbGram3")
save(wordCountProbGram3Break, file = "C:\\Users\\lc43922\\Coursera_Final_Project\\ShinyApp\\gram3Break.RData")
wordCountProbGram4Break <- sqldf("SELECT substr(word, 1, length(word) - CHARINDEX(' ', REVERSE(word))) as word,
                                 REVERSE(substr(REVERSE(word), 1, CHARINDEX(' ', REVERSE(word)) - 1)) as prediction,
                                 ocurrencies,
                                 probability
                                 from wordCountProbGram4")
#indexing for performance #####
wordCountProbGram4 <- as.data.table(wordCountProbGram4)
setkey(wordCountProbGram4, word)
################################
save(wordCountProbGram4Break, file = "C:\\Users\\lc43922\\Coursera_Final_Project\\ShinyApp\\gram4Break.RData")


