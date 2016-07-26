####################################################################################
# OBJECTIVE:  Tokenization, remove bad words, punctuation and stopwords            #
# AUTHOR   :  Renato Pedroso Neto                                                  #
# COMPANY  :  Coursera John Hopkins Capstone Project                               #
# DATA     :  Can be downloaded at:                                                #
# https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip  #
####################################################################################
library(tm)
#download badwords archive
badWords <- readLines("http://www.cs.cmu.edu/~biglou/resources/bad-words.txt", 
                      warn = FALSE)

cleanTheData <- function(data) {
    cont           <- 0
    removeAfter    <- c()
    loweredData    <- tolower(data)
    removePunc     <- removePunctuation(loweredData)
    removeBadWords <- removeWords(removePunc, badWords)
    tokenizedData  <- MC_tokenizer(removeBadWords)
    for(i in length(tokenizedData)){
        if(grepl('http://', tokenizedData[i]) == TRUE){
           removeAfter[i] <- tokenizedData[i]  
        }
    }
    tokenizedData <- removeWords(tokenizedData, removeAfter)
    return(tokenizedData)
}
setwd("C:\\Users\\Renato\\Capstone Project\\data\\final")

# Load only english words
blog_us <- readLines("en_US\\en_US.blogs.txt", warn = FALSE)
news_us <- readLines("en_US\\en_US.news.txt", warn = FALSE)
twitter_us <- readLines("en_US\\en_US.twitter.txt", warn = FALSE)

# Sampling the data
blog_us_sample <- sample(blog_us, 4000)
news_us_sample <- sample(news_us, 4000)
twitter_us_sample <- sample(twitter_us, 4000)

# Load the cleaned data
blog_us_clean       <- cleanTheData(blog_us_sample)
news_us_clean       <- cleanTheData(news_us_sample)
twitter_us_clean    <- cleanTheData(twitter_us_sample)

