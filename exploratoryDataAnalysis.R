####################################################################################
# OBJECTIVE:  Exploratory Analysis on swiftkey data                                #
# AUTHOR   :  Renato Pedroso Neto                                                  #
# COMPANY  :  Coursera John Hopkins Capstone Project                               #
# DATA     :  Can be downloaded at:                                                #
# https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip  #
####################################################################################

library("tm")
setwd("C:\\Users\\lc43922\\Coursera_Final_Project\\final")
blog_us <- readLines("en_US/en_US.blogs.txt", warn = FALSE)
news_us <- readLines("en_US/en_US.news.txt", warn = FALSE)
twitter_us <- readLines("en_US/en_US.twitter.txt", warn = FALSE)

# do some simple exploratory analysis
# line count
blogsLineCount <- length(blog_us)
newsLineCount <- length(news_us)
twitterLineCount <- length(twitter_us)
# word count
blogsWordCount <- sum(sapply(gregexpr("\\W+", blog_us), length) + 1)
newsWordCount <- sum(sapply(gregexpr("\\W+", news_us), length) + 1)
twitterWordCount <- sum(sapply(gregexpr("\\W+", twitter_us), length) + 1)


exploratoryAnalysis <- data.frame(File = c("Blogs","News","Twitter"),
                                  Line_Count = c(blogsLineCount, newsLineCount, twitterLineCount),
                                  Word_Count = c(blogsWordCount, newsWordCount, twitterWordCount))

