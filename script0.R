####################################################################################
# OBJECTIVE:  Initial analysis on swiftkey data                                    #
# AUTHOR   :  Renato Pedroso Neto                                                  #
# COMPANY  :  Coursera John Hopkins Capstone Project                               #
# DATA     :  Can be downloaded at:                                                #
# https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip  #
####################################################################################

setwd("C:\\Users\\Renato\\Capstone Project\\data\\final")

# load all languages blogs sentences:
blog_de <- readLines("de_DE\\de_DE.blogs.txt", warn = FALSE)
blog_us <- readLines("en_US\\en_US.blogs.txt", warn = FALSE)
blog_fi <- readLines("fi_FI\\fi_FI.blogs.txt", warn = FALSE)
blog_ru <- readLines("ru_RU\\ru_RU.blogs.txt", warn = FALSE)

# load all languages news sentences:
news_de <- readLines("de_DE\\de_DE.news.txt", warn = FALSE)
news_us <- readLines("en_US\\en_US.news.txt", warn = FALSE)
news_fi <- readLines("fi_FI\\fi_FI.news.txt", warn = FALSE)
news_ru <- readLines("ru_RU\\ru_RU.news.txt", warn = FALSE)

# load all languages twitter sentences:
twitter_de <- readLines("de_DE\\de_DE.twitter.txt", warn = FALSE)
twitter_us <- readLines("en_US\\en_US.twitter.txt", warn = FALSE)
twitter_fi <- readLines("fi_FI\\fi_FI.twitter.txt", warn = FALSE)
twitter_ru <- readLines("ru_RU\\ru_RU.twitter.txt", warn = FALSE)



