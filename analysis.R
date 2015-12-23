library(tm)
library(RWekajars)
library(RWeka)
library(wordcloud)
require(openNLP)
require(reshape)
setwd("~/ACADEMICS/datascience/Final Capstone")

#Corpus files dowloaded from:
# http://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip
#cached locally for performace

#Get files locations
us_txt_dir <- "final/en_US/"
blogs_txt <- paste(us_txt_dir, "en_US.blogs.txt", sep = "")
news_txt <- paste(us_txt_dir, "en_US.news.txt", sep = "")
twitter_txt <- paste(us_txt_dir, "en_US.twitter.txt", sep = "")

#Load text into R. Lowecase to normalize future operations
blogs_data <- tolower(readLines(blogs_txt, skipNul = T))
news_data <- tolower(readLines(news_txt, skipNul = T))
twitter_data <- tolower(readLines(twitter_txt, skipNul = T))

#Get line counts
blogs_lines <- length(blogs_data)
news_lines <- length(news_data)
twitter_lines <- length(twitter_data)

#Get max line length
blogs_char_cnt <- lapply(blogs_data, nchar)
blogs_max_chars <- blogs_char_cnt[[which.max(blogs_char_cnt)]]

news_char_cnt <- lapply(news_data, nchar)
news_max_chars <- news_char_cnt[[which.max(news_char_cnt)]]

twitter_char_cnt <- lapply(twitter_data, nchar)
twitter_max_chars <- twitter_char_cnt[[which.max(twitter_char_cnt)]]

#Get word counts (based on spaces)
blogs_words <- sum( sapply(gregexpr("\\S+", blogs_data), length ) )
news_words <- sum( sapply(gregexpr("\\S+", news_data), length ) )
twitter_words <- sum( sapply(gregexpr("\\S+", twitter_data), length ) )

#Summary of corpus stats
corpus_stats <- data.frame( "Files" = c("Blogs", "News", "Twitter"),
                            "Lines" = c(blog_lines, news_lines, twitter_lines),
                            "Longest Line" = c(blogs_max_chars, news_max_chars, twitter_max_chars),
                            "Words" = c(blogs_words, news_words, twitter_words))

#search for specific word ratios
twitter_num_love <- sum(grepl("love", twitter_data) == TRUE)
twitter_hate_num <- sum(grepl("hate", twitter_data) == TRUE)
twitter_love_hate_ratio <- twitter_num_love / twitter_hate_num

#search specific words in a sentence
bs_line <- twitter_data[grepl("biostats", twitter_data)]
chess_line_cnt <- sum(grepl("A computer once beat me at chess, but it was no match for me at kickboxing", twitter_data, ignore.case = T) == TRUE)

#Create samples for further processing
set.seed(892)
sample_pct <- .05
blogs_data_sample <- blogs_data[sample(1:blogs_lines, blogs_lines*sample_pct)]
news_data_sample <- news_data[sample(1:news_lines, news_lines*sample_pct)]
twitter_data_sample <- twitter_data[sample(1:twitter_lines, twitter_lines*sample_pct)]
sample_data <- list(blogs_data_sample, news_data_sample, twitter_data_sample)

#Create Corpus
cp <- Corpus(VectorSource(sample_data))

#Clean up corpus
cp <- tm_map(cp, removeNumbers)
cp <- tm_map(cp, removePunctuation)
cp <- tm_map(cp, stripWhitespace)

#Create doc term matrix
dtm <- DocumentTermMatrix(cp, control = list(stopwords = TRUE))

#Find frequent words
dtm_mtrx <- as.matrix(dtm)
frequency <- colSums(dtm_mtrx)
frequency <- sort(frequency, decreasing = TRUE)
wordcloud(names(frequency), frequency, min.freq = 25, random.order = FALSE, colors = brewer.pal(8, "Spectral"))

#tokenize for unigrams

ngramTokenizer <- function(l) {
    function(x) unlist(lapply(ngrams(words(x), l), paste, collapse = " "), use.names = FALSE)
}

#generate unigram data set
generateNgramData <- function(n) {
    ng_tdm <- TermDocumentMatrix(cp, control = list(tokenize = ngramTokenizer(n)))
    ng_matrix <- as.matrix(ng_tdm)
    ng_matrix <- rowSums(ng_matrix)
    ng_matrix <- sort(ng_matrix, decreasing = TRUE)
    final_ngram <- data.frame(terms = names(ng_matrix), freq = ng_matrix)
    
    if(n == 2) columns <- c('one', 'two')
    if(n == 3) columns <- c('one', 'two', 'three')
    if(n == 4) columns <- c('one', 'two', 'three', 'four')
    
    final_ngram <- transform(final_ngram, terms = colsplit(terms, split = " ", names = columns ))
    final_ngram
}
final_unigram <- generateNgramData(2)
final_bigram <- generateNgramData(3)
final_trigram <- generateNgramData(4)

#save final output for fast performace of Shiny App
save(final_unigram, file = "data/final_unigram.Rda")
save(final_bigram, file = "data/final_bigram.Rda")
save(final_trigram, file = "data/final_trigram.Rda")
