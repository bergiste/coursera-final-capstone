library(tm)
setwd("~/ACADEMICS/datascience/Final Capstone")
us_txt <- "final/en_US"
us_corpus <- VCorpus( DirSource(us_txt), readerControl = list( language = "en"))


#see meta data
#us_corpus[[1]]
#meta(us_corpus[[1]])

#Clean up corpus
us_corpus <- tm_map(us_corpus, content_transformer(tolower))
us_corpus <- tm_map(us_corpus, stripWhitespace)


#see raw text
#blogs <- lapply(us_corpus[1], as.character)
#news <- lapply(us_corpus[2], as.character)
#twitter <- lapply(us_corpus[3], as.character)
tdm <- TermDocumentMatrix(us_corpus, control = list(
    stopwords = T, removePunctuation = T
))
