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

unigram <- readRDS(file="data/final_unigram.Rda")
bigram <- readRDS(file="data/final_bigram.Rda")
trigram <- readRDS(file="data/final_trigram.Rda")

nextWordPredictor <- function(inputTxt) {
    #clean input
    inputTxt <- tolower(inputTxt)
    inputTxt <- removeNumbers(inputTxt)
    inputTxt <- removePunctuation(inputTxt)
    inputTxt <- stripWhitespace(inputTxt)
    
    #split into words
    inputList <- unlist(strsplit(inputTxt, " "))
    
    numWords <- length(inputList)
    
    if(numWords == 1) {
        predList <- unigram[unigram$terms$one == inputTxt,]$terms$two
    }else if (numWords == 2) {
        predList <- bigram[bigram$terms$one == inputList[1] & 
                           bigram$terms$two == inputList[2],]$terms$three
    }else {
        predList <- trigram[ trigram$terms$one == inputList[numWords-2] & 
                             trigram$terms$two == inputList[numWords-1] &
                             trigram$terms$three == inputList[numWords],]$terms$four
    }
    
    #Return top n predictors
    n <- 3
    if(length(predList) >= n){
        predList <- predList[1:n]
    }
    
    as.character(predList)
}

