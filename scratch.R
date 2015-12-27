library(tm)
setwd("~/ACADEMICS/datascience/Final Capstone")
# us_txt <- "final/en_US"
# us_corpus <- VCorpus( DirSource(us_txt), readerControl = list( language = "en"))
# 
# 
# #see meta data
# #us_corpus[[1]]
# #meta(us_corpus[[1]])
# 
# #Clean up corpus
# us_corpus <- tm_map(us_corpus, content_transformer(tolower))
# us_corpus <- tm_map(us_corpus, stripWhitespace)
# 
# 
# #see raw text
# #blogs <- lapply(us_corpus[1], as.character)
# #news <- lapply(us_corpus[2], as.character)
# #twitter <- lapply(us_corpus[3], as.character)
# tdm <- TermDocumentMatrix(us_corpus, control = list(
#     stopwords = T, removePunctuation = T
# ))

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
    
    runUnigram <- function(words){
        unigram[unigram$terms$one == words,]$terms$two
    }
    
    runBigram <- function(words){
        bigram[bigram$terms$one == words[1] & 
                   bigram$terms$two == words[2],]$terms$three
    }
    
    runTrigram <- function(words){
        trigram[ trigram$terms$one == words[1] & 
                     trigram$terms$two == words[2] &
                     trigram$terms$three == words[3],]$terms$four
    }
    
    if(numWords == 1) {
        print("running unigram")
        predList <- runUnigram(inputTxt)
    }else if (numWords == 2) {
        print("running bigram")
        word1 <- inputList[1]
        word2 <- inputList[2]
        predList <- runBigram(c(word1, word2))
        
        if(length(predList) == 0){
            print("Bigram failed running unigram")
            predList <- runUnigram(word2)
        }
    }else {
        print("running trigram")
        word1 <- inputList[numWords-2]
        word2 <- inputList[numWords-1]
        word3 <- inputList[numWords]
        predList <- runTrigram(c(word1, word2, word3))
        
        if(length(predList) == 0){
            print("trigram failed running bigram")
            predList <- runBigram(c(word2,word3))
        }
        
        if(length(predList) == 0){
            print("bigram failed running unigram")
            predList <- runUnigram(word3)
        }
    }
    
    #Return top n predictors
    n <- 3
    if(length(predList) >= n){
        predList <- predList[1:n]
    }
    
    as.character(predList)
    
}

