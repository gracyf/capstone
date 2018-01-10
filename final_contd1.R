#For five and 6 gram using samples
rm(list=ls())

setwd("C:/Users/Gracy/Coursera - Data Science Specialization/Course 10 - Capstone/Week 1/Coursera-SwiftKey/final/en_US")

library(doParallel)
library(data.table)
library(text2vec)
library(tidytext)
library(stringr)
library(qdap)
library(tm)

parallelizeTask <- function(task, ...) {
        # Calculate the number of cores
        ncores <- detectCores() - 1
        # Initiate cluster
        cl <- makeCluster(ncores)
        registerDoParallel(cl)
        #print("Starting task")
        r <- task(...)
        #print("Task done")
        stopCluster(cl)
        r
}


## Function which change a named vector of tokens to a sorted dataframe
vector2TopDF <- function(v){
        v <- v[order(v, decreasing = T)][1:1000]
        v <- data.frame(features = names(v),
                        freq = v, row.names = NULL)
        v <- transform(v, features = reorder(features,freq))
        ## Order factor by freq
        v$features <-factor(v$features, levels=v[order(v$freq), "features"])
        return(v)
}



read_file <- function(input_file_name){
        #file_data <- parallelizeTask(fread,input_file_name,sep='\n',header = FALSE,data.table = FALSE, col.names = c("text"),nrows=500 )
        file_data <- parallelizeTask(fread,input_file_name,sep='\n',header = FALSE,data.table = FALSE, col.names = c("text"))
        
}


tweet_file <- read_file("en_US.twitter.txt")
tweet_file <- as.data.frame(tweet_file[rbinom(nrow(tweet_file)*.5,nrow(tweet_file),.5), ])

blog_file <- read_file("en_US.blogs.txt")
blog_file <- as.data.frame(blog_file[rbinom(nrow(blog_file)*.5,nrow(blog_file),.5), ])

news_file <- read_file("en_US.news.txt")
news_file <- as.data.frame(news_file[rbinom(nrow(news_file)*.5,nrow(news_file),.5), ])

colnames(tweet_file) <- c("text")
colnames(news_file) <- c("text")
colnames(news_file) <- c("text")
temp_file <- rbind(tweet_file,blog_file)
twitter_file <- rbind(temp_file,news_file)

#twitter_file <- tweet_file

rm(tweet_file, blog_file, news_file,temp_file)
gc(verbose = FALSE)



## Function to substitute english shorten sentences
replaceShortEnglish <- function(t){
        t <- gsub(pattern = "can't", replacement = " cannot", t)
        t <- gsub(pattern = "cant", replacement = " cannot", t)
        t <- gsub(pattern = "'m", replacement = " am", t)
        t <- gsub(pattern = "ain't", replacement = "am not", t)
        t <- gsub(pattern = "'re", replacement = " are", t)
        t <- gsub(pattern = "'ve", replacement = " have", t)
        t <- gsub(pattern = "'d", replacement = " would", t)
        t <- gsub(pattern = "'ll", replacement = " will", t)
        t <- gsub(pattern = "n't", replacement = " not", t)
        t <- gsub(pattern = "what's", replacement = "what is", t)
        t <- gsub(pattern = "won't", replacement = "will not", t)
        t <- gsub(pattern = "don't", replacement = "do not", t)
        t <- gsub(pattern = "it's", replacement = "it is", t)
        t <- gsub(pattern = "didn't", replacement = "did not", t)
        t <- gsub(pattern = "didnt", replacement = "did not", t)
        t <- gsub(pattern = "it s", replacement = "it is", t)
        t <- gsub(pattern = "i'd", replacement = "i would", t)
        #t <- gsub(pattern = "u", replacement = "you", t)
        
        return(t)
}
## Function to clean twitter like content, url, RT and hastags
untwitter <- function(t){
        ## Remove URL
        t <- gsub('http\\S+\\s*', '', t)
        ## Remove hastags
        t <- gsub('#\\S+\\s*', '', t)
        # Remove "rt"
        t <- gsub('\\brt\\b', '', t)
        # Remove "rt"
        t <- gsub('\\bu\\b', ' you ', t)
        return(t)
}
## Function to remove all characters except english alphabetic and quotes, hypens, numbers
cleanText <- function(t) {
        t <- gsub("[^a-z _ -\\']", "", t)
        #t <- gsub("[[:digit:]]", "", t)
        t <- iconv(t, "latin1", "ASCII", sub="")
        t <- tolower(t)
        t <- removeNumbers(t)
        t <- removePunctuation(t, preserve_intra_word_dashes = TRUE)
        t <- gsub("http[[:alnum:]]*", "", t)
        t <- removeWords(t, stopwords("english"))
        t <- stripWhitespace(t)
        t <- str_trim(t, side = c("both"))
        t <- gsub("\u0092", "'", t)
        t <- gsub("\u0093|\u0094", "", t)
        t <- removePunctuation(t, preserve_intra_word_dashes = FALSE)
        ## Remove back-to-back same words
        t <- gsub("\\b(\\w+) \\1\\b", "\\1", t)
        ## Remove repeated letters when 3 or more are in a row
        #t <- gsub("(.)\\1{1,}" ,"\\1", t)
        
        
        return(t)
}


## Our tokenizer function
stem_tokenizer <- function(v) {
        v %>%
                untwitter %>%
                replaceShortEnglish %>%
                cleanText 
        # poerter stemmer
        #  %>% lapply(wordStem, 'en')
}

#swC <- c(stopwords("en"), badWords) 





chunk <- 1000
n <- nrow(twitter_file)
r  <- rep(1:ceiling(n/chunk),each=chunk)[1:n]
file_split <- split(twitter_file,r)

rm(twitter_file)
gc(verbose = FALSE)

fivegram_twitter <- data.frame(features = as.factor(character()),freq = numeric(0))
sixgram_twitter <- data.frame(features = as.factor(character()),freq = numeric(0))

chunk_no <- 0
for (splits in 1:length(file_split)) {
        chunk_no <- chunk_no + 1
        print("Processing a chunck: ")
        print(chunk_no)
        ncores <- detectCores() - 1
        cl <- makeCluster(ncores)
        registerDoParallel(cl)
        
        
        
        tokens_twitter <-  file_split[[splits]] %>% tolower %>% word_tokenizer %>% stem_tokenizer
        
        ## Create the iterator on the tokens
        it1 <- itoken(tokens_twitter, progessbar = FALSE,n_chunks = 1000)
        ## Generate the vocabulary using the iterator, 1-gram badwords / stopwords list.
        
        vocab1 <- create_vocabulary(it1, ngram = c(ngram_min = 5L, ngram_max = 5L))
        
        ## We need to reinitialise iterator : it
        it1 <- itoken(tokens_twitter,n_chunks = 1000)
        ## Here we create dtm directly:
        v_vectorizer <- vocab_vectorizer(vocab1)
        #{ sink("/dev/null"); dtm1 <- create_dtm(it, v_vectorizer); sink()}
        dtm1 <- create_dtm(it1, v_vectorizer)
        
        
        print("Creating 5-gram")
        ## Extract the top 1000 most frequent concepts
        wordsFreq <- colSums(as.matrix(dtm1))
        top5_twitter <- vector2TopDF(wordsFreq[order(wordsFreq, decreasing = T)][1:1000])
        
        
        
        print("Creating 6-gram")
        #six gram
        vocab2 <- create_vocabulary(it1, ngram = c(ngram_min = 6L, ngram_max = 6L))
        v_vectorizer <- vocab_vectorizer(vocab2)
        dtm2 <- create_dtm(it1, v_vectorizer)
        
        
        wordsFreq <- colSums(as.matrix(dtm2))
        top6_twitter <- vector2TopDF(wordsFreq[order(wordsFreq, decreasing = T)][1:1000])
        
        
        
        ## clean space
        rm(dtm1,vocab1)
        rm(dtm2,vocab2)
        
        gc(verbose = FALSE)
        gg <- gc(reset = TRUE)
        
        print("Revising five gram")
        fivegram_revised <- top5_twitter %>%
                dplyr::mutate(tempcol = stringr::str_split(features, '_')) %>%
                dplyr::rowwise() %>%
                dplyr::mutate(fivegram_w1 = unlist(tempcol)[1], fivegram_w2 = unlist(tempcol)[2],fivegram_w3 = unlist(tempcol)[3],fivegram_w4 = unlist(tempcol)[4],next_word = unlist(tempcol)[5]) %>%
                dplyr::select(fivegram_w1,fivegram_w2,fivegram_w3,fivegram_w4,next_word,freq)
        
        fivegram_revised <- as.data.frame(fivegram_revised)
        fivegram_revised <- fivegram_revised[!(fivegram_revised$fivegram_w1 == "" | fivegram_revised$fivegram_w2 == "" | fivegram_revised$fivegram_w3 == "" | fivegram_revised$fivegram_w4 == "" | fivegram_revised$next_word == "") , ]
        rm(top5_twitter)
        
        
        print("Revising six gram")
        sixgram_revised <- top6_twitter %>%
                dplyr::mutate(tempcol = stringr::str_split(features, '_')) %>%
                dplyr::rowwise() %>%
                dplyr::mutate(sixgram_w1 = unlist(tempcol)[1], sixgram_w2 = unlist(tempcol)[2],sixgram_w3 = unlist(tempcol)[3],sixgram_w4 = unlist(tempcol)[4],sixgram_w5 = unlist(tempcol)[5],next_word = unlist(tempcol)[6]) %>%
                dplyr::select(sixgram_w1,sixgram_w2,sixgram_w3,sixgram_w4,sixgram_w5,next_word,freq)
        
        sixgram_revised <- as.data.frame(sixgram_revised)
        sixgram_revised <- sixgram_revised[!(sixgram_revised$sixgram_w1 == "" | sixgram_revised$sixgram_w2 == "" | sixgram_revised$sixgram_w3 == "" | sixgram_revised$sixgram_w4 == "" | sixgram_revised$sixgram_w5 == "" | sixgram_revised$next_word == "") , ]
        rm(top6_twitter)
        
        
        print("Writing CSV File")
        
        write.table(fivegram_revised,file= "./data/fivegrams.csv",append=T, row.names=F, col.names=F,  sep=",")
        write.table(sixgram_revised,file= "./data/sixgrams.csv",append=T, row.names=F, col.names=F,  sep=",")
        
        
        rm(fivegram_revised)
        rm(sixgram_revised)
        gc(verbose = FALSE)
        
        stopCluster(cl)
        
}

