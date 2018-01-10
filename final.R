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
        v <- v[order(v, decreasing = T)][1:10000]
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


#tweet_file <- read_file("en_US.twitter.txt")
#tweet_file <- as.data.frame(tweet_file[rbinom(nrow(tweet_file)*.5,nrow(tweet_file),.5), ])

blog_file <- read_file("en_US.blogs.txt")
#blog_file <- as.data.frame(blog_file[rbinom(nrow(blog_file)*.5,nrow(blog_file),.5), ])

#news_file <- read_file("en_US.news.txt")
#news_file <- as.data.frame(news_file[rbinom(nrow(news_file)*.5,nrow(news_file),.5), ])

#colnames(tweet_file) <- c("text")
colnames(blog_file) <- c("text")
#colnames(news_file) <- c("text")
#temp_file <- rbind(tweet_file,blog_file)
#twitter_file <- rbind(temp_file,news_file)

twitter_file <- blog_file

#rm(tweet_file, blog_file, news_file,temp_file)
gc(verbose = FALSE)



#news_file <- read_file("en_US.news.txt")
#blog_file <- read_file("en_US.blogs.txt")



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




chunk <- 100000
n <- nrow(twitter_file)
r  <- rep(1:ceiling(n/chunk),each=chunk)[1:n]
file_split <- split(twitter_file,r)

rm(twitter_file)
gc(verbose = FALSE)

unigram_twitter <- data.frame(features = as.factor(character()),freq = numeric(0))
bigram_twitter <- data.frame(features = as.factor(character()),freq = numeric(0))
trigram_twitter <- data.frame(features = as.factor(character()),freq = numeric(0))
fourgram_twitter <- data.frame(features = as.factor(character()),freq = numeric(0))

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
        it1 <- itoken(tokens_twitter, progessbar = FALSE,n_chunks = 10000)
        ## Generate the vocabulary using the iterator, 1-gram badwords / stopwords list.
        
        vocab1 <- create_vocabulary(it1, ngram = c(ngram_min = 1L, ngram_max = 1L))
        
        
        
        ## We need to reinitialise iterator : it
        it1 <- itoken(tokens_twitter,n_chunks = 10000)
        ## Here we create dtm directly:
        v_vectorizer <- vocab_vectorizer(vocab1)
        #{ sink("/dev/null"); dtm1 <- create_dtm(it, v_vectorizer); sink()}
        dtm1 <- create_dtm(it1, v_vectorizer)
        
        
        print("Creating unigram")
        ## Extract the top 1000 most frequent concepts
        wordsFreq <- colSums(as.matrix(dtm1))
        top1_twitter <- vector2TopDF(wordsFreq[order(wordsFreq, decreasing = T)][1:10000])
        
        
        
        print("Creating bigram")
        #bigram
        vocab2 <- create_vocabulary(it1, ngram = c(ngram_min = 2L, ngram_max = 2L))
        v_vectorizer <- vocab_vectorizer(vocab2)
        dtm2 <- create_dtm(it1, v_vectorizer)
        
        
        wordsFreq <- colSums(as.matrix(dtm2))
        top2_twitter <- vector2TopDF(wordsFreq[order(wordsFreq, decreasing = T)][1:10000])
        
        
        print("Creating Trigram")
        #trigram
        vocab3 <- create_vocabulary(it1, ngram = c(ngram_min = 3L, ngram_max = 3L))
        v_vectorizer <- vocab_vectorizer(vocab3)
        dtm3 <- create_dtm(it1, v_vectorizer)
        
        
        wordsFreq <- colSums(as.matrix(dtm3))
        top3_twitter <- vector2TopDF(wordsFreq[order(wordsFreq, decreasing = T)][1:10000])
        
        
        print("Creating four gram")
        #fourgram
        vocab4 <- create_vocabulary(it1, ngram = c(ngram_min = 4L, ngram_max = 4L))
        v_vectorizer <- vocab_vectorizer(vocab4)
        dtm4 <- create_dtm(it1, v_vectorizer)
        
        
        wordsFreq <- colSums(as.matrix(dtm4))
        top4_twitter <- vector2TopDF(wordsFreq[order(wordsFreq, decreasing = T)][1:1000])
        
        
        
        ## clean space
        rm(dtm1,vocab1)
        rm(dtm2,vocab2)
        rm(dtm3,vocab3)
        rm(dtm4,vocab4)
        gc(verbose = FALSE)
        gg <- gc(reset = TRUE)
        
        print("Revising four gram")
        fourgram_revised <- top4_twitter %>%
                dplyr::mutate(tempcol = stringr::str_split(features, '_')) %>%
                dplyr::rowwise() %>%
                dplyr::mutate(fourgram_w1 = unlist(tempcol)[1], fourgram_w2 = unlist(tempcol)[2],fourgram_w3 = unlist(tempcol)[3],next_word = unlist(tempcol)[4]) %>%
                dplyr::select(fourgram_w1,fourgram_w2,fourgram_w3,next_word,freq)
        
        fourgram_revised <- as.data.frame(fourgram_revised)
        fourgram_revised <- fourgram_revised[!(fourgram_revised$fourgram_w1 == "" | fourgram_revised$fourgram_w2 == "" | fourgram_revised$fourgram_w3 == "" | fourgram_revised$next_word == "") , ]
        rm(top4_twitter)
        
        
        print("Revising trigram")
        trigram_revised <- top3_twitter %>%
                dplyr::mutate(tempcol = stringr::str_split(features, '_')) %>%
                dplyr::rowwise() %>%
                dplyr::mutate(trigram_w1 = unlist(tempcol)[1], trigram_w2 = unlist(tempcol)[2],next_word = unlist(tempcol)[3]) %>%
                dplyr::select(trigram_w1,trigram_w2,next_word,freq)
        
        trigram_revised <- as.data.frame(trigram_revised)
        trigram_revised <- trigram_revised[!(trigram_revised$trigram_w1 == "" | trigram_revised$trigram_w2 == "" | trigram_revised$next_word == "") , ]
        rm(top3_twitter)
        
        
        print("Revising bigram")
        bigram_revised <- top2_twitter %>%
                dplyr::mutate(tempcol = stringr::str_split(features, '_')) %>%
                dplyr::rowwise() %>%
                dplyr::mutate(bigram_w1 = unlist(tempcol)[1], next_word = unlist(tempcol)[2]) %>%
                dplyr::select(bigram_w1,next_word,freq)
        
        bigram_revised <- as.data.frame(bigram_revised)
        bigram_revised <- bigram_revised[!(bigram_revised$bigram_w1 == "" | bigram_revised$next_word == "") , ]
        rm(top2_twitter)
        
        
        
        print("Writing CSV File")
        
        write.table(top1_twitter,file= "./data/unigrams.csv",append=T, row.names=F, col.names=F,  sep=",")
        write.table(bigram_revised,file= "./data/bigrams.csv",append=T, row.names=F, col.names=F,  sep=",")
        write.table(trigram_revised,file= "./data/trigrams.csv",append=T, row.names=F, col.names=F,  sep=",")
        write.table(fourgram_revised,file= "./data/fourgrams.csv",append=T, row.names=F, col.names=F,  sep=",")
        
        rm(top1_twitter)
        #rm(bigram_twitter)
        #rm(trigram_twitter)
        rm(bigram_revised)
        rm(trigram_revised)
        rm(fourgram_revised)
        gc(verbose = FALSE)
        
        stopCluster(cl)
        
}






# Smoothing for unseen word in our corpus compared with total no. of english words in universe
# Do smoothing using stpid backoff and keep it ready for prediction in csv file
#N <- sum(unigram_twitter$freq)
#unigram_revised <- unigram_twitter %>% rowwise() %>% mutate(score = freq/N)

#https://rpubs.com/DMW29/194887



        
# Beging and end of file not considered