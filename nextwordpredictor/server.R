#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
rm(list=ls())




library(tm)
library(qdap)
library(shiny)
library(stringr)
library(dplyr)
library(data.table)
library(R.utils)

#rsconnect::configureApp("nextwordpredictor", size="large")

#READ ACTUAL FILES to PREDICT DIRECTLY

#uni_file <- read.csv("C:/Users/Gracy/Coursera - Data Science Specialization/Course 10 - Capstone/Week 1/Coursera-SwiftKey/final/en_US/data/unigrams.csv",header = F)
bi_file <- as.data.table(readRDS("bigram.rds"))
tri_file <- as.data.table(readRDS("trigram.rds"))
four_file <- as.data.table(readRDS("fourgram.rds"))

five_file <- as.data.table(readRDS("fivegram.rds"))
six_file <- as.data.table(readRDS("sixgram.rds"))

#colnames(uni_file) <- c("unigram_word","uni_freq")
colnames(bi_file) <- c("bigram_word1","bigram_word2","bi_freq")
colnames(tri_file) <- c("trigram_word1","trigram_word2","trigram_word3","tri_freq")
colnames(four_file) <- c("fourgram_word1","fourgram_word2","fourgram_word3","fourgram_word4","four_freq")

colnames(five_file) <- c("fivegram_word1","fivegram_word2","fivegram_word3","fivegram_word4","fivegram_word5","five_freq")
colnames(six_file) <- c("sixgram_word1","sixgram_word2","sixgram_word3","sixgram_word4","sixgram_word5","sixgram_word6","six_freq")

#READ SMOOTHED FILES 

uni_score <- as.data.table(readRDS("uni_score.rds"))
bi_score <- as.data.table(readRDS("bi_score.rds"))
tri_score <- as.data.table(readRDS("tri_score.rds"))
four_score <- as.data.table(readRDS("four_score.rds"))

five_score <- as.data.table(readRDS("five_score.rds"))
six_score <- as.data.table(readRDS("six_score.rds"))


colnames(uni_score) <- c("unigram_word","uni_freq","score")
colnames(bi_score) <- c("bigram_word1","bigram_word2","bi_freq","score")
colnames(tri_score) <- c("trigram_word1","trigram_word2","trigram_word3","tri_freq","score")
colnames(four_score) <- c("fourgram_word1","fourgram_word2","fourgram_word3","fourgram_word4","four_freq","score")

colnames(five_score) <- c("fivegram_word1","fivegram_word2","fivegram_word3","fivegram_word4","fivegram_word5","score")
colnames(six_score) <- c("sixgram_word1","sixgram_word2","sixgram_word3","sixgram_word4","sixgram_word5","sixgram_word6","score")




setkeyv(bi_file,"bigram_word1")
setkeyv(tri_file,c("trigram_word1","trigram_word2"))
setkeyv(four_file,c("fourgram_word1","fourgram_word2","fourgram_word3"))
setkeyv(five_file,c("fivegram_word1","fivegram_word2","fivegram_word3","fivegram_word4"))
setkeyv(six_file,c("sixgram_word1","sixgram_word2","sixgram_word3","sixgram_word4","sixgram_word5"))


setkeyv(uni_score,"unigram_word")
setkeyv(bi_score,"bigram_word1")
setkeyv(tri_score,c("trigram_word1","trigram_word2"))
setkeyv(four_score,c("fourgram_word1","fourgram_word2","fourgram_word3"))

setkeyv(five_score,c("fivegram_word1","fivegram_word2","fivegram_word3","fivegram_word4"))
setkeyv(six_score,c("sixgram_word1","sixgram_word2","sixgram_word3","sixgram_word4","sixgram_word5"))



uni_score <- uni_score %>% arrange(desc(score))
uni_prediction <- uni_score[3:5,]

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

# Define server logic required to draw a histogram
shinyServer(function(input, output) {
        
        output$nextWord <- renderText({
                req(input$textEntered)
                
                prediction <- uni_prediction
                predicted_word <- as.character(prediction$unigram_word)
                
                inputText <- input$textEntered
                
                inputText <- replaceShortEnglish(inputText)
                inputText <- untwitter(inputText)
                
                #Nothing after clean up 
                if ( grepl("^\\s*$", inputText) == TRUE){
                         query <- "Hi"
                }else
                        query <- inputText
                
                if ( word_count(query) >= 5){
                        query <- word(query,-5,-1)
                        w <- str_split(query,' ')
                        
                        prediction <- six_file[six_file$sixgram_word1 == w[[1]][1] & six_file$sixgram_word2 == w[[1]][2] & six_file$sixgram_word3 == w[[1]][3] & six_file$sixgram_word4 == w[[1]][4] & six_file$sixgram_word5 == w[[1]][5], ]
                        
                        if ( nrow(prediction) > 3){
                                prediction <- prediction %>% arrange(desc(six_freq))
                                prediction <- prediction[1:3,]
                                predicted_word <- as.character(prediction$sixgram_word6)
                        }
                        else if ( nrow(prediction) <=3 & nrow(prediction) > 0){
                                n <- nrow(prediction)
                                prediction <- prediction[1:n,]
                                predicted_word <- as.character(prediction$sixgram_word6)
                        }
                        else if (nrow(prediction) == 0 ){
                                query <- word(query,-4,-1)
                                w <- str_split(query,' ')
                                prediction <- five_score[five_score$fivegram_word1 == w[[1]][1] & five_score$fivegram_word2 == w[[1]][2] & five_score$fivegram_word3 == w[[1]][3] & five_score$fivegram_word4 == w[[1]][4], ]
                                if ( nrow(prediction) >3 ){
                                        prediction <- prediction %>% arrange(desc(score))
                                        prediction <- prediction[1:3,]
                                        predicted_word <- as.character(prediction$fivegram_word5)
                                }
                                else if ( nrow(prediction) <=3 & nrow(prediction) > 0){
                                        n <- nrow(prediction)
                                        prediction <- prediction[1:n,]
                                        predicted_word <- as.character(prediction$fivegram_word5)
                                }
                                else if ( nrow(prediction) == 0){
                                        query <- word(query,-3,-1)  
                                        w <- str_split(query,' ')
                                        prediction <- four_score[four_score$fourgram_word1 == w[[1]][1] & four_score$fourgram_word2 == w[[1]][2] & four_score$fourgram_word3 == w[[1]][3], ]
                                        if ( nrow(prediction) >3 ){
                                                prediction <- prediction %>% arrange(desc(score))
                                                prediction <- prediction[1:3,]
                                                predicted_word <- as.character(prediction$fourgram_word4)
                                        }
                                        else if ( nrow(prediction) <=3 & nrow(prediction) > 0){
                                                n <- nrow(prediction)
                                                prediction <- prediction[1:n,]
                                                predicted_word <- as.character(prediction$fourgram_word4)
                                                
                                        }
                                        else if ( nrow(prediction) == 0){
                                                query <- word(query,-2,-1)
                                                w <- str_split(query,' ')
                                                prediction <- tri_score[tri_score$trigram_word1 == w[[1]][1] & tri_score$trigram_word2 == w[[1]][2], ]
                                                if ( nrow(prediction) >3 ){
                                                        prediction <- prediction %>% arrange(desc(score))
                                                        prediction <- prediction[1:3,]
                                                        predicted_word <- as.character(prediction$trigram_word3)
                                                }
                                                else if ( nrow(prediction) <=3 & nrow(prediction) > 0){
                                                        n <- nrow(prediction)
                                                        prediction <- prediction[1:n,]
                                                        predicted_word <- as.character(prediction$trigram_word3)
                                                }
                                                else if ( nrow(prediction) == 0){
                                                        query <- word(query,-1)  
                                                        prediction <- bi_score[bi_score$bigram_word1 == query, ]
                                                        if ( nrow(prediction) >3 ){
                                                                prediction <- prediction %>% arrange(desc(score))
                                                                prediction <- prediction[1:3,]
                                                                predicted_word <- as.character(prediction$bigram_word2)
                                                        }
                                                        else if ( nrow(prediction) <=3 & nrow(prediction) > 0){
                                                                n <- nrow(prediction)
                                                                prediction <- prediction[1:n,]
                                                                predicted_word <- as.character(prediction$bigram_word2)
                                                                
                                                        }
                                                        else if ( nrow(prediction) == 0){
                                                                #most freq occuring unigram
                                                                prediction <- uni_prediction
                                                                predicted_word <- as.character(prediction$unigram_word)
                                                        }
                                                }
                                        }
                                }
                }
                else if( word_count(query) == 4){
                        query <- word(query,-4,-1)
                        w <- str_split(query,' ')
                        
                        prediction <- five_file[five_file$fivegram_word1 == w[[1]][1] & five_file$fivegram_word2 == w[[1]][2] & five_file$fivegram_word3 == w[[1]][3] & & five_file$fivegram_word4 == w[[1]][4], ]
                        
                        if ( nrow(prediction) >3 ){
                                prediction <- prediction %>% arrange(desc(score))
                                prediction <- prediction[1:3,]
                                predicted_word <- as.character(prediction$fivegram_word5)
                        }
                        else if ( nrow(prediction) <=3 & nrow(prediction) > 0){
                                n <- nrow(prediction)
                                prediction <- prediction[1:n,]
                                predicted_word <- as.character(prediction$fivegram_word5)
                        }
                        else if ( nrow(prediction) == 0){
                                query <- word(query,-3,-1)  
                                w <- str_split(query,' ')
                                prediction <- four_score[four_score$fourgram_word1 == w[[1]][1] & four_score$fourgram_word2 == w[[1]][2] & four_score$fourgram_word3 == w[[1]][3], ]
                                if ( nrow(prediction) >3 ){
                                        prediction <- prediction %>% arrange(desc(score))
                                        prediction <- prediction[1:3,]
                                        predicted_word <- as.character(prediction$fourgram_word4)
                                }
                                else if ( nrow(prediction) <=3 & nrow(prediction) > 0){
                                        n <- nrow(prediction)
                                        prediction <- prediction[1:n,]
                                        predicted_word <- as.character(prediction$fourgram_word4)
                                        
                                }
                                else if ( nrow(prediction) == 0){
                                        query <- word(query,-2,-1)
                                        w <- str_split(query,' ')
                                        prediction <- tri_score[tri_score$trigram_word1 == w[[1]][1] & tri_score$trigram_word2 == w[[1]][2], ]
                                        if ( nrow(prediction) >3 ){
                                                prediction <- prediction %>% arrange(desc(score))
                                                prediction <- prediction[1:3,]
                                                predicted_word <- as.character(prediction$trigram_word3)
                                        }
                                        else if ( nrow(prediction) <=3 & nrow(prediction) > 0){
                                                n <- nrow(prediction)
                                                prediction <- prediction[1:n,]
                                                predicted_word <- as.character(prediction$trigram_word3)
                                        }
                                        else if ( nrow(prediction) == 0){
                                                query <- word(query,-1)  
                                                prediction <- bi_score[bi_score$bigram_word1 == query, ]
                                                if ( nrow(prediction) >3 ){
                                                        prediction <- prediction %>% arrange(desc(score))
                                                        prediction <- prediction[1:3,]
                                                        predicted_word <- as.character(prediction$bigram_word2)
                                                }
                                                else if ( nrow(prediction) <=3 & nrow(prediction) > 0){
                                                        n <- nrow(prediction)
                                                        prediction <- prediction[1:n,]
                                                        predicted_word <- as.character(prediction$bigram_word2)
                                                        
                                                }
                                                else if ( nrow(prediction) == 0){
                                                        #most freq occuring unigram
                                                        prediction <- uni_prediction
                                                        predicted_word <- as.character(prediction$unigram_word)
                                                }
                                        }
                                }
                        }
                        
                }
                else if ( word_count(query) == 3){
                        query <- word(query,-3,-1)
                        w <- str_split(query,' ')
                        
                        prediction <- four_file[four_file$fourgram_word1 == w[[1]][1] & four_file$fourgram_word2 == w[[1]][2] & four_file$fourgram_word3 == w[[1]][3], ]
                        
                        
                        if ( nrow(prediction) > 3){
                                prediction <- prediction %>% arrange(desc(four_freq))
                                prediction <- prediction[1:3,]
                                predicted_word <- as.character(prediction$fourgram_word4)
                        }
                        else if ( nrow(prediction) <=3 & nrow(prediction) > 0){
                                n <- nrow(prediction)
                                prediction <- prediction[1:n,]
                                predicted_word <- as.character(prediction$fourgram_word4)
                        }
                        else if (nrow(prediction) == 0 ){
                                query <- word(query,-2,-1)
                                w <- str_split(query,' ')
                                prediction <- tri_score[tri_score$trigram_word1 == w[[1]][1] & tri_score$trigram_word2 == w[[1]][2], ]
                                if ( nrow(prediction) >3 ){
                                        prediction <- prediction %>% arrange(desc(score))
                                        prediction <- prediction[1:3,]
                                        predicted_word <- as.character(prediction$trigram_word3)
                                }
                                else if ( nrow(prediction) <=3 & nrow(prediction) > 0){
                                        n <- nrow(prediction)
                                        prediction <- prediction[1:n,]
                                        predicted_word <- as.character(prediction$trigram_word3)
                                }
                                else if ( nrow(prediction) == 0){
                                        query <- word(query,-1)  
                                        prediction <- bi_score[bi_score$bigram_word1 == query, ]
                                        if ( nrow(prediction) >3 ){
                                                prediction <- prediction %>% arrange(desc(score))
                                                prediction <- prediction[1:3,]
                                                predicted_word <- as.character(prediction$bigram_word2)
                                        }
                                        else if ( nrow(prediction) <=3 & nrow(prediction) > 0){
                                                n <- nrow(prediction)
                                                prediction <- prediction[1:n,]
                                                predicted_word <- as.character(prediction$bigram_word2)
                                                
                                        }
                                        else if ( nrow(prediction) == 0){
                                                #most freq occuring unigram
                                                prediction <- uni_prediction
                                               predicted_word <- as.character(prediction$unigram_word)
                                        }
                                }
                                
                        }
                        
                }
                else if ( word_count(query) == 2){
                        w <- str_split(query,' ')
                        prediction <- tri_file[tri_file$trigram_word1 == w[[1]][1] & tri_file$trigram_word2 == w[[1]][2], ]
                        
                        if ( nrow(prediction) > 3){
                                prediction <- prediction %>% arrange(desc(tri_freq))
                                prediction <- prediction[1:3,]
                                predicted_word <- as.character(prediction$trigram_word3)
                        }
                        else if ( nrow(prediction) <=3 & nrow(prediction) > 0){
                                n <- nrow(prediction)
                                prediction <- prediction[1:n,]
                                predicted_word <- as.character(prediction$trigram_word3)
                        }  
                        else if (nrow(prediction) == 0 ){
                                query <- word(query,-1)
                                prediction <- bi_score[bi_score$bigram_word1 == query, ]
                                if ( nrow(prediction) >3 ){
                                        prediction <- prediction %>% arrange(desc(score))
                                        prediction <- prediction[1:3,]
                                        predicted_word <- as.character(prediction$bigram_word2)  
                                }
                                else if ( nrow(prediction) <=3 & nrow(prediction) > 0){
                                        n <- nrow(prediction)
                                        prediction <- prediction[1:n,]
                                        predicted_word <- as.character(prediction$bigram_word2)
                                }
                                else if ( nrow(prediction) == 0){
                                        prediction <- uni_prediction
                                        predicted_word <- as.character(prediction$unigram_word)
                                }
                        }
                }
                else if ( word_count(query) == 1){
                        prediction <- bi_file[bi_file$bigram_word1 == query, ]
                        
                        if ( nrow(prediction) > 3){
                                prediction <- prediction %>% arrange(desc(bi_freq))
                                prediction <- prediction[1:3,]
                                predicted_word <- as.character(prediction$bigram_word2)
                        }
                        else if ( nrow(prediction) <=3 & nrow(prediction) > 0){
                                n <- nrow(prediction)
                                prediction <- prediction[1:n,]
                                predicted_word <- as.character(prediction$bigram_word2)
                        }  
                        else if (nrow(prediction) == 0 ){
                                prediction <- uni_prediction
                                predicted_word <- as.character(prediction$unigram_word)
                                
                        }
                        
                }
                else{
                        prediction <- uni_prediction
                        predicted_word <- as.character(prediction$unigram_word)
                        
                }
                predicted_word <- gsub(" ","  ", predicted_word)
                predicted_word
        })
        
        output$sentence <- renderText(input$textEntered)
})
