rm(list=ls())

setwd("C:/Users/Gracy/Coursera - Data Science Specialization/Course 10 - Capstone/Week 1/Coursera-SwiftKey/final/en_US")


library(tm)
library(qdap)
library(shiny)
library(stringr)
library(dplyr)
library(data.table)
library(R.utils)
#READ ACTUAL FILES to PREDICT DIRECTLY

uni_score <- as.data.table(readRDS("uni_score.rds"))
bi_score <- as.data.table(readRDS("bi_score.rds"))
tri_score <- as.data.table(readRDS("tri_score.rds"))
four_score <- as.data.table(readRDS("four_score.rds"))
colnames(uni_score) <- c("unigram_word","uni_freq","score")
colnames(bi_score) <- c("bigram_word1","bigram_word2","bi_freq","score")
colnames(tri_score) <- c("trigram_word1","trigram_word2","trigram_word3","tri_freq","score")
colnames(four_score) <- c("fourgram_word1","fourgram_word2","fourgram_word3","fourgram_word4","four_freq","score")



#setkeyv(bi_file,"bigram_word1")
#setkeyv(tri_file,c("trigram_word1","trigram_word2"))
#setkeyv(four_file,c("fourgram_word1","fourgram_word2","fourgram_word3"))

setkeyv(uni_score,"unigram_word")
setkeyv(bi_score,"bigram_word1")
setkeyv(tri_score,c("trigram_word1","trigram_word2"))
setkeyv(four_score,c("fourgram_word1","fourgram_word2","fourgram_word3"))




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
        t <- gsub(pattern = "u", replacement = "you", t)
        
        
        return(t)
}

predict_word <- function(input){
        
        predicted_word <- uni_prediction
        
        print(input)
        print(word_count(input))
        #clean
        
        input <- replaceShortEnglish(input)
        input <- iconv(input, "latin1", "ASCII", sub="")
        input <- tolower(input)
        input <- removeNumbers(input)
        input <- removePunctuation(input, preserve_intra_word_dashes = TRUE)
        input <- gsub("http[[:alnum:]]*", "", input)
        input <- removeWords(input, stopwords("english"))
        input <- stripWhitespace(input)
        input <- str_trim(input, side = c("both"))
        input <- gsub("\u0092", "'", input)
        input <- gsub("\u0093|\u0094", "", input)
        input <- removePunctuation(input, preserve_intra_word_dashes = FALSE)
        ## Remove back-to-back same words
        input <- gsub("\\b(\\w+) \\1\\b", "\\1", input)
        ## Remove repeated letters when 3 or more are in a row
        input <- gsub("(.)\\1{1,}" ,"\\1", input)
        
        
        print(input)
        if ( grepl("^\\s*$", input) == TRUE) print( predicted_word)
        #print(word_count(input))
        
        #count entered words
        if ( word_count(input) > 3){
                
                query <- word(input,-3,-1)
                print("Actual query")
                print(query)
        }
        else
                query <- input
                print("Actual query")
                print(query)
                
        #find the match
        if ( word_count(query) == 3){
                w <- str_split(query,' ')
                #search the original data frame and get the  3 words ( if available) matching this
                prediction <- four_file[four_file$fourgram_word1 == w[[1]][1] & four_file$fourgram_word2 == w[[1]][2] & four_file$fourgram_word3 == w[[1]][3], ]
                
                print(prediction)
                print("Count of prediction")
                print(nrow(prediction))
                if ( nrow(prediction) > 3){
                        print("results more than 3")
                        # sort by freq and return first three..so get freq too
                        prediction <- prediction %>% arrange(desc(four_freq))
                        prediction <- prediction[1:3,]
                        #return prediction$fourgram_word4
                        print(prediction$fourgram_word4)
                }
                else if ( nrow(prediction) <=3 & nrow(prediction) > 0){
                        print("result 3 or less")
                        n <- nrow(prediction)
                        prediction <- prediction[1:n,]
                        #return prediction$fourgram_word4
                        print(prediction$fourgram_word4)
                        
                }
                else if (nrow(prediction) == 0 ){
                        print("backoff")
                        query <- word(query,2,3)
                        w <- str_split(query,' ')
                        prediction <- tri_score[tri_score$trigram_word1 == w[[1]][1] & tri_score$trigram_word2 == w[[1]][2], ]
                        if ( nrow(prediction) >3 ){
                                prediction <- prediction %>% arrange(desc(score))
                                prediction <- prediction[1:3,]
                                #return prediction$trigram_word3
                                print(prediction$trigram_word3)  
                        }
                        else if ( nrow(prediction) <=3 & nrow(prediction) > 0){
                                n <- nrow(prediction)
                                prediction <- prediction[1:n,]
                                #return prediction$fourgram_word4
                                print(prediction$trigram_word3)
                                
                        }
                        else if ( nrow(prediction) == 0){
                                print("backoff two")
                                query <- word(query,-1)  
                                prediction <- bi_score[bi_score$bigram_word1 == query, ]
                                if ( nrow(prediction) >3 ){
                                        prediction <- prediction %>% arrange(desc(score))
                                        prediction <- prediction[1:3,]
                                        #return prediction$fourgram_word4
                                        print(prediction$bigram_word2)  
                                }
                                else if ( nrow(prediction) <=3 & nrow(prediction) > 0){
                                        n <- nrow(prediction)
                                        prediction <- prediction[1:n,]
                                        #return prediction$fourgram_word4
                                        print(prediction$bigram_word2)
                                        
                                }
                                else if ( nrow(prediction) == 0){
                                        #most freq occuring unigram
                                        prediction <- uni_score[1:3,]
                                        print(prediction$unigram_word)
                                }
                        }
                        #go to next score trigram df and repeat the same..n if found sort by score and return the first three
                        #if not found ..go to bi gram n if found sort by score and return the first three
                        # if not found in bigram..return the most possible unigram 
                }
                #if word_count(query == 2)
                # search in original trigram df
        }
        else if ( word_count(query) == 2){
                w <- str_split(query,' ')
                prediction <- tri_file[tri_file$trigram_word1 == w[[1]][1] & tri_file$trigram_word2 == w[[1]][2], ]
                print(prediction$trigram_word3)
                print("Count of prediction")
                print(nrow(prediction))
                if ( nrow(prediction) > 3){
                        print("results more than 3")
                        # sort by freq and return first three..so get freq too
                        prediction <- prediction %>% arrange(desc(tri_freq))
                        prediction <- prediction[1:3,]
                        #return prediction$fourgram_word4
                        print(prediction$trigram_word3)
                }
                else if ( nrow(prediction) <=3 & nrow(prediction) > 0){
                        print("result 3 or less")
                        n <- nrow(prediction)
                        prediction <- prediction[1:n,]
                        #return prediction$fourgram_word4
                        print(prediction$trigram_word3)
                }  
                else if (nrow(prediction) == 0 ){
                        print("backoff")
                        query <- word(query,-1)
                        prediction <- bi_score[bi_score$bigram_word1 == query, ]
                        if ( nrow(prediction) >3 ){
                                prediction <- prediction %>% arrange(desc(score))
                                prediction <- prediction[1:3,]
                                #return prediction$fourgram_word4
                                print(prediction$bigram_word2)  
                        }
                        else if ( nrow(prediction) <=3 & nrow(prediction) > 0){
                                n <- nrow(prediction)
                                prediction <- prediction[1:n,]
                                #return prediction$fourgram_word4
                                print(prediction$bigram_word2)
                                
                        }
                        else if ( nrow(prediction) == 0){
                                #most freq occuring unigram
                                prediction <- uni_score[1:3,]
                                print(prediction$unigram_word)
                        }
                }
                
                
        }
        else if ( word_count(query) == 1){
                prediction <- bi_file[bi_file$bigram_word1 == query, ]
                print(prediction$bigram_word2)
                print("Count of prediction")
                print(nrow(prediction))
                if ( nrow(prediction) > 3){
                        print("results more than 3")
                        # sort by freq and return first three..so get freq too
                        prediction <- prediction %>% arrange(desc(bi_freq))
                        prediction <- prediction[1:3,]
                        #return prediction$fourgram_word4
                        print(prediction$bigram_word2)
                }
                else if ( nrow(prediction) <=3 & nrow(prediction) > 0){
                        print("result 3 or less")
                        n <- nrow(prediction)
                        prediction <- prediction[1:n,]
                        #return prediction$fourgram_word4
                        print(prediction$bigram_word2)
                }  
                else if (nrow(prediction) == 0 ){
                        print("backoff")
                        prediction <- uni_score[1:3,]
                        print(prediction$unigram_word)
                        
                }
                
        }
        else
                print("Enter a valid word")
        prediction
}
