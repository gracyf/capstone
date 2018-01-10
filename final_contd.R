#read files into data frame
# group by the words and sum freq
# calc prob for those ngram
rm(list=ls())

library(dplyr)
library(stringr)
library(doParallel)


setwd("C:/Users/Gracy/Coursera - Data Science Specialization/Course 10 - Capstone/Week 1/Coursera-SwiftKey/final/en_US")

ncores <- detectCores() - 1
# Initiate cluster
cl <- makeCluster(ncores)
registerDoParallel(cl)


uni_file <- read.csv("./data/unigrams.csv",header = F)
bi_file <- read.csv("./data/bigrams.csv",header = F)
tri_file <- read.csv("./data/trigrams.csv",header = F)
four_file <- read.csv("./data/fourgrams.csv", header = F)

five_file <- read.csv("./data/fivegrams.csv",header = F)
six_file <- read.csv("./data/sixgrams.csv",header = F)


colnames(uni_file) <- c("unigram_word","uni_freq")
colnames(bi_file) <- c("bigram_word1","bigram_word2","bi_freq")
colnames(tri_file) <- c("trigram_word1","trigram_word2","trigram_word3","tri_freq")
colnames(four_file) <- c("fourgram_word1","fourgram_word2","fourgram_word3","fourgram_word4","four_freq")
colnames(five_file) <- c("fivegram_word1","fivegram_word2","fivegram_word3","fivegram_word4","fivegram_word5","five_freq")
colnames(six_file) <- c("sixgram_word1","sixgram_word2","sixgram_word3","sixgram_word4","sixgram_word5","sixgram_word6","six_freq")

#Remove rows if it has single characters 

uni_file <- uni_file[!(str_length(uni_file$unigram_word)) == 1, ]
bi_file <- bi_file[!((str_length(bi_file$bigram_word1) == 1) | (str_length(bi_file$bigram_word2) == 1)), ]
tri_file <- tri_file[!((str_length(tri_file$trigram_word1) == 1) | (str_length(tri_file$trigram_word2) == 1) | (str_length(tri_file$trigram_word3) == 1)), ]
four_file <- four_file[!((str_length(four_file$fourgram_word1) == 1) | (str_length(four_file$fourgram_word2) == 1) | (str_length(four_file$fourgram_word3) == 1) | (str_length(four_file$fourgram_word4) == 1)), ]

five_file <- five_file[!((str_length(five_file$fivegram_word1) == 1) |  (str_length(five_file$fivegram_word2) == 1) | (str_length(five_file$fivegram_word3) == 1) | (str_length(five_file$fivegram_word4) == 1) | (str_length(five_file$fivegram_word5) == 1)), ]
six_file <- six_file[!((str_length(six_file$sixgram_word1) == 1) | (str_length(six_file$sixgram_word2) == 1) | (str_length(six_file$sixgram_word3) == 1) | (str_length(six_file$sixgram_word4) == 1) | (str_length(six_file$sixgram_word5) == 1) | (str_length(six_file$sixgram_word6) == 1)), ]



#Group same words as there are from different chunkcs
uni_file <- uni_file %>% group_by(unigram_word) %>% summarise(sum(uni_freq))
bi_file <- bi_file %>% group_by(bigram_word1,bigram_word2) %>% summarise(sum(bi_freq))
tri_file <- tri_file %>% group_by(trigram_word1,trigram_word2,trigram_word3) %>% summarise(sum(tri_freq))
four_file <- four_file %>% group_by(fourgram_word1,fourgram_word2,fourgram_word3,fourgram_word4) %>% summarise(sum(four_freq))

five_file <- five_file %>% group_by(fivegram_word1,fivegram_word2,fivegram_word3,fivegram_word4,fivegram_word5) %>% summarise(sum(five_freq))
six_file <- six_file %>% group_by(sixgram_word1,sixgram_word2,sixgram_word3,sixgram_word4,sixgram_word5,sixgram_word6) %>% summarise(sum(six_freq))

#Reset col names again
colnames(uni_file) <- c("unigram_word","uni_freq")
colnames(bi_file) <- c("bigram_word1","bigram_word2","bi_freq")
colnames(tri_file) <- c("trigram_word1","trigram_word2","trigram_word3","tri_freq")
colnames(four_file) <- c("fourgram_word1","fourgram_word2","fourgram_word3","fourgram_word4","four_freq")

colnames(five_file) <- c("fivegram_word1","fivegram_word2","fivegram_word3","fivegram_word4","fivegram_word5","five_freq")
colnames(six_file) <- c("sixgram_word1","sixgram_word2","sixgram_word3","sixgram_word4","sixgram_word5","sixgram_word6","six_freq")


saveRDS(uni_file,file = "./nextwordpredictor/unigram.rds")
saveRDS(bi_file,file = "./nextwordpredictor/bigram.rds")
saveRDS(tri_file,file = "./nextwordpredictor/trigram.rds")
saveRDS(four_file, file = "./nextwordpredictor/fourgram.rds")

saveRDS(five_file, file = "./nextwordpredictor/fivegram.rds")
saveRDS(six_file, file = "./nextwordpredictor/sixgram.rds")

#SMOOTHING

#SMOOTHING
#What to do if no 4-gram begins with the input trigram.
#In this case one approach is to trim the input down to 2 words by deleting the first word. 
#Then we can check if any 3-gram in the corpus starts with the 2 words. The last word in the 3-gram 
#is a candidate for a guess. However, we can score it so we can compare it to other candidates. 
#This is process is called the back-off. It ends with the worse scenario - the input has never been seen in the corpus. 
#In this case you can return the most common 1-gram.


total_unigram <- 102059900 #Single words In the whole corpus

uni_score <- uni_file %>%  rowwise %>% mutate( score = uni_freq/total_unigram ) %>% arrange(desc(score)) %>% select(unigram_word,uni_freq,score)

merge_file1 <- merge(uni_file,bi_file,by.x="unigram_word",by.y="bigram_word1") # If needed we can keep it or remove asap

rm (uni_file)
gc(verbose = FALSE)

bi_score <- merge_file1 %>% rowwise %>% mutate( score = 0.4 * ( bi_freq/uni_freq)) %>% select(unigram_word,bigram_word2,bi_freq,score)
colnames(bi_score) <- c("bigram_word1","bigram_word2","bi_freq","score")

merge_file2 <- merge(bi_file,tri_file,by.x=c("bigram_word1","bigram_word2"), by.y = c("trigram_word1","trigram_word2"))
rm (bi_file)
gc(verbose = FALSE)

tri_score <- merge_file2 %>% rowwise %>% mutate( score = 0.16 * (tri_freq/bi_freq)) %>% select(bigram_word1,bigram_word2,trigram_word3,tri_freq,score)
colnames(tri_score) <- c("trigram_word1","trigram_word2","trigram_word2","tri_freq","score")

merge_file3 <- merge(tri_file,four_file,by.x=c("trigram_word1","trigram_word2","trigram_word3"), by.y = c("fourgram_word1","fourgram_word2","fourgram_word3"))

rm (tri_file)
gc(verbose = FALSE)

four_score <- merge_file3 %>% rowwise %>% mutate( score = 0.064 * (four_freq/tri_freq)) %>% select(trigram_word1,trigram_word2,trigram_word3,fourgram_word4,four_freq,score)
colnames(four_score) <- c("fourgram_word1","fourgram_word2","fourgram_word3","fourgram_word4","four_freq","score")


merge_file4 <- merge(four_file,five_file,by.x=c("fourgram_word1","fourgram_word2","fourgram_word3","fourgram_word4"), by.y = c("fivegram_word1","fivegram_word2","fivegram_word3","fivegram_word4"))

rm (four_file)
gc(verbose = FALSE)

five_score <- merge_file4 %>% rowwise %>% mutate( score = 0.4 * 0.064 * (five_freq/four_freq)) %>% select(fourgram_word1,fourgram_word2,fourgram_word3,fourgram_word4,fivegram_word5,five_freq,score)
colnames(five_score) <- c("fivegram_word1","fivegram_word2","fivegram_word3","fivegram_word4","fivegram_word5" ,"five_freq","score")

rm (five_file)
gc(verbose = FALSE)

merge_file5 <- merge(five_file,six_file,by.x=c("fivegram_word1","fivegram_word2","fivegram_word3","fivegram_word4","fivegram_word5"), by.y = c("sixgram_word1","sixgram_word2","sixgram_word3","sixgram_word4","sixgram_word5"))

rm (five_file)
gc(verbose = FALSE)

six_score <- merge_file5 %>% rowwise %>% mutate( score = 0.4 * 0.4 * 0.064 * (six_freq/five_freq)) %>% select(fivegram_word1,fivegram_word2,fivegram_word3,fivegram_word4,fivegram_word5,sixgram_word6,six_freq,score)
colnames(six_score) <- c("sixgram_word1","sixgram_word2","sixgram_word3","sixgram_word4","sixgram_word5","sixgram_word6" ,"six_freq","score")

rm (six_file)
gc(verbose = FALSE)






saveRDS(uni_score, file = "./nextwordpredictor/uni_score.rds")
saveRDS(bi_score, file = "./nextwordpredictor/bi_score.rds")
saveRDS(tri_score, file = "./nextwordpredictor/tri_score.rds")
saveRDS(four_score, file = "./nextwordpredictor/four_score.rds")
saveRDS(five_score, file = "./nextwordpredictor/five_score.rds")
saveRDS(six_score, file = "./nextwordpredictor/six_score.rds")


rm (uni_score)
rm (bi_score)
rm (tri_score)
rm (four_score)

rm(five_score)
rm(six_score)

gc(verbose = FALSE)

stopCluster(cl)

#In our example, "this is a car", the base of the 4-gram_X is the 3-gram "this is a".
#So your score would be the frequency of "this is a car" divided by the frequencyi of "this is a".

#w1 < "hi how are you"
#s1 <- 10
#w2 <- "hi how are"
#s2 <- 15+
#w3 <- "hi how"
#w4 <- "hi"

#score(w1) = s1/s2





