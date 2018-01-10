setwd("C:/Users/Gracy/Coursera - Data Science Specialization/Course 10 - Capstone/Week 1/Coursera-SwiftKey/final/en_US/data_1")



uni_score2 <- read.table("uni_score.csv", sep = ",")
library(data.table)

setkeyv(uni_score, unigram_word)





system("gzip uni_score.csv")
read.table("bigdata-compressed.txt.gz", sep=","))

setwd('C:/temp')
zip(zipfile = 'testZip', files = 'test.txt')

store as zip files and read as data.table and then use setkeyv on col to search for words and get next word

install.packages("Rcompression")

d <- "C:/Users/Gracy/Coursera - Data Science Specialization/Course 10 - Capstone/Week 1/Coursera-SwiftKey/final/en_US/data_1/bi_score.csv"
system("gzip d")
files <- list.files(d, recursive=TRUE)
zip("myarchive.zip", files=paste(d, files, sep="/"), altNames=files)

library(R.utils)
gzip("./data_1/bi_score.csv")

print(file.info("./data_1/bi_score.csv.gz"))


bi_score <- read.table("./data_1/bi_score.csv.gz", sep=",")
colnames(bi_score) <- c("bigram_w1","next_word","freq","score")
setkeyv(bi_score, "bigram_w1")



f <- fread("./data_1/bi_score.csv.gz",sep=',',header = FALSE,data.table = TRUE, col.names = c("bigram_w1","next_word","freq","score"))


gunzip("./data_1/uni_score.rds.gz")


gzip("./data_1/uni_score.rds")

r <- readRDS("./data_1/uni_score.rds.gz")

r <- readRDS("./data_1/uni_score.rds")
r1 <- as.data.table(r)

setkeyv(r1,"unigram_word")
