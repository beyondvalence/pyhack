# Machine learning Hackers
# Thursday October 1, 2015
# Chapter 3. Classification: spam filtering

wd <- getwd()
(loc <- paste0(wd, "/Documents/R/hackers/data/03-Classification"))
setwd(loc)

# 3.1 Binary classification ####
# this or that, spam or ham

# 3.2 Moving gently into conditional probability ####
# looking to classify emails as spam or ham
# Using the Naive Bayes model for conditional probability
# naive due to assumption of statistical independence
# assumes word count in emails can be estimated in isolation
library(tm)
library(ggplot2)

# set path variables to easy ham, hard ham, spam folders
spam.path <- 'data/spam/'
spam2.path <- 'data/spam_2/'
easyham.path <- 'data/easy_ham/'
easyham2.path <- 'data/easy_ham_2/'
hardham.path <- 'data/hard_ham/'
hardham2.path <- 'data/hard_ham_2/'

# write function that opens email file
# finds first line break
# and returns text as character vector

get.msg <- function(path) {
  con <- file(path, open="rt", encoding="latin1")
  text <- readLines(con)
  # The message always begins after the first full line break
  # so this fn finds the line number where the line break occurs
  # and starts the capture on the next line to the end
  # to return the text message
  msg <- text[seq(which(text=="")[1]+1, length(text),1)]
  close(con)
  return(paste(msg, collapse="\n"))
}

# create vector containing all messages
spam.docs <- dir(spam.path)
spam.docs <- spam.docs[which(spam.docs!="cmds")]
all.spam <- sapply(spam.docs, function(p) get.msg(paste(spam.path,p,sep="")))
