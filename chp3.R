# Machine learning Hackers
# Thursday October 1, 2015
# Chapter 3. Classification: spam filtering

wd <- "C:/Users/wayne/Documents/R/hackers/data"
(loc <- paste0(wd, "/03-Classification"))
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

# 3.2.1 write function that opens email file ####
# finds first line break
# and returns text as character vector

get.msg <- function(path) {
  cat("create connection\n")
  con <- file(path, open="rt") # need to remove encoding specification
  cat("\topen file connection\n")
  text <- readLines(con)
  # The message always begins after the first full line break
  # so this fn finds the line number where the line break occurs
  # and starts the capture on the next line to the end
  # for each line with seq()
  # and returns resulting text message
  cat("\t\ttext separation\n")
  msg <- text[seq(which(text=="")[1]+1,length(text),by=1)]
  cat("\t\t\tclosing file connection\n")
  close(con)
  return(paste(msg, collapse="\n"))
}

# 3.2.2 create vector containing all messages ####
spam.docs <- dir(spam.path)
spam.docs <- spam.docs[which(spam.docs!="cmds" & spam.docs!="00136.faa39d8e816c70f23b4bb8758d8a74f0")]
all.spam <- sapply(spam.docs, function(p) get.msg(paste(spam.path,p,sep="")))

# 3.2.3 create TDM ####
get.tdm <- function(doc.vec) {
  # construct corpus using vector of spam emails
  doc.corpus <- Corpus(VectorSource(doc.vec))
  # specify options to clean the text
  control <- list(stopwords=TRUE, removePunctuation=TRUE, 
                  removeNumbers=TRUE, minDocFreq=2)
  doc.dtm <- TermDocumentMatrix(doc.corpus, control)
  return(doc.dtm)
}
spam.tdm <- get.tdm(all.spam)

# 3.2.4 build training data set ####
spam.matrix <- as.matrix(spam.tdm)
spam.counts <- rowSums(spam.matrix)
spam.df <- data.frame(cbind(names(spam.counts),
                            as.numeric(spam.counts)),
                            stringsAsFactors=FALSE)
names(spam.df) <- c("term", "freq")
spam.df$freq <- as.numeric(spam.df$freq)
spam.occurrence <- sapply(1:nrow(spam.matrix),
                          function(i) 
                            {length(which(spam.matrix[i,]>0))/ncol(spam.matrix)})
spam.density <- spam.df$freq/sum(spam.df$freq)
spam.df <- transform(spam.df, 
                     density=spam.density, 
                     occurrence=spam.occurrence)