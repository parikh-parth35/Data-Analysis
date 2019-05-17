# reading the file

ind <- read.csv("F:\\Parth Parikh\\us\\NYU\\2nd semester\\Business Analytics\\Project 3\\youtube dataset\\INvideos.csv")



###############
#Data Cleaning
###############

install.packages("Hmisc")
library("Hmisc")

install.packages("corrplot")
library("corrplot")

try <- ind[, 8:11]
class(try)
try <- as.matrix(try)
p <- rcorr(as.matrix(try))
p

mydata.cor <- cor(try)
corrplot(mydata.cor, type = "lower")

# removing duplicate video_id
library("dplyr")
temp <- ind %>% distinct(video_id, .keep_all = T)

temp$score <- (0.2*temp$likes) + (0.2*temp$comment_count) + (0.6*temp$views)
temp$score <- temp$score/100

# sorting the data frame in decreasing order of scores
temp <- temp[order(temp$score, decreasing = T), ]

summary(temp$score)


###############
# Text Analysis
###############

# Text analysis for video title
library(tm)
library(quanteda)


temp$title <- gsub("'", "", temp$title) # remove apostrophes
temp$title <- gsub("[[:punct:]]", " ", temp$title)  # replace punctuation with space
temp$title <- gsub("[[:cntrl:]]", " ", temp$title)  # replace control characters with space
temp$title <- gsub("^[[:space:]]+", "", temp$title) # remove whitespace at beginning of documents
temp$title <- gsub("[[:space:]]+$", "", temp$title) # remove whitespace at end of documents
temp$title <- gsub("[^a-zA-Z -]", " ", temp$title) # allows only letters
temp$title <- tolower(temp$title)  # force to lowercase
temp$title <- gsub("(?<=[\\s])\\s*|^\\s+|\\s+$", "", temp$title, perl=TRUE) # removing all kind of white spaces and removing it

sum((temp$title == ""))

temp <- temp[!(temp$title == ""), ]

# grouping it according to the quartile values
group1 <- subset(temp, score >= 914.16)
group2 <- subset(temp, score < 914.16)

?corpus

# creating corpus according to the groups

group1_title_corpus <- corpus(group1$title,
                              docnames = group1$video_id,
                              docvar = data.frame(views = group1$views, 
                                                  likes = group1$likes,
                                                  dislikes = group1$dislikes,
                                                  comment_count = group1$comment_count,
                                                  comment_disabled = group1$comments_disabled,
                                                  ratings_disabled = group1$ratings_disabled))

names(group1_title_corpus)
summary(group1_title_corpus)

# viewing the richness of the docs
doc.df_group1_title <- group1_title_corpus$documents
token.df_group1_title <- count.fields(textConnection(doc.df_group1_title$texts))
doc.df_group1_title$Tokens <- token.df_group1_title

library(ggplot2)
group1_title_tockenplot <- ggplot(data = doc.df_group1_title, aes(x = Tokens))
group1_title_tockenplot + geom_histogram(binwidth = 20) + ylab("Distribution of tokens")


# creating a document feature matrix
help(dfm)
group1_title_dfm <- dfm(group1_title_corpus, 
                        remove = stopwords("english"), 
                        verbose=TRUE, 
                        stem=FALSE)

topfeatures(group1_title_dfm, n = 100)

group1_swlist <- c("th", "full", "episode", "best", "ep", "may", "etv", "march", "april",
                   "no", "february", "december", "january", "november", "june", "hd",
                   "s", "ep")

group1_title_dfm <- dfm(group1_title_corpus, 
                        remove = c(group1_swlist, stopwords("english")), 
                        verbose=TRUE, 
                        stem=FALSE)
topfeatures(group1_title_dfm, n = 100)

# evaluating sparcity and removing the words that does not appear in at least 2 docs
group1_title_dfm.tm <- convert(group1_title_dfm, to="tm")
group1_title_dfm.tm  
group1_title_dfm.sparse <- removeSparseTerms(group1_title_dfm.tm, 0.99)
group1_title_dfm.sparse


#exploration in context
tokens <- as.tokens()
kwic(group1_title_corpus, "telugu", 2)

kwic(group1_title_corpus , "sharma", window = 3)
kwic(group1_title_corpus , "vs", window = 3)

# Sentiment Analysis for group1_title
mydict <- dictionary(list(negative = c("detriment*", "bad*", "awful*", "terrib*", "horribl*"),
                          postive = c("good", "great", "super*", "excellent", "yay")))

group1_title_sentiment <- dfm(group1_title_corpus, 
                              remove = c(group1_swlist, stopwords("english")), 
                              verbose=TRUE, 
                              dictionary = mydict,
                              stem=FALSE)
topfeatures(group1_title_sentiment)
View(group1_title_sentiment)

# forming the word cloud for group1_title

install.packages("wordcloud")
library("wordcloud")
set.seed(420)   #keeps cloud' shape fixed
dark2 <- brewer.pal(8, "Set1")   
freq <- topfeatures(group1_title_dfm, n=500)

wordcloud(names(freq), 
          freq, max.words = 50, 
          scale = c(3, 0.1), 
          colors = brewer.pal(8, "Set1"))

#specifying a correlation limit of 0.5   
findAssocs(group1_title_dfm.tm, 
           c("india", "sharma", "vs"), 
           corlimit = 0.4)


# Topic Modeling for group1_title
install.packages("stm")
library(stm)

#Process the data for analysis.
help("textProcessor")
textpro <- textProcessor(documents = group1$title, metadata = group1)
names(textpro)  # produces:  "documents", "vocab", "meta", "docs.removed" 
meta <- textpro$meta
vocab <- textpro$vocab
docs <- textpro$documents
out <- prepDocuments(docs, vocab, meta)
docs <- out$documents
vocab <- out$vocab
meta <- out$meta


#running stm for top 20 topics
help("stm")
prevfit <- stm(docs, vocab, 
               K=20, 
               verbose = TRUE,
               data = meta, 
               max.em.its=50)

topics <- labelTopics(prevfit , topics=c(1:20))
topics   #shows topics with highest probability words

#exploring the topics in context.  Provides an example of the text 
help("findThoughts")

z <- group1$title
z <- z[1:7791]
length(z)

findThoughts(prevfit, texts = z,  topics = 10,  n = 2)

help("plot.STM")
plot.STM(prevfit, type = "summary")
plot.STM(prevfit, type = "labels", topics = c(17, 19, 5, 14, 18, 15, 16, 8))
plot.STM(prevfit, type = "perspectives", topics = c(5, 14))
plot.STM(prevfit, type = "perspectives", topics = c(15, 19))

# to aid on assigment of labels & intepretation of topics
help(topicCorr)
mod.out.corr <- topicCorr(prevfit)  #Estimates a graph of topic correlations

install.packages("igraph")
library(igraph)

plot(mod.out.corr)
