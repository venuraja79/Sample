library(tm)
library(SnowballC)

setwd("D:/Work2015/Projects/iTravel/R/Data")

#Read data
travel.data <- read.csv("VAText.csv",header = TRUE, stringsAsFactors = FALSE) 
#head(travel.data)

#prob.text <- paste(travel.data$Question," ",travel.data$Answer)
prob.text <- travel.data$Answer

prob.source <- VectorSource(prob.text)
corpus <- Corpus(prob.source)
corpus.names <- travel.data$Sno 

#getTransformations()

#cleaning
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeWords, stopwords("english"))
corpus <- tm_map(corpus, removePunctuation)
#corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, stemDocument)


#Build Doc matrix
dtm <- TermDocumentMatrix(corpus)
tfidf.matrix <- weightTfIdf(dtm)
#term.doc.matrix <- as.matrix(dtm)

query.vector <- as.matrix(tfidf.matrix[, 38])
tfidf.matrix <- as.matrix(tfidf.matrix[, 1:37])

# DOT Product
doc.scores <- t(query.vector) %*% tfidf.matrix

results.df <- data.frame(doc = corpus.names[-37], score = t(doc.scores), text = travel.data$Answer[-37])
results.df <- results.df[order(results.df$X38, decreasing = TRUE), ]


options(width = 2000)
print(results.df[1:3,], row.names = FALSE, right = FALSE, digits = 2)

# Clean the corpus
cleanTextCorpus <- function(textCorpus){
  textCorpus <- tm_map(textCorpus, content_transformer(tolower))
  textCorpus <- tm_map(textCorpus, removePunctuation)
  textCorpus <- tm_map(textCorpus, removeWords, stopwords("english"))
  textCorpus <- tm_map(textCorpus, removePunctuation)
  textCorpus <- tm_map(textCorpus, stemDocument)
  
  return(textCorpus);
}

getTermFrequency <- function(textCorpus){
  return(TermDocumentMatrix(textCorpus))
}

dotProduct <- function(queryVector, tfMatrix){
  scores <- t(queryVector) %*% tfMatrix
  return (scores)
}

#inspect(query.vector[0:20,])
#findFreqTerms(dtm, lowfreq = 5)


