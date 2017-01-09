library(tm)
library(SnowballC)

setwd("D:/Work2015/Projects/iTravel/R/Data")

#Read data
travel.data <- read.csv("iTravelCRData.csv",header = TRUE, stringsAsFactors = FALSE) 
#head(travel.data)

prob.text <- travel.data$Description

prob.source <- VectorSource(prob.text)
corpus <- Corpus(prob.source)
corpus.names <- travel.data$Request.No 
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


query.vector <- as.matrix(tfidf.matrix[, 11])
tfidf.matrix <- as.matrix(tfidf.matrix[, 1:10])

# DOT Product
doc.scores <- t(query.vector) %*% tfidf.matrix

results.df <- data.frame(doc = corpus.names[-11], score = t(doc.scores), text = travel.data$source.code[-11])
results.df <- results.df[order(results.df$X11, decreasing = TRUE), ]


options(width = 2000)
print(results.df, row.names = FALSE, right = FALSE, digits = 2)


#inspect(query.vector[0:20,])
#findFreqTerms(dtm, lowfreq = 5)


