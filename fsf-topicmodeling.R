
# Read CSV into a data frame.
posts <- read.csv("~/work/code/courses/mooc/fsf-coursera/converted.csv")

# Change all strings in posts from factors to strings
posts <- data.frame(lapply(posts, as.character), stringsAsFactors=FALSE)

# Select only the posts with ASCII values.
posts <- posts[sapply(posts[, "post_text"], Encoding) == "unknown",]
posts_text <- posts[, "post_text"]

# Load into corpus
corpus <- Corpus(VectorSource(posts[, "post_text"]))

# Setup document-term matrix to run LDA on.
posts_dtm <- DocumentTermMatrix(corpus, 
        control = list(stemming = TRUE, stopwords = TRUE, minWordLength = 3, removeNumbers
                       = TRUE, removePunctuation = TRUE))

# Calculate tf-idf
term_tfidf <- tapply(posts_dtm$v/row_sums(posts_dtm)[posts_dtm$i], posts_dtm$j, mean) * 
  log2(nDocs(posts_dtm)/col_sums(posts_dtm > 0))

# Remove low tf-idf and zero value rows
posts_dtm <- posts_dtm[, term_tfidf >= 0.1]
posts_dtm <- posts_dtm[row_sums(posts_dtm) > 0,]

summary(col_sums(posts_dtm))

# Training the topic model
k <- 10
seed <- 11121
posts_model <- LDA(posts_dtm, k = k, method = "Gibbs", 
              control = list(seed = seed, burnin = 1000, thin = 100, iter = 1000));

# Top 15 terms for the topics
terms(posts_model, 15)

# Top 5 topics for the first 10 documents
topics(posts_model, 5)[, 1:10]


# Create a user vs topic table
# subset(posts, post_author == "James McCarthy")

docTopicProbs <- posterior(posts_model)$topics

# Returns the most popular topics for a particular
# topic.
# getMostPopularTopicsForDocument(posts_model, 100, 4)
getMostPopularTopicsForDocument <- function(model, documentNumber, numTopics) {
  topics(model, numTopics)[, documentNumber]
}

# getMostPopularWordsForTopic(posts_model, 10, 20)
getMostPopularWordsForTopic <- function(model, topicNumber, numTerms = 10) {
  terms(model, numTerms)[, topicNumber]
}


# Returns the documents with highest probability for a
# particular topic.
getDocumentsForTopic <- function(documents, docTopicProbs, topicNumber, numDocuments = 5) {
  documents[order(docTopicProbs[, topicNumber], decreasing = TRUE)[1:numDocuments]]
}

# Display top 3 posts for topic 3 and 7
getDocumentsForTopic(posts_text, docTopicProbs, 3, 3)
getDocumentsForTopic(posts_text, docTopicProbs, 7, 3)


getTopTopicsForUser <- function(docTopicProbs, documents, user) {
  order(colSums(docTopicProbs[which(posts$post_author == user),]), decreasing = TRUE)
}

unique(posts$post_author)[1:10]
getTopTopicsForUser(docTopicProbs, posts, "chirag sindhu")
getTopTopicsForUser(docTopicProbs, posts, "James McCarthy")

