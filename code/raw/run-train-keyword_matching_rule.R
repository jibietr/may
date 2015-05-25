## Experiments results

# load libraries
require(tm)
require(SnowballC)
require(Metrics)

train <- read.csv('../../data/clean/train.csv')

## function to compute matching word
matching.words <- function(x,y){
  corpus <- Corpus(VectorSource(list(x, y))) ## Create corpus of 2 documents
  # TOCHECK: is this the most adequate processing?
  corpus <- tm_map(corpus, content_transformer(tolower))
  corpus <- tm_map(corpus, removePunctuation)
  corpus <- tm_map(corpus, removeWords, stopwords("english"))
  corpus <- tm_map(corpus, stemDocument)
  frequencies <- DocumentTermMatrix(corpus)
  tf_matrix <- as.matrix(frequencies)
  # normalize similarity to vector length
  similarity <- sum(tf_matrix[1,]>0 & tf_matrix[2,]>0) # number of words present in query which are also present in response
  similarity
}


train <- adply(train,1,transform,similarity=matching.words(query,product_title))

# train predictor
train$predicted= ifelse(train$similarity==0,1,ifelse(train$similarity>=train$n_keywords,4,ifelse(train$similarity==1,2,3)))

# measure	
ret <- ScoreQuadraticWeightedKappa(train$predicted,train$median_relevance,1,4)
# this outputs 0.4339351
print(ret)



