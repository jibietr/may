## Experiments results

# load libraries
require(tm)
require(SnowballC)
require(Metrics)
require(randomForest)

# load data
train <- read.table('../../data/clean/train.csv',header=TRUE,sep=",",stringsAsFactors=FALSE)

# compute features
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
  similarity <- sum(tf_matrix[1,]>0 & tf_matrix[2,]>0)/dim(tf_matrix)[2] # number of words present in query which are also present in response
  similarity
}

# compute feature as a similarity 
RankNorm <- function(x){
 x <- rank(x)
 ans <- x/max(x)
 ans
}

train <- adply(train,1,transform,similarity=matching.words(query,product_description))

# train predictor
train$norm.similarity <- RankNorm(train$similarity)
train$median_relevance <- as.factor(train$median_relevance)
train.rf <- randomForest(median_relevance~norm.similarity,train)

# measure	
ret <- ScoreQuadraticWeightedKappa(train.rf$predicted,train.rf$y,1,4)
print(ret)
#print('save RF')
#save(train.rf,"train-rf.RData")

#train$pred.scores2 = ifelse(train$similarity==0,1,ifelse(train$similarity>=train$n_keywords,4,ifelse(train$similarity==1,2,3)))


