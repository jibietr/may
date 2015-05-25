
# remove product description from original data

# load data
train <- read.table('../../data/original/train.csv',header=TRUE,sep=",",stringsAsFactors=FALSE)

train <- subset(train,select=-c(product_description))

file.name <- '../../data/original/train-no-description.csv'
write.table(train,file=file.name,row.names=FALSE,col.names=TRUE,sep=",")
