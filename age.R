#-------------------------------------------------------------------------------
# Age prediction
# author: Aleksey Shipitsyn
#-------------------------------------------------------------------------------
# install.packages("imager")
# install.packages("spatstat")
# install.packages("e1071")
# install.packages("randomForest")

library(imager)
setwd("~/Documents/Competitions/Age")
wd <- getwd()
train.dir <- 'train_DETg9GD'
test.dir <- 'test_Bh8pGW3'

train.ind <- read.csv(file=paste0(train.dir,'/','train.csv'), header=T, stringsAsFactors=F)
table(train.ind$Class)

# MIDDLE    OLD  YOUNG 
#  10804   2396   6706 

#-------------------------------------------------------------------------------
# Undersampling major classes with random selection of major classes instances
# takes the smallest class and undersample all other classes to size of smallest

# input: classvec = vector of class labels
# output: vector of indeces of undersampled classvec

random.undersample <- function(classvec) {
   
   N <- length(classvec)
   tt <- sort(table(classvec), decreasing=T)
   nclass <- length(tt)
   Nmin <- tt[nclass]
   
   # take full minor class
   out <- (1:N)[classvec == attr(tt[nclass], 'names')] 
   
   # sample major classes
   for (i in 1:(nclass-1)) {
      x <- (1:N)[classvec == attr(tt[i], 'names')] 
      out <- c(out, sample(x=x, size=Nmin, replace=F))
   }
   return(out)
}

#-------------------------------------------------------------------------------
# undersample major classes
set.seed(4)
ind <- random.undersample(classvec=train.ind$Class)
train.ind <- train.ind[ind,]
table(train.ind$Class)

# load images to a list
setwd(paste0(train.dir,'/train'))
im.train <- list()
for (i in 1:nrow(train.ind)) {
   im.train[[i]] <- load.image(train.ind$ID[i])
}

# # image dimentions
# dm <- matrix(0, nrow=length(im.train), ncol=2)
# for (i in 1:length(im.train)) {   
#    dm[i, ] <- dim(im.train[[i]])[1:2]
# }
# summary(dm[,1])
# summary(dm[,2])
# 
# par(mfrow=c(1,2))
# hist(dm[,1], main='Width')
# hist(dm[,2], main='Height')

#-------------------------------------------------------------------------------
# Features and target for train set
#-------------------------------------------------------------------------------
# resize images 
size <- 28
for (i in 1:length(im.train)) {
   im.train[[i]] <- resize(im.train[[i]], size_x=size, size_y=size)
}

# histogram equalisation
for (i in 1:length(im.train)) {
   a <- grayscale(im.train[[i]])
   f <- ecdf(a)
   im.train[[i]] <- as.cimg(f(a), dim=dim(a))
}

# features and target for train set
X.train <- matrix(0, ncol=size*size, nrow=length(im.train))
for (i in 1:length(im.train)) {
   X.train[i,] <- as.numeric(im.train[[i]])
}
Y.train <- factor(train.ind$Class)

#-------------------------------------------------------------------------------
# Features and target for test set
#-------------------------------------------------------------------------------
setwd(wd)
test.ind <- read.csv(file=paste0(test.dir,'/','test.csv'), header=T, stringsAsFactors=F)
test.ind$Class <- ""

setwd(paste0(test.dir,'/test'))
im.test <- list()
for (i in 1:nrow(test.ind)) {
   im.test[[i]] <- load.image(test.ind$ID[i])
}

for (i in 1:length(im.test)) {
   im.test[[i]] <- resize(im.test[[i]], size_x=size, size_y=size)
}

# histogram equalisation
for (i in 1:length(im.test)) {
   a <- grayscale(im.test[[i]])
   f <- ecdf(a)
   im.test[[i]] <- as.cimg(f(a), dim=dim(a))
}

# features and target for train set
X.test <- matrix(0, ncol=size*size, nrow=length(im.test))
for (i in 1:length(im.test)) {
   X.test[i,] <- as.numeric(im.test[[i]])
}

setwd(wd)

#-------------------------------------------------------------------------------
# Classification 
#-------------------------------------------------------------------------------
# Naive Bayes
library(e1071)
nb.fit <- naiveBayes(x=X.train, y=Y.train)
nb.pred <- predict(nb.fit, newdata=X.test, type='class')
test.ind$Class <- as.character(nb.pred)
test.ind <- test.ind[, c('Class', 'ID')]
write.table(test.ind, file='nb.csv', row.names=F, col.names=T, sep=',', quote=F)

# Random Forest
library(randomForest)
rf.fit <- randomForest(x=X.train, y=Y.train)
rf.pred <- predict(rf.fit, newdata=X.test, type='class')
test.ind$Class <- as.character(rf.pred)
write.table(test.ind, file='rf.csv', row.names=F, col.names=T, sep=',', quote=F)

#-------------------------------------------------------------------------------




