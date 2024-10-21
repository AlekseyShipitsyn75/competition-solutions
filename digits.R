#-------------------------------------------------------------------------------
# Digits recognition
# author: Aleksey Shipitsyn
# 2017-12-14

#-------------------------------------------------------------------------------
# install.packages("imager")
# install.packages("spatstat")
# install.packages("e1071")
# install.packages("randomForest")

library(imager)
library(spatstat)
library(e1071)
library(randomForest)

# Directories for training and test sets
setwd("~/Documents/Competitions/Digits")
wd <- getwd()
train.path <- '/Images/train/'
test.path <- '/Images/test/'

setwd(paste0(wd, train.path)) 
train.files <- dir()
# head(train.files)

#-------------------------------------------------------------------------------
# Threshold function

apply.threshold <- function(x, thr=0.7) {
   x[x < thr] <- 0
   x[x > 0] <- 1
   return(x)
}

#-------------------------------------------------------------------------------
# Combine all images to matrix
m <- matrix(0, ncol=28*28, nrow=length(train.files))

for (i in 1:length(train.files)) {
   print(i)
   im <- load.image(train.files[i])
   im <- imsplit(im, 'c')[[1]]
   m[i,] <- as.numeric(im)
}

m <- data.frame(m)
m$filename <- train.files

# Labels
setwd(wd)
y <- read.csv('train.csv', header=T, stringsAsFactors=F)

m <- merge(m, y, by='filename')
names(m)

#-------------------------------------------------------------------------------
# Train X and Y
x <- as.matrix(m[,-c(1, ncol(m))])
dimnames(x)[[1]] <- train.files
y <- factor(m$label)

#-------------------------------------------------------------------------------
# Threshold and delete empty
# x <- apply.threshold(x, thr=0.8)

Nmin <- dim(x)[1] * 0.0005
empty.pixels <- which(colSums(x) < Nmin)
x <- x[, -empty.pixels]
dim(x)

#-------------------------------------------------------------------------------
# Test X
setwd(paste0(wd, test.path)) 
test.files <- dir()
# head(test.files)

mt <- matrix(0, ncol=28*28, nrow=length(test.files))

for (i in 1:length(test.files)) {
   print(i)
   im <- load.image(test.files[i])
   im <- imsplit(im, 'c')[[1]]
   mt[i,] <- as.numeric(im)
}

mt <- data.frame(mt)
mt$filename <- test.files

setwd(wd)
save(list=c('m','mt'), file='im.RData')
load('im.RData')

#-------------------------------------------------------------------------------
# Train X and Y
xt <- as.matrix(mt[, -ncol(mt)])
dimnames(xt)[[1]] <- test.files
dim(xt)

# Threshold and delete empty
# xt <- apply.threshold(xt, thr=0.8)
xt <- xt[, -empty.pixels]
dim(xt)

#-------------------------------------------------------------------------------
# Classification 
#-------------------------------------------------------------------------------
# Naive Bayes
nb.fit <- naiveBayes(x=x, y=y)
nb.pred <- predict(nb.fit, newdata=xt, type='class')
nb.pred <- data.frame(test.files, nb.pred, stringsAsFactors=F)
names(nb.pred) <- c('filename','label')
write.table(nb.pred, file='nb.csv', row.names=F, col.names=T, sep=',', quote=F)

# Random Forest
rf.fit <- randomForest(x=x, y=y)
rf.pred <- predict(rf.fit, newdata=xt, type='class')
rf.pred <- data.frame(test.files, rf.pred, stringsAsFactors=F)
names(rf.pred) <- c('filename','label')
write.table(rf.pred, file='rf.csv', row.names=F, col.names=T, sep=',', quote=F)

#-------------------------------------------------------------------------------

