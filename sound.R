#-------------------------------------------------------------------------------
# Urban Sound Classification
# author: Aleksey Shipitsyn
# 2017-12-22

#-------------------------------------------------------------------------------
# install.packages(c("fftw","tuneR","rgl","rpanel"), repos="http://cran.at.r-project.org/")
# install.packages("seewave", repos="http://cran.at.r-project.org/")
# install.packages('audio')

library(seewave)
library(tuneR)
library(e1071)
library(randomForest)

#-------------------------------------------------------------------------------
# Read sounds of train set
#-------------------------------------------------------------------------------
setwd("~/Documents/Competitions/Sounds")
wd <- getwd()
train.dir <- 'train'

train.ind <- read.csv(file=paste0(train.dir,'/','train.csv'), header=T, stringsAsFactors=F)
s <- sort(table(train.ind$Class), decreasing=T)
s
# jackhammer    engine_idling            siren  air_conditioner children_playing 
#        668              624              607              600              600 
# dog_bark         drilling     street_music         car_horn         gun_shot 
#      600              600              600              306              230 

most.frequent.class <- names(s)[1]
most.frequent.class
# "jackhammer"

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
set.seed(14)
ind <- random.undersample(classvec=train.ind$Class)
train.ind <- train.ind[ind,]
table(train.ind$Class)

# load sounds to a list, handle files that are corrupted
setwd(paste0(train.dir,'/Train'))
s.train <- list()
for (i in 1:nrow(train.ind)) {
   filename <- paste0(train.ind$ID[i], '.wav')
   cat('i=', i, ': ',filename, '\n', sep='')
   s.train[[i]] <- tryCatch(readWave(filename), error=function(e) { return(NA) })
}

# check and delete corrupted files 
corrupted.files <- which(is.na(s.train))
s.train[[corrupted.files]] <- NULL
train.ind[corrupted.files, 'Class'] <- NA
train.ind <- na.omit(train.ind)

setwd(wd)
save(list=c('s.train','train.ind','most.frequent.class'), file='train.RData')
load(file='train.RData')

#-------------------------------------------------------------------------------
# Normalize train set
#-------------------------------------------------------------------------------
# check size and sampling rate

check.size <- function(wavels) {
   N <- length(wavels)
   out <- matrix(0, ncol=3, nrow=N)
   dimnames(out) <- list(NULL, c('nsamp','srate','sec'))
   
   for (i in 1:N) {
      cat(i,'\n')
      a <- wavels[[i]]
      if (class(a) != 'Wave') { a <- Wave(a) }
      if (a@stereo) { a <- mono(a) }
      out[i,] <- c(length(a@left), a@samp.rate, length(a@left) / a@samp.rate)
   } 
   return(out)
}

#-------------------------------------------------------------------------------
# resample and take mono function

resample.mono <- function(wavels, sample.rate=44100) {
   for (i in 1:length(wavels)) {
      # cat(i,'\n')
      a <- wavels[[i]]
      if (class(a) != 'Wave') { 
         a <- Wave(a) 
      }
      if (a@stereo) {
         a <- mono(a)
      }
      if (a@samp.rate != sample.rate) {
         a <- resamp(a, g=sample.rate, output='Wave')
      }
      wavels[[i]] <- a
   }
   return(wavels)
}

#-------------------------------------------------------------------------------
# equalize size of sounds

equalize.size <- function(wavels, nsamp=88200) {
   for (i in 1:length(wavels)) {
      # cat(i,'\n')
      a <- wavels[[i]]
      if (length(a@left) < nsamp) {
         a <- addsilw(a, d=sec.length - length(a@left)/a@samp.rate + 0.01, output='Wave')
      }
      if (length(a@left) > nsamp) {
         a <- extractWave(a, from=1, to=nsamp, xunit='samples')
      }
      wavels[[i]] <- a
   }
   return(wavels)
}

#-------------------------------------------------------------------------------
n <- check.size(s.train)
summary(n)
par(mfrow=c(1,2))
hist(n[, 'sec'], breaks=20)
hist(n[, 'srate'], breaks=20)

# resample to 44100 and take mono
s.train <- resample.mono(s.train)

sec.length <- 2
sample.rate <- 44100
short.files <- which(n[,'sec'] < sec.length)
short.files

# take 2 seconds and add silent endings to the short files
s.train <- equalize.size(s.train, nsamp=sec.length * sample.rate)

n <- check.size(s.train)
summary(n)

# normalize sounds
for (i in 1:length(s.train)) {
   s.train[[i]] <- normalize(s.train[[i]])
}

plot(s.train[[18]])
plot(s.train[[4]])
plot(s.train[[28]])
plot(s.train[[36]])

#-------------------------------------------------------------------------------
# Features and target of train set
#-------------------------------------------------------------------------------
# Features matrix of MFCC 
a <- melfcc(s.train[[1]], numcep=10)
dims=dim(a)
X.train <- matrix(0, nrow=length(s.train), ncol=dims[1]*dims[2])

for (i in 1:length(s.train)) {
   cat(i,'\n')
   X.train[i,] <- as.numeric(melfcc(s.train[[i]], numcep=10))
}

# Target
Y.train <- factor(train.ind$Class)

#-------------------------------------------------------------------------------
# Classification
#-------------------------------------------------------------------------------
# Naive Bayes
nb.fit <- naiveBayes(x=X.train, y=Y.train)
# Random Forest
rf.fit <- randomForest(x=X.train, y=Y.train)

#-------------------------------------------------------------------------------
# Test set
#-------------------------------------------------------------------------------
setwd(wd)
test.dir <- 'test'
test.ind <- read.csv(file=paste0(test.dir,'/','test.csv'), header=T, stringsAsFactors=F)
test.ind$Class <- character(length=nrow(test.ind))

# load sounds to a list
setwd(paste0(test.dir,'/Test'))
s.test <- list()
for (i in 1:nrow(test.ind)) {
   filename <- paste0(test.ind$ID[i], '.wav')
   cat('i=', i, ': ',filename, '\n', sep='')
   s.test[[i]] <- tryCatch(readWave(filename), error=function(e) { return(NA) })
}

# handle corrupted files 
corrupted.files <- which(is.na(s.test))
corrupted.files
# s.test[[corrupted.files[1]]]
# s.test[[corrupted.files[2]]]
# s.test[[corrupted.files[3]]]
# s.test[[corrupted.files[4]]]
# s.test[[corrupted.files[5]]]
# s.test[[corrupted.files[6]]]
# s.test[[corrupted.files[7]]]

test.ind[corrupted.files, 'Class'] <- most.frequent.class

corr <- corrupted.files
for (i in 1:length(corrupted.files)) {
   s.test[[corr[i]]] <- NULL
   corr <- corr - 1
}
which(is.na(s.test))

# check size
n <- check.size(s.test)
summary(n)
par(mfrow=c(1,2))
hist(n[, 'sec'], breaks=20)
hist(n[, 'srate'], breaks=20)

# resample to 44100 and take mono
s.test <- resample.mono(s.test)

short.files <- which(n[,'sec'] < sec.length)
short.files

# take 2 seconds and add silent endings to the short files
s.test <- equalize.size(s.test, nsamp=sec.length * sample.rate)

n <- check.size(s.test)
summary(n)

# normalize sounds
for (i in 1:length(s.test)) {
   s.test[[i]] <- normalize(s.test[[i]])
}

par(mfrow=c(2,2))
plot(s.test[[18]])
plot(s.test[[4]])
plot(s.test[[28]])
plot(s.test[[36]])

#-------------------------------------------------------------------------------
# Features of test set
#-------------------------------------------------------------------------------
# Features matrix of MFCC 
X.test <- matrix(0, nrow=length(s.test), ncol=dims[1]*dims[2])
for (i in 1:length(s.test)) {
   cat(i,'\n')
   X.test[i,] <- as.numeric(melfcc(s.test[[i]], numcep=10))
}
sum(is.na(X.test))

#-------------------------------------------------------------------------------
# Prediction on test set and submission
#-------------------------------------------------------------------------------
# Naive Bayes
nb.pred <- predict(nb.fit, newdata=X.test, type='class')
test.ind$Class[-corrupted.files] <- as.character(nb.pred)
test.ind <- test.ind[, c('Class','ID')]
setwd(wd)
write.table(test.ind, file='nb.csv', row.names=F, col.names=T, sep=',', quote=F)

# Random forest
rf.pred <- predict(rf.fit, newdata=X.test, type='class')
test.ind$Class[-corrupted.files] <- as.character(rf.pred)
write.table(test.ind, file='rf.csv', row.names=F, col.names=T, sep=',', quote=F)

#-------------------------------------------------------------------------------