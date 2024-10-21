################################################################################
# SentiRuEval-2016 competition solution
# author: Aleksey Shipitsyn
# 2016-09-21

################################################################################
# Bank tweets
################################################################################
# function to read xml file and return character data frame
xml2dataframe <- function(fname, banks = TRUE) {
   # read xml file and extract data base
   require(XML)
   fxml <- htmlTreeParse(file = fname, validate = TRUE)
   db <- fxml$children$html[[1]][[1]][[4]]
   
   # create data frame
   names <- NULL
   if (banks) { 
      names <- c('id','twitid','date','text','sberbank','vtb','gazprom','alfabank',
                 'bankmoskvy','raiffeisen','uralsib','rshb')
   }else{
      names <- c('id','twitid','date','text',
                 'beeline','mts','megafon','tele2','rostelecom','komstar','skylink')
   }
   nrows <- length(db) - 1 # because the first row is comment
   ncols <- length(names)
   df <- matrix('', nrow = nrows, ncol = ncols)
   df <- as.data.frame(df, stringsAsFactors = FALSE)
   names(df) <- names
   
   # fill the data frame
   for (i in 2:nrows) { df[i-1,] <- xmlSApply(db[[i]], xmlValue) }
   return(df)
}
################################################################################
dftrain <- xml2dataframe(fname = 'bank_train_2016.xml', banks = TRUE) 
dftest <- xml2dataframe(fname = 'banks_test_etalon.xml', banks = TRUE) 
################################################################################
# take text and sentiment value from tweets data frame
textsent <- function(df) {
   sentiment <- function(x) {
      x <- gsub(pattern = 'NULL', replacement = 'NA', x)
      s <- sum(as.integer(x), na.rm = TRUE)
      return(sign(s))
   }
   sent <- apply(X = df[, -c(1:4)], MARGIN = 1, FUN = sentiment)
   df <- data.frame(text = df$text, sent = sent, stringsAsFactors = FALSE)
   return(df)
}
################################################################################   
train <- textsent(dftrain)
test <- textsent(dftest)
################################################################################   
# create corpus of tweets and clean it
require(tm)
corp <- Corpus(VectorSource(c(dftrain$text, dftest$text)))
# to lower case
corp <- tm_map(corp, content_transformer(tolower))
# remove stopwords
corp <- tm_map(corp, removeWords, words = stopwords("ru"))
# remove URL
removeURL <- function(x) { gsub("http[^[:space:]]*", "", x) }
corp <- tm_map(corp, content_transformer(removeURL))
# remove English words
removeENG <- function(x) { gsub('[a-z0-9]+', '', x) }
corp <- tm_map(corp, content_transformer(removeENG))
# remove punctutation and numbers
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
# trim white spaces
corp <- tm_map(corp, stripWhitespace)
corp <- tm_map(corp, content_transformer(trimws))
# stemming
corp <- tm_map(corp, stemDocument, language = "ru")
################################################################################   
# term-document matrix and features of training and test sets
tdm <- TermDocumentMatrix(corp)
tdm
# discard all terms that appear in 20 documents or less
tdm2 <- removeSparseTerms(tdm, 1 - 20 / length(corp))
tdm2
inspect(tdm2[1:50, 1:10])

# train and test sets
dtm <- t(as.matrix(tdm2))
Xtrain <- data.frame(dtm[1:9392,])
Xtest <- data.frame(dtm[9393:dim(dtm)[1],])

# PCA for gaining in speed 
PCA <- prcomp(Xtrain, center = FALSE, scale. = FALSE, retx = TRUE)
plot(PCA, type = 'l')
options(scipen = 999) # rid of scientific format in printing
summary(PCA) # 250 PC scores cover 90% of total variance

Xtrain <- data.frame(PCA$x[, 1:250]) 
Xtest <- data.frame(predict(PCA, newdata = Xtest)[, 1:250])
Ytrain <- train$sent
Ytest <- test$sent
################################################################################   
# function for under-sampling major classes 
# it takes number of elements of minimum class and undersample all other classes 
# input: classvec = vector of class labels
#        dfpred = data frame with predictors
# output: list with under-sampled classvec and dfpred
################################################################################   
undersample <- function(dfpred, classvec) {
   N <- length(classvec)
   tt <- table(classvec)
   Nmin <- min(tt)
   out <- vector(mode = 'integer')
   for (i in 1:length(tt)) {
      x <- (1:N)[classvec == attr(tt[i], 'names')] 
      out <- c(out, sample(x = x, size = Nmin, replace = FALSE))
   }
   return(list(dfpred = dfpred[out,], classvec = classvec[out]))
}
################################################################################   
# function for over-sampling minor classes 
# it takes all classes as they are and additionally oversample all minor classes 
# input: classvec = vector of class labels
#        dfpred = data frame with predictors
# output: list with over-sampled classvec and dfpred
################################################################################   
oversample <- function(dfpred, classvec) {
   N <- length(classvec)
   tt <- table(classvec)
   Nmax <- max(tt)
   Ntosample <- abs(tt - Nmax)
   out <- 1:N
   for (i in 1:length(tt)) {
      x <- (1:N)[classvec == attr(tt[i], 'names')] 
      out <- c(out, sample(x = x, size = Ntosample[i], replace = TRUE))
   }
   return(list(dfpred = dfpred[out,], classvec = classvec[out]))
}
################################################################################   
# classes are unbalanced, use undersampling and oversampling for balancing
set.seed(654)
und <- undersample(Xtrain, Ytrain)
undXtrain <- data.frame(und$dfpred)
undYtrain <- und$classvec

ove <- oversample(Xtrain, Ytrain)
oveXtrain <- data.frame(ove$dfpred)
oveYtrain <- ove$classvec
################################################################################   
# function to find fscore for confusion matrix of any dimention   
fmeasure <- function(tt, beta = 1, averaging = 'micro') {   
   N <- dim(tt)[1]
   TP <- rep(0,N) 
   FP <- rep(0,N)
   FN <- rep(0,N)
   precis <- rep(0,N)
   recall <- rep(0,N)
   fscore <- rep(0,N)
   
   # scores for each class
   for (i in 1:N) {
      TP[i] <- tt[i,i]
      FP[i] <- sum(tt[-i,i])
      FN[i] <- sum(tt[i,-i])
      precis[i] <- TP[i] / (TP[i] + FP[i])
      recall[i] <- TP[i] / (TP[i] + FN[i]) 
      fscore[i] <- (1+beta^2)*precis[i]*recall[i] / (beta^2*precis[i] + recall[i]) 
   }
   # averaging output fscore
   out <- 0
   if (averaging == 'micro') {
      pr <- sum(TP) / sum(TP + FP)
      rc <- sum(TP) / sum(TP + FN)
      out <- (1 + beta^2) * pr * rc / (beta^2 * pr + rc) 
   }
   if (averaging == 'macro') { 
      out <- mean(fscore) 
   }
   return(out)
}

#############################################################################
# misclassification rate function
misclass.rate <- function(tt, foreach = FALSE) {
   if(length(dim(tt)) > 2 | dim(tt)[1] != dim(tt)[2]) {
      return('Error: input should be squared matrix')
   }
   if (!foreach) {
      return( 1 - sum(diag(tt)) / sum(tt) )
   }
   N <- dim(tt)[1]
   out <- rep(-1, N)
   for (i in 1:N) { out[i] <- 1 - tt[i,i] / sum(tt[i,]) }
   return(out)
}

################################################################################   
# Classification models wraped into function
classify <- function(Xtrain, Ytrain, Xtest, Ytest) {
   # results DF
   resDF <- data.frame(model = c('Linear Discriminant Analysis',
                                 'Quadratic Discriminant Analysis',
                                 'Logistic regression',
                                 'Linear model',
                                 'Neural Network',
                                 'Classification tree',
                                 'SVM', 'kNN', 'Naive Bayes',
                                 'Assembled by Voting'),
                       mis.total = rep(0,10),
                       mis.neg = rep(0,10),
                       mis.zero = rep(0,10), 
                       mis.pos = rep(0,10), 
                       f1.micro = rep(0,10), 
                       f1.macro = rep(0,10), 
                       time.sec = rep(0,10), stringsAsFactors = FALSE)
   
   #############################################################################
   # convert to 3 classes from probabilities of two classes
   to3classes <- function(x, threshold = 0.8) {
      out <- vector(mode = 'integer', length = length(x))
      out[x > threshold] <- 1
      out[x < (1 - threshold)] <- -1
      return(out)
   } 
   
   #############################################################################
   # Linear Discriminant Analysis
   ptm <- proc.time()[1]
   lda.fit <- lda(Ytrain ~ ., data = data.frame(Xtrain, Ytrain))
   pred.lda <- predict(lda.fit, newdata = Xtest)
   pred.lda <- as.integer(pred.lda$class) - 2
   tt <- table(true = Ytest, pred = pred.lda)
   
   resDF[1,2]   <- misclass.rate(tt)
   resDF[1,3:5] <- misclass.rate(tt, foreach = TRUE)
   resDF[1,6:7] <- c(fmeasure(tt,1,'micro'), fmeasure(tt,1,'macro'))
   resDF[1,8]   <- proc.time()[1] - ptm
   
   
   #############################################################################
   # Quadratic Discriminant Analysis
   require(MASS)
   ptm <- proc.time()[1]
   qda.fit <- qda(Ytrain ~ ., data = data.frame(Xtrain, Ytrain))
   pred.qda <- predict(qda.fit, newdata = Xtest)
   pred.qda <- as.integer(pred.qda$class) - 2
   tt <- table(true = Ytest, pred = pred.qda)
   
   resDF[2,2]   <- misclass.rate(tt)
   resDF[2,3:5] <- misclass.rate(tt, foreach = TRUE)
   resDF[2,6:7] <- c(fmeasure(tt,1,'micro'), fmeasure(tt,1,'macro'))
   resDF[2,8]   <- proc.time()[1] - ptm
   
   #############################################################################
   # Logistic regression
   ptm <- proc.time()[1]
   logtrain <- Xtrain[Ytrain != 0,]
   logtrain$class <- ifelse(Ytrain[Ytrain != 0] < 0, yes = 0, no = 1)
   logistic.fit <- glm(class ~ ., data = logtrain, family = binomial())
   pred.lg <- predict(logistic.fit, newdata = Xtest, type = 'response')
   pred.lg <- to3classes(pred.lg, threshold = 0.9)
   tt <- table(true = Ytest, pred = pred.lg)
   
   resDF[3,2]   <- misclass.rate(tt)
   resDF[3,3:5] <- misclass.rate(tt, foreach = TRUE)
   resDF[3,6:7] <- c(fmeasure(tt,1,'micro'), fmeasure(tt,1,'macro'))
   resDF[3,8]   <- proc.time()[1] - ptm
   
   #############################################################################
   # Linear model
   lmfit <- lm(Ytrain ~ ., data = data.frame(Xtrain, Ytrain))
   pred.lm <- predict(lmfit, newdata = Xtest)
   pred.lm <- sign(round(pred.lm))
   tt <- table(true = Ytest, pred = pred.lm) 
   
   resDF[4,2]   <- misclass.rate(tt)
   resDF[4,3:5] <- misclass.rate(tt, foreach = TRUE)
   resDF[4,6:7] <- c(fmeasure(tt,1,'micro'), fmeasure(tt,1,'macro'))
   resDF[4,8]   <- proc.time()[1] - ptm
   
   #############################################################################
   # Neural network 
   require(nnet)
   ptm <- proc.time()[1]
   nn.fit <- multinom(Ytrain ~ ., data = data.frame(Xtrain, Ytrain))
   pred.nn <- predict(nn.fit, newdata = Xtest, type = 'class')
   pred.nn <- as.integer(pred.nn) - 2
   tt <- table(true = Ytest, pred = pred.nn)
   
   resDF[5,2]   <- misclass.rate(tt)
   resDF[5,3:5] <- misclass.rate(tt, foreach = TRUE)
   resDF[5,6:7] <- c(fmeasure(tt,1,'micro'), fmeasure(tt,1,'macro'))
   resDF[5,8]   <- proc.time()[1] - ptm
   
   #############################################################################
   # Classification tree
   require(party)
   ptm <- proc.time()[1]
   tr <- ctree(class ~ ., data = data.frame(Xtrain, class = factor(Ytrain)))
   pred.tr <- predict(tr, newdata = Xtest, type = 'response')
   pred.tr <- as.integer(pred.tr) - 2
   tt <- table(true = Ytest, pred = pred.tr)
   
   resDF[6,2]   <- misclass.rate(tt)
   resDF[6,3:5] <- misclass.rate(tt, foreach = TRUE)
   resDF[6,6:7] <- c(fmeasure(tt,1,'micro'), fmeasure(tt,1,'macro'))
   resDF[6,8]   <- proc.time()[1] - ptm
   
   #############################################################################
   # SVM classificator
   require(kernlab)
   ptm <- proc.time()[1]
   svm.fit <- ksvm(class ~ ., data = data.frame(Xtrain, 
                                  class = factor(Ytrain)), kernel = 'rbfdot')
   pred.sv <- predict(svm.fit, newdata = Xtest, type = 'response')
   pred.sv <- as.integer(pred.sv) - 2
   tt <- table(true = Ytest, pred = pred.sv)
   
   resDF[7,2]   <- misclass.rate(tt)
   resDF[7,3:5] <- misclass.rate(tt, foreach = TRUE)
   resDF[7,6:7] <- c(fmeasure(tt,1,'micro'), fmeasure(tt,1,'macro'))
   resDF[7,8]   <- proc.time()[1] - ptm
   
   #############################################################################
   # kNN
   require(class)
   ptm <- proc.time()[1]
   knn.fit <- knn(train = Xtrain, test = Xtest, cl = factor(Ytrain), k = 3)
   pred.knn <- as.integer(knn.fit) - 2
   tt <- table(true = Ytest, pred = pred.knn)
   
   resDF[8,2]   <- misclass.rate(tt)
   resDF[8,3:5] <- misclass.rate(tt, foreach = TRUE)
   resDF[8,6:7] <- c(fmeasure(tt,1,'micro'), fmeasure(tt,1,'macro'))
   resDF[8,8]   <- proc.time()[1] - ptm
   
   #############################################################################
   # Naive Bayes
   require(e1071)
   ptm <- proc.time()[1]
   nb.fit <- naiveBayes(Ytrain ~ ., data = data.frame(Xtrain, Ytrain))
   pred.nb <- predict(nb.fit, newdata = Xtest, type = 'raw')
   pred.nb <- apply(X = pred.nb, MARGIN = 1, which.max) - 2
   tt <- table(true = Ytest, pred = pred.nb)
   
   resDF[9,2]   <- misclass.rate(tt)
   resDF[9,3:5] <- misclass.rate(tt, foreach = TRUE)
   resDF[9,6:7] <- c(fmeasure(tt,1,'micro'), fmeasure(tt,1,'macro'))
   resDF[9,8]   <- proc.time()[1] - ptm
   
   #############################################################################
   # voting function 
   vote <- function(x) {
      tx <- table(x)
      tx <- sort(tx, decreasing = TRUE)
      out <- as.integer(names(tx)[1])
      return(out)
   }

   #############################################################################
   # assemble by voting for test set
   pred.as <- cbind(pred.lda, pred.qda, pred.lg, pred.lm, pred.nn, pred.tr, 
                    pred.sv, pred.knn, pred.nb)
   pred.as <- apply(X = pred.as, MARGIN = 1, FUN = vote)
   tt <- table(true = Ytest, pred = pred.as)
   
   resDF[10,2]   <- misclass.rate(tt)
   resDF[10,3:5] <- misclass.rate(tt, foreach = TRUE)
   resDF[10,6:7] <- c(fmeasure(tt,1,'micro'), fmeasure(tt,1,'macro'))
   resDF[10,8]   <- sum(resDF[1:9,8])
   
   #############################################################################
   # printing results
   od <- order(resDF$f1.macro, decreasing = TRUE)
   resDF <- resDF[od,]
   row.names(resDF) <- 1:10
   options(digits = 3)
   print('Comparison of classification models')
   print(resDF)
}

################################################################################   
# training with undesampled training set, testing with test set
classify(Xtrain = undXtrain, Ytrain = undYtrain, Xtest = Xtest, Ytest = Ytest)

# [1] "Comparison of classification models"
#                              model mis.total mis.neg mis.zero mis.pos f1.micro f1.macro time.sec
# 1                     Linear model     0.364   0.809    0.166   0.707    0.636    0.435     1.86
# 2              Assembled by Voting     0.519   0.478    0.554   0.365    0.481    0.434    50.44
# 3                              SVM     0.541   0.511    0.573   0.382    0.459    0.414     9.84
# 4                   Neural Network     0.547   0.556    0.569   0.365    0.453    0.406     2.23
# 5     Linear Discriminant Analysis     0.576   0.541    0.629   0.273    0.424    0.394     1.15
# 6  Quadratic Discriminant Analysis     0.590   0.392    0.691   0.349    0.410    0.388     1.22
# 7                      Naive Bayes     0.545   0.730    0.482   0.549    0.455    0.366    21.65
# 8                              kNN     0.621   0.522    0.680   0.441    0.379    0.351     8.80
# 9              Logistic regression     0.635   0.609    0.668   0.454    0.365    0.334     1.60
# 10             Classification tree     0.687   0.519    0.789   0.355    0.313    0.310     2.09

################################################################################   
# training with oversampled training set, testing with test set
classify(Xtrain = oveXtrain, Ytrain = oveYtrain, Xtest = Xtest, Ytest = Ytest)

# [1] "Comparison of classification models"
#                              model mis.total mis.neg mis.zero mis.pos f1.micro f1.macro time.sec
# 1                              SVM     0.423   0.384    0.407   0.641    0.577    0.483   450.43
# 2              Assembled by Voting     0.431   0.477    0.403   0.526    0.569    0.483   703.32
# 3                   Neural Network     0.487   0.497    0.493   0.421    0.513    0.449    26.06
# 4  Quadratic Discriminant Analysis     0.515   0.433    0.549   0.470    0.485    0.431     3.75
# 5                     Linear model     0.356   0.834    0.145   0.720    0.644    0.431    14.66
# 6                              kNN     0.500   0.509    0.488   0.559    0.500    0.426   101.63
# 7     Linear Discriminant Analysis     0.554   0.516    0.604   0.276    0.446    0.412     9.42
# 8              Classification tree     0.512   0.520    0.486   0.678    0.488    0.401    53.43
# 9              Logistic regression     0.519   0.674    0.460   0.566    0.481    0.391    12.50
# 10                     Naive Bayes     0.538   0.740    0.468   0.543    0.462    0.369    31.44

################################################################################   
# Telecom tweets
################################################################################
dftrain <- xml2dataframe(fname = 'tkk_train_2016.xml', banks = FALSE) 
dftest <- xml2dataframe(fname = 'tkk_test_etalon.xml', banks = FALSE) 

train <- textsent(dftrain)
test <- textsent(dftest)
################################################################################   
# create corpus of tweets and clean it
require(tm)
corp <- Corpus(VectorSource(c(dftrain$text, dftest$text)))
# to lower case
corp <- tm_map(corp, content_transformer(tolower))
# remove stopwords
corp <- tm_map(corp, removeWords, words = stopwords("ru"))
# remove URL
removeURL <- function(x) { gsub("http[^[:space:]]*", "", x) }
corp <- tm_map(corp, content_transformer(removeURL))
# remove English words
removeENG <- function(x) { gsub('[a-z0-9]+', '', x) }
corp <- tm_map(corp, content_transformer(removeENG))
# remove punctutation and numbers
corp <- tm_map(corp, removePunctuation)
corp <- tm_map(corp, removeNumbers)
# trim white spaces
corp <- tm_map(corp, stripWhitespace)
corp <- tm_map(corp, content_transformer(trimws))
# stemming
corp <- tm_map(corp, stemDocument, language = "ru")
################################################################################   
# term-document matrix and features of training and test sets
tdm <- TermDocumentMatrix(corp)
tdm
# discard all terms that appear in 20 documents or less
tdm2 <- removeSparseTerms(tdm, 1 - 20 / length(corp))
tdm2
inspect(tdm2[1:50, 1:10])

# train and test sets
dtm <- t(as.matrix(tdm2))
Xtrain <- data.frame(dtm[1:8643,])
Xtest <- data.frame(dtm[8644:dim(dtm)[1],])

# PCA for gaining in speed 
PCA <- prcomp(Xtrain, center = FALSE, scale. = FALSE, retx = TRUE)
plot(PCA, type = 'l')
options(scipen = 999) # rid of scientific format in printing
summary(PCA) # 300 PC scores cover 85% of total variance

Xtrain <- data.frame(PCA$x[, 1:300]) 
Xtest <- data.frame(predict(PCA, newdata = Xtest)[, 1:300])
Ytrain <- train$sent
Ytest <- test$sent
################################################################################  
# classes are unbalanced, use undersampling and oversampling for balancing
set.seed(65)
und <- undersample(Xtrain, Ytrain)
undXtrain <- data.frame(und$dfpred)
undYtrain <- und$classvec

ove <- oversample(Xtrain, Ytrain)
oveXtrain <- data.frame(ove$dfpred)
oveYtrain <- ove$classvec
################################################################################  
# Classification
# training with undesampled training set, testing with test set
classify(Xtrain = undXtrain, Ytrain = undYtrain, Xtest = Xtest, Ytest = Ytest)

# [1] "Comparison of classification models"
#                              model mis.total mis.neg mis.zero mis.pos f1.micro f1.macro time.sec
# 1                              SVM     0.418   0.424    0.387   0.561    0.582    0.512    31.13
# 2              Assembled by Voting     0.422   0.425    0.384   0.620    0.578    0.502    85.84
# 3     Linear Discriminant Analysis     0.445   0.432    0.436   0.567    0.555    0.487     2.65
# 4                   Neural Network     0.450   0.427    0.445   0.604    0.550    0.479     5.08
# 5  Quadratic Discriminant Analysis     0.496   0.458    0.534   0.492    0.504    0.453     1.62
# 6                      Naive Bayes     0.495   0.503    0.457   0.668    0.505    0.436    19.41
# 7              Logistic regression     0.514   0.592    0.412   0.668    0.486    0.422     3.23
# 8                     Linear model     0.470   0.745    0.146   0.797    0.530    0.415     3.91
# 9                              kNN     0.517   0.546    0.452   0.722    0.483    0.411    14.62
# 10             Classification tree     0.572   0.421    0.693   0.711    0.428    0.364     4.18

################################################################################  
# training with oversampled training set, testing with test set
classify(Xtrain = oveXtrain, Ytrain = oveYtrain, Xtest = Xtest, Ytest = Ytest)

# [1] "Comparison of classification models"
#                              model mis.total mis.neg mis.zero mis.pos f1.micro f1.macro time.sec
# 1              Assembled by Voting     0.401   0.408    0.345   0.674    0.599    0.512   622.47
# 2                              SVM     0.405   0.446    0.318   0.674    0.595    0.508   437.15
# 3     Linear Discriminant Analysis     0.428   0.406    0.425   0.561    0.572    0.502     9.30
# 4                   Neural Network     0.441   0.411    0.447   0.567    0.559    0.491    22.15
# 5  Quadratic Discriminant Analysis     0.480   0.430    0.517   0.545    0.520    0.461     3.71
# 6              Logistic regression     0.501   0.573    0.391   0.733    0.499    0.426     8.57
# 7                      Naive Bayes     0.502   0.487    0.479   0.711    0.498    0.425    26.40
# 8                              kNN     0.518   0.549    0.466   0.636    0.482    0.421    60.83
# 9              Classification tree     0.512   0.522    0.460   0.749    0.488    0.411    43.89
# 10                    Linear model     0.471   0.751    0.134   0.840    0.529    0.403    10.46
################################################################################  


