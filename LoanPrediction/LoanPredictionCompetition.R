################################################################################
# Loan Prediction 
# http://datahack.analyticsvidhya.com/contest/practice-problem-loan-prediction-iii 
# author: Aleksey Shipitsyn
# 2016-07-25

################################################################################
# function for finding sample mode
sample.mode <- function(x, method = 'table') {
   m <- NULL
   if (method == 'table') {
      tt <- table(x)
      m <- attr(which.max(tt), 'names')  # sample mode as character
      class(m) <- class(x)               # modify class to class of input  
   }
   else if (method == 'density') {
      d <- density(x, na.rm = TRUE)
      m <- x[which.max(d$y)]
   }
   return(m)
}

################################################################################
preprocess.data <- function() {
   # read and explore the data
   train <- read.csv('train_u6lujuX_CVtuZ9i.csv', header=T, sep=',', na.strings='')
   test <- read.csv('test_Y3wMUE5_7gLdaTN.csv', header=T, sep=',', na.strings='')
   # str(train)
   # str(test)
   # summary(train)
   # summary(test)
   
   #############################################################################
   # Transform variables and impute NA
   #############################################################################
   # cannot drop NA because of submission rules to predict on test set
   # look how many rows with NA 
   sum(apply(X = train, MARGIN = 1, FUN = function(x) {sum(is.na(x))}))
   sum(apply(X = test, MARGIN = 1, FUN = function(x) {sum(is.na(x))}))
   
   #############################################################################
   # function to convert binary factor variable to integer {-1,0,1}
   binary2integer <- function(x) {
      x <- as.integer(x) - 1
      x[x == 0] <- -1
      x[is.na(x)] <- 0
      return(x)
   }
   
   #############################################################################
   # function to convert ordinal factor variable to integer {0,1,2,...}
   ordinal2integer <- function(x) {
      x <- as.integer(x)
      x[is.na(x)] <- 0
      return(x)
   }
   
   #############################################################################
   # function for stochastic regression imputation
   # input: X - regressors of training data
   #        y - variable of training data to impute
   #        newdata - data frame of regressors of test data
   # output: vector or list with imputed values of y and new
   impute.stoch <- function(X, y, newdata = NULL) {
      df <- data.frame(X, y)
      fit <- lm(y ~ ., data = df)
      pred <- predict(fit, newdata = df, se.fit = TRUE)
      for (i in 1:length(y)) {
         if (is.na(y[i])) {
            y[i] <- rnorm(1, mean = pred$fit[i], sd = pred$se.fit[i]) 
         }   
      }
      if(is.null(newdata)) { 
         return(y) 
      }
      else {
         pred <- predict(fit, newdata = newdata, se.fit = TRUE)
         new.y <- vector(mode = 'numeric')
         for (i in 1:nrow(newdata)) {
            if (is.na(new.y[i])) {
               new.y[i] <- rnorm(1, mean = pred$fit[i], sd = pred$se.fit[i])
            }
         }
         return(list(y = y, new.y = new.y))   
      }
   }
   
   #############################################################################
   # function for NA imputation 
   # input: df = data frame to impute
   #        train = data frame to sample from
   #        method = {sampling, median, }
   # output: imputed data frame
   impute <- function(df, train, method = 'sampling') {
      if (method == 'sampling') {
         for (vname in names(df)) {
            mis.df <- is.na(df[, vname])
            mis.tr <- is.na(train[, vname])
            df[mis.df, vname] <- sample(x = train[!mis.tr, vname], 
                                        size = sum(mis.df), replace = TRUE)
         }
      }
      else if (method == 'median') {
         for (vname in names(df)) {
            mis.df <- is.na(df[, vname])
            mis.tr <- is.na(train[, vname])
            if (is.numeric(df[, vname])) {
               df[mis.df, vname] <- median(df[!mis.tr, vname]) 
            }
            if(is.character(df[, vname])) {
               tt <- table(df[!mis.df, vname])
               df[mis.df, vname] <- attr(which.max(tt), 'names')
            }
         }
      }
      return(df)
   } 
   
   #############################################################################
   # rid of loan ID and put into row names
   row.names(train) <- as.character(train$Loan_ID)
   row.names(test) <- as.character(test$Loan_ID)
   class <- train[, 13]
   train <- train[,-c(1,13)]
   test <- test[,-1]
   
   # impute factor variables by sampling
   # train[,-c(6:9)] <- impute(df = train[,-c(6:9)], train = train, method='sampling')
   # test[, -c(6:9)] <- impute(df = test[, -c(6:9)], train = train, method='sampling')
   
   # Gender to integer {Female = -1, NA = 0, Male = 1}
   train$Gender <- binary2integer(train$Gender)
   test$Gender <- binary2integer(test$Gender)
   
   # Maried to integer {No = -1, NA = 0, Yes = 1}
   train$Married <- binary2integer(train$Married)
   test$Married <- binary2integer(test$Married)
   
   # Dependents to integer {NA = 0, 1 = 1, 2 = 2, 3+ = 3}, assume it is ordinal
   train$Dependents <- ordinal2integer(train$Dependents)
   test$Dependents <- ordinal2integer(test$Dependents)
   
   # Education to integer {Graduate = -1, NA = 0, Not Graduate = 1}
   train$Education <- binary2integer(train$Education)
   test$Education <- binary2integer(test$Education)
   
   # Self_Employed to integer {No = -1, NA = 0, Yes = 1}
   train$Self_Employed <- binary2integer(train$Self_Employed)
   test$Self_Employed <- binary2integer(test$Self_Employed)
   
   # Property area as ordinal to integer {NA = 0, Rural = 1, Semiurban = 2, Urban = 3}
   # TODO possible also to make it dummy variables
   train$Property_Area <- ordinal2integer(train$Property_Area)
   test$Property_Area <- ordinal2integer(test$Property_Area)
   
   # Credit_History to integer {No = -1, NA = 0, Yes = 1}
   train$Credit_History <- factor(train$Credit_History)
   train$Credit_History <- binary2integer(train$Credit_History)
   test$Credit_History <- factor(test$Credit_History)
   test$Credit_History <- binary2integer(test$Credit_History)
   
   # Scale LoanAmount by 1000 (restore to initial units)
   train$LoanAmount <- train$LoanAmount * 1000
   test$LoanAmount <- test$LoanAmount * 1000
   
   # for LoanAmount impute NA with stochastic regression 
   impLA <- impute.stoch(X=train[,-c(8,9)], y=train[,8], newdata=test[,-c(8,9)])
   train$LoanAmount <- impLA$y
   test$LoanAmount <- impLA$new.y
   
   # Scale Loan Term to years impute NA with stochastic regression 
   impLT <- impute.stoch(X=train[,-c(9)], y=train[,9], newdata=test[,-9])
   train$Loan_Amount_Term <- impLT$y
   test$Loan_Amount_Term <- impLT$new.y
   
   # Loan_Status to integer {No = 0 , Yes = 1}
   class <- as.integer(class) - 1
   
   #############################################################################
   # Add features by Economic meanings 
   #############################################################################
   # Total Income
   train$TotalIncome <- train$ApplicantIncome + train$CoapplicantIncome
   test$TotalIncome <- test$ApplicantIncome + test$CoapplicantIncome
   
   # net income after living expences
   train$NetIncome <- (train$ApplicantIncome + train$CoapplicantIncome -
                          (train$Dependents + 2) * 300)
   test$NetIncome <- (test$ApplicantIncome + test$CoapplicantIncome -
                         (test$Dependents + 2) * 300)
   
   # Total Income to Loan Amount ratio
   train$TotalIncomeToLoan <- train$TotalIncome / train$LoanAmount
   test$TotalIncomeToLoan <- test$TotalIncome / test$LoanAmount
   
   # Net Income to Loan Amount ratio
   train$NetIncomeToLoan <- train$NetIncome / train$LoanAmount
   test$NetIncomeToLoan <- test$NetIncome / test$LoanAmount
   
   # net income per head
   train$NetIncomePerHead <- train$NetIncome / (train$Dependents + 2)
   test$NetIncomePerHead <- test$NetIncome / (test$Dependents + 2)
   
   # interest rate, bigger for longer term
   train$Rate <- (1 + train$Loan_Amount_Term / 100 ) / 100
   test$Rate <- (1 + test$Loan_Amount_Term / 100 ) / 100
   
   # annuity payment per month
   train$Annuity <- (train$LoanAmount / train$Loan_Amount_Term * 
                        (1 + train$Rate)^(train$Loan_Amount_Term / 12)) 
   test$Annuity <- (test$LoanAmount / test$Loan_Amount_Term * 
                       (1 + test$Rate)^(test$Loan_Amount_Term / 12)) 
   
   # net income / annuity  
   train$AnnuityCoverage <- train$NetIncome / train$Annuity
   test$AnnuityCoverage <- test$NetIncome / test$Annuity
   
   # total income after annuity payment
   train$TotalIncomeAfterAnnuity <- train$TotalIncome - train$Annuity
   test$TotalIncomeAfterAnnuity <- test$TotalIncome - test$Annuity
   
   # total income after annuity payment ratio
   train$TotalIncomeAfterAnnuityRatio <- train$TotalIncomeAfterAnnuity / train$TotalIncome
   test$TotalIncomeAfterAnnuityRatio <- test$TotalIncomeAfterAnnuity / test$TotalIncome
   
   # net income after annuity payment
   train$NetIncomeAfterAnnuity <- train$NetIncome - train$Annuity
   test$NetIncomeAfterAnnuity <- test$NetIncome - test$Annuity
   
   # net income after annuity payment ratio
   train$NetIncomeAfterAnnuityRatio <- train$NetIncomeAfterAnnuity / train$NetIncome
   test$NetIncomeAfterAnnuityRatio <- test$NetIncomeAfterAnnuity / test$NetIncome
   
   # Dependents to average Dependents
   train$DependentsToAvg <- train$Dependents / sample.mode(train$Dependents)
   test$DependentsToAvg <- test$Dependents / sample.mode(test$Dependents)
   
   # Applicant Income to average Applicant Income
   train$ApplicantIncomeToAvg <- train$ApplicantIncome / median(train$ApplicantIncome)
   test$ApplicantIncomeToAvg <- test$ApplicantIncome / median(test$ApplicantIncome)
   
   # Coapplicant Income to average Coapplicant Income
   train$CoapplicantIncomeToAvg <- train$CoapplicantIncome / median(train$CoapplicantIncome)
   test$CoapplicantIncomeToAvg <- test$CoapplicantIncome / median(test$CoapplicantIncome)
   
   # Net Income to average Net Income
   train$NetIncomeToAvg <- train$NetIncome / median(train$NetIncome)
   test$NetIncomeToAvg <- test$NetIncome / median(test$NetIncome)
   
   # Net Income per head to Average Net Income per Head
   train$NetIncomePerHeadToAvg <- train$NetIncomePerHead / median(train$NetIncomePerHead)
   test$NetIncomePerHeadToAvg <- test$NetIncomePerHead / median(test$NetIncomePerHead)
   
   # Loan Ammount to average Loan Ammount
   train$LoanAmountToAvg <- train$LoanAmount / median(train$LoanAmount)
   test$LoanAmountToAvg <- test$LoanAmount / median(test$LoanAmount)
   
   # Loan Ammount Term to average Loan Ammount Term
   train$LoanTermToAvg <- train$Loan_Amount_Term / sample.mode(train$Loan_Amount_Term)
   test$LoanTermToAvg <- test$Loan_Amount_Term / sample.mode(test$Loan_Amount_Term)
   
   # interest rate to average interest rate
   train$RateToAvg <- train$Rate / median(train$Rate)
   test$RateToAvg <- test$Rate / median(test$Rate)
   
   # logs
   train$logTotalIncome <- log(train$TotalIncome)
   test$logTotalIncome <- log(test$TotalIncome)
   
   # these mignt be NaN because of zero Income
   train$logApplicantIncome <- log(1 + train$ApplicantIncome)
   test$logApplicantIncome <- log(1 + test$ApplicantIncome)
   train$logCoapplicantIncome <- log(1 + train$CoapplicantIncome)
   test$logCoapplicantIncome <- log(1 + test$CoapplicantIncome)
   
   train$logLoanAmount <- log(train$LoanAmount)
   test$logLoanAmount <- log(test$LoanAmount)
   
   # these mignt be NaN because of negative Net Income
   train$logNetIncome <- log(train$NetIncome)
   test$logNetIncome <- log(test$NetIncome)
   # train$logTotalIncomeAfterAnnuity <- log(train$TotalIncomeAfterAnnuity)
   # test$logTotalIncomeAfterAnnuity <- log(test$TotaltIncomeAfterAnnuity)
   
   # Loan Term to years
   train$Loan_Amount_Term <- train$Loan_Amount_Term / 360
   test$Loan_Amount_Term <- test$Loan_Amount_Term / 360
   
   # scale Loan Amont by 1,000,000
   train$LoanAmount <- train$LoanAmount / 1000000
   test$LoanAmount <- test$LoanAmount / 1000000
   
   # scale Income by 1000
   train$ApplicantIncome <- train$ApplicantIncome / 1000
   test$ApplicantIncome <- test$ApplicantIncome / 1000
   
   train$CoapplicantIncome <- train$CoapplicantIncome / 1000
   test$CoapplicantIncome <- test$CoapplicantIncome / 1000
   
   train$TotalIncome <- train$TotalIncome / 1000
   test$TotalIncome <- test$TotalIncome / 1000
   
   train$TotalIncomeAfterAnnuity <- train$TotalIncomeAfterAnnuity / 1000
   test$TotalIncomeAfterAnnuity <- test$TotalIncomeAfterAnnuity / 1000
   
   train$NetIncome <- train$NetIncome / 1000
   test$NetIncome <- test$NetIncome / 1000
   
   train$NetIncomePerHead <- train$NetIncomePerHead / 1000
   test$NetIncomePerHead <- test$NetIncomePerHead / 1000
   
   train$Annuity <- train$Annuity / 1000
   test$Annuity <- test$Annuity / 1000
   
   train$NetIncomeAfterAnnuity <- train$NetIncomeAfterAnnuity / 1000
   test$NetIncomeAfterAnnuity <- test$NetIncomeAfterAnnuity / 1000
   
   # str(train)
   # str(test)
   # summary(train)
   # summary(test)
   #############################################################################
   # PCA, we will fit models on PC components 
   PCA <- prcomp(train, center = TRUE, scale. = TRUE, retx = TRUE)
   plot(PCA, type = 'l')
   options(scipen = 999) # rid off scientific format in printing
   summary(PCA) # 13 PC scores cover 95% of total variance
   train.pca <- data.frame(PCA$x[, 1:13])
   train.pca <- cbind(train.pca, class = class)
   testPCA <- predict(PCA, newdata = test)
   test.pca <- data.frame(testPCA[, 1:13])
   
   # # check for Inf, and -Inf values
   # for(i in 1:ncol(test.pca)) {
   #    if (sum(test.pca[,i] == Inf) > 0) print(names(test.pca)[i])
   #    if (sum(test.pca[,i] == -Inf) > 0) print(names(test.pca)[i])
   # }
   
   # sample part of training set
   N <- nrow(train.pca)
   ind <- sample(1:N, size = floor(N * 0.8), replace = FALSE)
   return(list(train.pca = train.pca[ind, ], test.pca = test.pca))
}
################################################################################
# metric multidimensional scaling plots, colors by class
# col <- class + 1
# pairs(train.pca, pch = 3, col = col)
# 
# dm1 <- dist(train)       
# coords1 <- cmdscale(dm1, 2)
# plot(coords1, las = 1, pch = 3, col = col, xlim = c(-20, 20), ylim = c(-10, 10),
#      xlab = '', ylab = '', main = 'Metric multidimensional scaling, train data')
# 
# dm2 <- dist(train.pca)       
# coords2 <- cmdscale(dm2, 2)
# plot(coords2, las = 1, pch = 3, col = col, xlim = c(-5, 5),
#      xlab = '', ylab = '', main = 'Metric multidimensional scaling, PCA train data')

################################################################################
# function for undersampling major classes 
# it takes number of elements of minimum class and undersample all other classes 
# input: classvec = vector of class labels
#        dfpred = data frame with predictors
# output: list with undersampled classvec and dfpred

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
# function for oversampling minor classes 
# it takes all classes as they are and additionally oversample all minor classes 
# input: classvec = vector of class labels
#        dfpred = data frame with predictors
# output: list with undersampled classvec and dfpred

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
# function for equalizing classes, oversampling of minors and undersampling of majors  
# input: classvec = vector of class labels
#        dfpred = data frame with predictors
# output: list with undersampled classvec and dfpred

equalsample <- function(dfpred, classvec) {
   N <- length(classvec)
   tt <- table(classvec)
   Nequal <- round(mean(tt),0)
   Ntosample <- Nequal - tt
   out <- vector(mode = 'integer')
   for (i in 1:length(tt)) {
      x <- (1:N)[classvec == attr(tt[i], 'names')] 
      if (Ntosample[i] == 0) { # take all as they are
         out <- c(out, x) 
      }
      else if (Ntosample[i] > 0) { # take all and add sampled
         out <- c(out, x)
         out <- c(out, sample(x = x, size = Ntosample[i], replace = TRUE))
      }
      else { # take less then they are
         out <- c(out, sample(x = x, size = Nequal, replace = FALSE))
      }
   }
   return(list(dfpred = dfpred[out,], classvec = classvec[out]))
}

################################################################################
# Classification
classify <- function(trainDF, validDF) {
   # results DF
   resDF <- data.frame(matrix(0, ncol = nrow(validDF), nrow = 8))
   #############################################################################
   # Logistic regression
   logistic.fit <- glm(class ~ ., data = trainDF, family = binomial())
   # prediction on validation set
   pred.lg <- predict(logistic.fit, newdata = validDF, type = 'response')
   pred.lg <- as.vector(ifelse(pred.lg > 0.5, yes = 1, no = 0))
   resDF[1,] <- pred.lg
   
   #############################################################################
   # Classification trees
   library(party)
   tr <- ctree(class ~ ., data = trainDF)
   pred.tr <- predict(tr, newdata = validDF, type = 'response')
   pred.tr <- as.vector(ifelse(pred.tr > 0.5, yes = 1, no = 0))
   resDF[2,] <- pred.tr
   
   #############################################################################
   # Random forest 
   library(randomForest)
   tDF <- data.frame(trainDF[, -ncol(trainDF)], factor.class = factor(trainDF$class))
   rf <- randomForest(factor.class ~ ., data = tDF, importance = T, proximity = T)
   pred.rf <- predict(rf, newdata = validDF, type = 'response')
   pred.rf <- as.integer(pred.rf) - 1
   resDF[3,] <- pred.rf
   
   #############################################################################
   # SVM classificator
   library(kernlab)
   svm.fit <- ksvm(factor.class ~ ., data = tDF, kernel = 'rbfdot',
                   kpar = list(sigma = 0.05), C = 5, cross = 5)
   pred.sv <- predict(svm.fit, newdata = validDF, type = 'response')
   pred.sv <- as.integer(pred.sv) - 1
   resDF[4,] <- pred.sv
   
   #############################################################################
   # Naive Bayes
   library(e1071)
   nb.fit <- naiveBayes(class ~ ., data = trainDF)
   pred.nb <- predict(nb.fit, newdata = validDF, type = 'raw')
   pred.nb <- apply(X = pred.nb, MARGIN = 1, which.max) - 1
   resDF[5,] <- pred.nb
   
   #############################################################################
   # Bayes Trees
   library(BayesTree)
   bart.fit <- bart(x.train = trainDF[,-ncol(trainDF)], y.train = trainDF$class, 
                    x.test = validDF, ndpost = 500, verbose = FALSE)
   pred.brt <- rep(-1, nrow(validDF))
   for (i in 1:length(pred.brt)) {
      pred.brt[i] <- sample.mode(pnorm(bart.fit$yhat.test[,i]), method = 'density')
   }
   pred.brt <- as.vector(ifelse(pred.brt > 0.5, yes = 1, no = 0))
   resDF[6,] <- pred.brt
   
   #############################################################################
   # Gaussian Process
   gp.fit <- gausspr(factor.class ~ ., data = tDF)
   pred.gp <- predict(gp.fit, validDF)
   pred.gp <- as.integer(pred.gp) - 1
   resDF[7,] <- pred.gp
   
   #############################################################################
   # # Neural network classificator
   # library(neuralnet)
   # regressors <- paste(names(trainDF)[-length(trainDF)], collapse = '+')
   # f <- as.formula(paste("class ~", regressors))
   # nn.fit <- neuralnet(formula = f, data = trainDF)
   # pred.nn <- compute(nn.fit, validDF)
   # pred.nn <- pred.nn$net.result[,1]
   # pred.nn <- as.vector(ifelse(pred.nn > 0.5, yes = 1, no = 0))
   # pred.nn[is.na(pred.nn)] <- as.integer(runif(1) > 0.5)
   # resDF[8,] <- pred.nn
   
   #############################################################################
   # # kNN
   # library(class)
   # knn.fit <- knn(train = trainDF[,-ncol(trainDF)], test = validDF, 
   #                cl = factor(trainDF$class), k = 3)
   # pred.knn <- as.integer(knn.fit) - 1
   # resDF[9,] <- pred.knn
   
   #############################################################################
   return(resDF)
}
################################################################################
# Running all 
################################################################################
set.seed(34)
res <- NULL
for (i in 1:10) {
   data <- preprocess.data()
   trainDF <- data$train.pca 
   validDF <- data$test.pca
   if (i != 1) { res <- rbind(res, classify(trainDF, validDF)) }
   else { res <- classify(trainDF, validDF) }
} 
# final assembled prediction by voting
pred <- as.vector(apply(res, 2, function(x) { as.integer(sum(x) > length(x)/2)}))
pred <- ifelse(pred == 1, yes = 'Y', no = 'N')

# write to file the prediction 
testset <- read.csv('test_Y3wMUE5_7gLdaTN.csv', header=T, sep=',', na.strings='')
output <- data.frame(testset$Loan_ID, pred)
names(output) <- c('Loan_ID', 'Loan_Status')
write.table(output, file = 'solution9.csv', sep = ',', row.names = FALSE, quote = FALSE)
################################################################################




















