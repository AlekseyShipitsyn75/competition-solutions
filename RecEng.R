#-------------------------------------------------------------------------------
# Competition: Recommender engine
# Aleksey Shipitsyn
# 2019-06-11

#-------------------------------------------------------------------------------
# Read data
#-------------------------------------------------------------------------------
setwd("~/Documents/Competitions/Recommender Engine")
dir()

samp_subm <- read.csv("sample_submission_SCGtj9F.csv", stringsAsFactors=F, na.strings=c('NA',''))
test_subm <- read.csv("test_submissions_NeDLEvX.csv", stringsAsFactors=F, na.strings=c('NA',''))

setwd("~/Documents/Competitions/Recommender Engine/train")
dir()

prob_data <- read.csv("problem_data.csv", stringsAsFactors=F, na.strings=c('NA',''))
train_subm <- read.csv("train_submissions.csv", stringsAsFactors=F, na.strings=c('NA',''))
user_data <- read.csv("user_data.csv", stringsAsFactors=F, na.strings=c('NA',''))

setwd("~/Documents/Competitions/Recommender Engine")


#-------------------------------------------------------------------------------
# Data exploration
#-------------------------------------------------------------------------------
# returns percentage of NA in columns
check_na_col <- function(df) {
  x <- apply(df, MARGIN=2, FUN = function(x) { sum(is.na(x)) / nrow(df) * 100})
  return(x[x > 0])
}

check_na_col(prob_data)
# level_type     points       tags 
# 2.032396  59.856357  53.239609 

check_na_col(user_data)
# country 
# 32.28787

#-------------------------------------------------------------------------------
# Data preprocessing
#-------------------------------------------------------------------------------
# Join all the data: user, problem, score
df <- merge(x=train_subm, y=user_data, all.x=T)
df <- merge(x=df, y=prob_data, all.x=T)

check_na_col(df)
#    country level_type     points       tags 
# 24.3748994  0.3992402 18.7224315  9.9339966 

# impute NA
df$country[is.na(df$country)] <- 'Unknown'
df$level_type[is.na(df$level_type)] <- 'U'
df$points[is.na(df$points)] <- median(df$points, na.rm=T)
df$tags[is.na(df$tags)] <- 'Unknown'

# days passed since registration and last login
df$registration_time_seconds <- Sys.time() - as.POSIXct(df$registration_time_seconds, origin="1970-01-01") 
df$last_online_time_seconds <- Sys.time() - as.POSIXct(df$last_online_time_seconds, origin="1970-01-01") 

# preprocess tags into term matrix
tags <- strsplit(df$tags, split=',')
utags <- unique(unlist(tags))
a <- t(sapply(tags, FUN=function(x){ as.integer(utags %in% x) }, simplify='array')) 
dimnames(a)[[2]] <- utags

# join tags 
df <- cbind(df, a)
df$tags <- NULL

# categorical to dummies
library('fastDummies')
df <- dummy_cols(df, select_columns=c('country','rank','level_type'), remove_first_dummy=T, remove_selected_columns=T)  

# train and test split
set.seed(9)
test <- sample(1:nrow(df), size=round(nrow(df)*0.2), replace=F)
df.test <- df[test,]
df.train <- df[-test, ]

# target distribution - skewed
hist(df$attempts_range)
table(df$attempts_range)
#     1     2     3     4     5     6 
# 82804 47320 14143  5499  2496  3033 

# # undersample major classes to size of 3000
# set.seed(52)
# df.train.under <- df.train[df.train$attempts_range == 5 | df.train$attempts_range == 6, ]
# for (i in 1:4) {
#   a <- df.train[df.train$attempts_range == i, ]
#   ind <- sample(1:nrow(a), size=3000, replace=F)
#   df.train.under <- rbind(df.train.under, a[ind, ])
# }
# 
#-------------------------------------------------------------------------------
# Performance metric - weighted F1
#-------------------------------------------------------------------------------
f1.weighted <- function(actual, predicted, cm.print=T) {
  require(caret)
  require(e1071)
  
  # confusion matrix
  cm <- confusionMatrix(data=predicted, reference=actual, mode='prec_recall')
  if (cm.print) { print(cm) }
  
  # compute weights by number of observations in each class
  w <- colSums(cm$table) / sum(cm$table)
  # weighted F1
  return( sum(w * cm$byClass[,'F1'], na.rm=T) )
}

# actual = as.factor(c(1,2,3,4,5,1,2,3,4,5))
# predicted = factor(c(2,1,3,4,2,1,2,3,2,5), levels=levels(actual))
# f1.weighted(actual, predicted) # 0.633333

#-------------------------------------------------------------------------------
# Modelling
#-------------------------------------------------------------------------------
# Poisson model 
model.poisson <- glm(attempts_range ~ ., family=poisson(link="log"), data=df.train[,-c(1,2)])

# performance on training set
actual <- factor(df.train$attempts_range)
predicted <- predict(model.poisson, newdata=df.train[, -c(1,2,3)], type='response')
predicted <- factor(as.integer(floor(predicted)), levels=levels(actual)) 
f1.weighted(actual=actual, predicted=predicted) # 0.4464

# performance on test set
act <- factor(df.test$attempts_range, levels=levels(actual))
pred <- predict(model.poisson, newdata=df.test[, -c(1,2,3)], type='response')
pred <- factor(as.integer(floor(pred)), levels=levels(actual)) 
f1.weighted(actual=act, predicted=pred) # 0.4497

#-------------------------------------------------------------------------------
# RF
library(randomForest)
model.rf <- randomForest(x=df.train[,-c(1,2,3)], y=actual, ntree=50)

# performance on training set
predicted <- predict(model.rf, newdata=df.train[,-c(1,2,3)], type='response')
f1.weighted(actual=actual, predicted=predicted) # 0.6668

# performance on test set
pred <- predict(model.rf, newdata=df.test[, -c(1,2,3)], type='response')
f1.weighted(actual=act, predicted=pred) # 0.4474

#-------------------------------------------------------------------------------
# retrain model on all training data


#-------------------------------------------------------------------------------
# submission data set






# new users check
sum(!(test_subm$user_id %in% train_subm$user_id)) # 16

# new items check
sum(!(test_subm$problem_id %in% train_subm$problem_id)) # 520





#-------------------------------------------------------------------------------






