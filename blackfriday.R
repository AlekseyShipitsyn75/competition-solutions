###############################################################################
# Black Friday competition
# author: Aleksey Shipitsyn
# 2016-07-29

###############################################################################
# Read and clean data
################################################################################
train <- read.table('train.csv', header = T, sep = ',', na.strings = '', 
                    stringsAsFactors = FALSE)
test <-  read.table('test-comb.csv', header = T, sep = ',', na.strings = '', 
                    stringsAsFactors = FALSE)
summary(train)
summary(test)

# check NA and NaN
apply(train, 2, function(x) {sum(is.na(x) | is.nan(x))})
apply(test,  2, function(x) {sum(is.na(x) | is.nan(x))})

# substitute zeros in NA values of product categories 2 and 3
fillna <- function(x) {
   x[is.na(x)] <- 0
   return(x)
}
train[,c(10,11)] <- apply(X = train[,c(10,11)], MARGIN = 2, FUN = fillna)
test[,c(11,12)] <- apply(X = test[,c(11,12)], MARGIN = 2, FUN = fillna)

################################################################################
# transform variables
train$Gender <- ifelse(train$Gender == 'M', yes = 1, no = 0)
test$Gender <- ifelse(test$Gender == 'M', yes = 1, no = 0)

train$Age <- factor(train$Age)
test$Age <- factor(test$Age, levels = levels(train$Age))
train$Age <- as.integer(train$Age)
test$Age <- as.integer(test$Age)

train$Occupation <- factor(train$Occupation)
test$Occupation <- factor(test$Occupation, levels = levels(train$Occupation))

train$City_Category <- factor(train$City_Category)
test$City_Category <- factor(test$City_Category, levels = levels(train$City_Category))

train$Stay_In_Current_City_Years[train$Stay_In_Current_City_Years == '4+'] <- '4'
train$Stay_In_Current_City_Years <- as.integer(train$Stay_In_Current_City_Years)
test$Stay_In_Current_City_Years[test$Stay_In_Current_City_Years == '4+'] <- '4'
test$Stay_In_Current_City_Years <- as.integer(test$Stay_In_Current_City_Years)

lev <- c(0:20)
train$Product_Category_1 <- factor(train$Product_Category_1, levels = lev)
train$Product_Category_2 <- factor(train$Product_Category_2, levels = lev)
train$Product_Category_3 <- factor(train$Product_Category_3, levels = lev)
test$Product_Category_1  <- factor(test$Product_Category_1,  levels = lev)
test$Product_Category_2  <- factor(test$Product_Category_2,  levels = lev)
test$Product_Category_3  <- factor(test$Product_Category_3,  levels = lev)
################################################################################
# factors to dummies
################################################################################
# transfrom factors into dummy variables 
# input: df = data frame with factor variables
# output: data frame where instead of each factor variable there is a matrix of dummy variables
factor2dummy <- function(df) {
   out <- data.frame(vector(mode = 'integer', length = nrow(df)))
   for (i in 1:ncol(df)) {
      v <- df[, i]
      if(class(v) != 'factor') { 
         out <- data.frame(out, v) 
         names(out)[ncol(out)] <- names(df)[i]
      }
      else { 
         lev <- levels(v)
         mat <- matrix(0, nrow = nrow(df), ncol = length(lev))
         dimnames(mat)[[2]] <- paste(names(df)[i], lev, sep = '.')
         for (j in 1:length(v)) {
            mat[j, ] <- as.integer(v[j] == lev)
         }
         out <- data.frame(out, mat)
      }
   }
   return(out[,-1])
}

train.dummy <- factor2dummy(train[,-c(1,2)])
test.dummy <- factor2dummy(test[, -c(1,2,3,13)])

################################################################################
# fitting models
################################################################################
# linear model
lm.fit <- lm(Purchase ~ ., data = train.dummy)
pred.lm <- predict(lm.fit, newdata = test.dummy)

################################################################################
# submission
submit <- data.frame(User_ID = test$User_ID, 
                     Product_ID = test$Product_ID,
                     Purchase = round(pred.lm, 0))
write.table(submit, file = 'submit1.csv', row.names = FALSE, col.names = TRUE, 
            quote = FALSE, sep = ',')
################################################################################
