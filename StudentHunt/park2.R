################################################################################
# Footfall parks competition
# author: Aleksey Shipitsyn
################################################################################
# read and preprocess data
train <- read.csv('Train_xyqdbho.csv', sep = ',', stringsAsFactors = FALSE)
train$Date <- as.POSIXlt(train$Date, format = '%d-%m-%Y')
################################################################################
dates2features <- function(dates) {
   year <- dates$year
   year <- ifelse(year >= 90, yes = year + 1900, no = year + 2000)
   month <- dates$mon + 1
   mday <- dates$mday
   wday <- dates$wday + 1
   yday <- dates$yday + 1
   return(data.frame(year, month, mday, wday, yday))
} 
################################################################################
dates <- dates2features(train$Date)
train <- cbind(dates, train)
################################################################################
# function to get interval of days of the year given day
daysinterval <- function(day, interval = 10) {
   low <- day - interval
   if (low < 0) low <- 365 + low
   upp <- day + interval
   if (upp > 365) upp <- upp - 365
   return(c(low, upp))
}
################################################################################
# function to impute NA by sampling conditioning on Date and Location_Type
impute.cond <- function(DF, from) {
   for(i in 1:nrow(DF)) {
      if (sum(is.na(DF[i, ])) > 0) {
         location <- DF[i, ]$Location_Type
         days <- daysinterval(DF[i, ]$yday, 5)
         # similar observations by day of the year and location
         similar <- from[(from$yday >= days[1] & from$yday <= days[2]) 
                         & (from$Location_Type == location), ]
         similar <- na.omit(similar)
         if (nrow(similar) > 0) {
            nacol <- which(is.na(DF[i,]))
            sampind <- sample(1:nrow(similar), size = 1) 
            DF[i, nacol] <- similar[sampind, nacol]
         }
      }
   } 
   return(DF)
}
################################################################################   
imptrain <- impute.cond(DF = train, from = train)
# check
apply(imptrain, MARGIN = 2, FUN = function(x) { sum(is.na(x)) } )
cor(imptrain[,-c(6,8)], use = 'complete.obs')
################################################################################   
# function for regression imputation
# input: X - data frame of regressors, there is no NA in X
#        y - variable to impute
# output: imputed y 
impute.regression <- function(y, X) {
   ind <- which(is.na(y))
   fit <- lm(y ~ ., data = data.frame(y,X), na.action = 'na.omit')
   pred <- predict(fit, newdata = X) 
   y[ind] <- pred[ind]
   return(y)
}
################################################################################   
# impute NA with regression model taking all vars without NA as predictors
y <- imptrain$Direction_Of_Wind
X <- imptrain[, -c(6,8:18)]
imptrain$Direction_Of_Wind <- impute.regression(y = y, X = X)

y <- imptrain$Average_Breeze_Speed
X <- imptrain[, -c(6,8,10:18)]
imptrain$Average_Breeze_Speed <- impute.regression(y = y, X = X)

y <- imptrain$Max_Breeze_Speed
X <- imptrain[, -c(6,8,11:18)]
imptrain$Max_Breeze_Speed <- impute.regression(y = y, X = X)

y <- imptrain$Min_Breeze_Speed
X <- imptrain[, -c(6,8,12:18)]
imptrain$Min_Breeze_Speed <- impute.regression(y = y, X = X)

y <- imptrain$Var1
X <- imptrain[, -c(6,8,13:18)]
imptrain$Var1 <- impute.regression(y = y, X = X)

y <- imptrain$Average_Atmospheric_Pressure
X <- imptrain[, -c(6,8,14:18)]
imptrain$Average_Atmospheric_Pressure <- impute.regression(y = y, X = X)

y <- imptrain$Max_Atmospheric_Pressure
X <- imptrain[, -c(6,8,15:18)]
imptrain$Max_Atmospheric_Pressure <- impute.regression(y = y, X = X)

y <- imptrain$Min_Atmospheric_Pressure
X <- imptrain[, -c(6,8,16:18)]
imptrain$Min_Atmospheric_Pressure <- impute.regression(y = y, X = X)

y <- imptrain$Min_Ambient_Pollution
X <- imptrain[, -c(6,8,17:18)]
imptrain$Min_Ambient_Pollution <- impute.regression(y = y, X = X)

y <- imptrain$Max_Ambient_Pollution
X <- imptrain[, -c(6,8,18)]
imptrain$Max_Ambient_Pollution <- impute.regression(y = y, X = X)
################################################################################   
# read and preprocess test set
test <- read.csv('Test_pyI9Owa.csv', sep = ',', stringsAsFactors = FALSE)
test$Date <- as.POSIXlt(test$Date, format = '%d-%m-%Y')
dates <- dates2features(test$Date)
test <- cbind(dates, test)

# combine test and train sets for imputation of test set
test$Footfall <- NA
data <- rbind(test, train)

# impute test set
imptest <- impute.cond(DF = test, from = data)
# check
apply(imptest, 2, function(x) sum(is.na(x)))

# impute NA with regression model taking all vars without NA as predictors
y <- imptest$Direction_Of_Wind
X <- imptest[, -c(6,8:18,23)]
imptest$Direction_Of_Wind <- impute.regression(y = y, X = X)

y <- imptest$Average_Breeze_Speed
X <- imptest[, -c(6,8,10:18,23)]
imptest$Average_Breeze_Speed <- impute.regression(y = y, X = X)

y <- imptest$Max_Breeze_Speed
X <- imptest[, -c(6,8,11:18,23)]
imptest$Max_Breeze_Speed <- impute.regression(y = y, X = X)

y <- imptest$Min_Breeze_Speed
X <- imptest[, -c(6,8,12:18,23)]
imptest$Min_Breeze_Speed <- impute.regression(y = y, X = X)

y <- imptest$Var1
X <- imptest[, -c(6,8,13:18,23)]
imptest$Var1 <- impute.regression(y = y, X = X)

y <- imptest$Average_Atmospheric_Pressure
X <- imptest[, -c(6,8,14:18,23)]
imptest$Average_Atmospheric_Pressure <- impute.regression(y = y, X = X)

y <- imptest$Max_Atmospheric_Pressure
X <- imptest[, -c(6,8,15:18,23)]
imptest$Max_Atmospheric_Pressure <- impute.regression(y = y, X = X)

y <- imptest$Min_Atmospheric_Pressure
X <- imptest[, -c(6,8,16:18,23)]
imptest$Min_Atmospheric_Pressure <- impute.regression(y = y, X = X)

y <- imptest$Min_Ambient_Pollution
X <- imptest[, -c(6,8,17:18,23)]
imptest$Min_Ambient_Pollution <- impute.regression(y = y, X = X)

y <- imptest$Max_Ambient_Pollution
X <- imptest[, -c(6,8,18,23)]
imptest$Max_Ambient_Pollution <- impute.regression(y = y, X = X)

y <- imptest$Footfall
X <- imptest[, -c(6,8,23)]
imptest$Footfall <- impute.regression(y = y, X = X)
################################################################################  
# scale data
sctrain <- scale(imptrain[,-c(6,8,23)])
sctest  <- scale(imptest[,-c(6,8,23)], 
                 center = attr(sctrain, 'scaled:center'),
                 scale = attr(sctrain, 'scaled:scale'))

################################################################################   
# add more features
################################################################################   
# features of past days  
features.past <- function(df, n = 1) {
   newdf <- df[c(rep(1,1),1:(nrow(df)-1)),]
   if (n > 1) {
      for (i in 2:n) {
         newdf <- data.frame(newdf, df[c(rep(1,i),1:(nrow(df)-i)),])
      }
   }
   return(newdf)
}
################################################################################   
# paired interactions, for operation = 'product' squares and paired products
# for operation = 'division' squared roots and paired divisions
features.interact2 <- function(df, operation = 'product') {
   N <- ncol(df)
   out <- matrix(0, nrow = nrow(df), ncol = N*N - N*(N-1)/2)
   cnt <- 1
   for (i in 1:N) {
      for(j in i:N) {
         if (operation == 'product') { 
            out[,cnt] <- df[,i] * df[,j] 
         }
         if (operation == 'division') { 
            out[,cnt] <- ifelse(i == j, yes = sign(df[,i]) * sqrt(abs(df[,i])), 
                                no = df[,j] / (df[,i]+0.000001)) 
         }
         cnt <- cnt + 1
      }
   }
   return(data.frame(out))
}
################################################################################   
scdata <- data.frame(rbind(sctrain, sctest))
# scdata <- data.frame(scdata, features.past(df = scdata[,-c(1,2,6,20)], n = 1))
sq <- features.interact2(df = scdata[,-c(1,2,6,20)], operation = 'product')
scdata <- data.frame(rbind(sctrain, sctest))
scdata <- data.frame(scdata, features.past(df = scdata[,-c(1,2,6,20)], n = 5))
scdata <- data.frame(scdata, sq)

# scale everything
scdata <- scale(scdata)

# lm model to predict Footfall
model.lm <- lm(Footfall~., data=data.frame(Footfall=train$Footfall, scdata[1:nrow(train),])) 
summary(model.lm)
pred.lm <- predict(model.lm, newdata = data.frame(scdata[(nrow(train)+1):nrow(scdata),]))

# Random forest
require(randomForest)
rf <- randomForest(y = train$Footfall, x = scdata[1:nrow(train),],
                   xtest = scdata[(nrow(train)+1):nrow(scdata),], ntree = 20)
pred.rf <- round(rf$test$predicted)

require(glmnet) 
# ridge 
model.ridge <- cv.glmnet(x=scdata[1:nrow(train),], y=train$Footfall, alpha=0, nfolds=5)
pred.ridge <- predict(model.ridge, newx=scdata[(nrow(train)+1):nrow(scdata),], s="lambda.min")

# lasso
model.lasso <- cv.glmnet(x=scdata[1:nrow(train),], y=train$Footfall, alpha=1, nfolds=5)
pred.lasso <- predict(model.lasso, newx=scdata[(nrow(train)+1):nrow(scdata),], s="lambda.min")

################################################################################   
# predict on test set and fill output file
df <- data.frame(ID = imptest$ID, Footfall = pred.rf)
names(df)[2] <- 'Footfall'
write.table(df, file = 'solution28.csv', sep = ',')









