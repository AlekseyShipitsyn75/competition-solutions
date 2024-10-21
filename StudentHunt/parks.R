################################################################################
# Footfall parks competition
# author: Aleksey Shipitsyn
# 2016-09-28

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
         days <- daysinterval(DF[i, ]$yday)
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
apply(imptrain, 2, function(x) sum(is.na(x)))
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
imptrain$Direction_Of_Wind <- impute.regression(y,X)

y <- imptrain$Average_Breeze_Speed
X <- imptrain[, -c(6,8,10:18)]
imptrain$Average_Breeze_Speed <- impute.regression(y,X)

y <- imptrain$Max_Breeze_Speed
X <- imptrain[, -c(6,8,11:18)]
imptrain$Max_Breeze_Speed <- impute.regression(y,X)

y <- imptrain$Min_Breeze_Speed
X <- imptrain[, -c(6,8,12:18)]
imptrain$Min_Breeze_Speed <- impute.regression(y,X)

y <- imptrain$Var1
X <- imptrain[, -c(6,8,13:18)]
imptrain$Var1 <- impute.regression(y,X)

y <- imptrain$Average_Atmospheric_Pressure
X <- imptrain[, -c(6,8,14:18)]
imptrain$Average_Atmospheric_Pressure <- impute.regression(y,X)

y <- imptrain$Max_Atmospheric_Pressure
X <- imptrain[, -c(6,8,15:18)]
imptrain$Max_Atmospheric_Pressure <- impute.regression(y,X)

y <- imptrain$Min_Atmospheric_Pressure
X <- imptrain[, -c(6,8,16:18)]
imptrain$Min_Atmospheric_Pressure <- impute.regression(y,X)

y <- imptrain$Min_Ambient_Pollution
X <- imptrain[, -c(6,8,17:18)]
imptrain$Min_Ambient_Pollution <- impute.regression(y,X)

y <- imptrain$Max_Ambient_Pollution
X <- imptrain[, -c(6,8,18)]
imptrain$Max_Ambient_Pollution <- impute.regression(y,X)
################################################################################   
# read and preprocess test set
test <- read.csv('Test_pyI9Owa.csv', sep = ',', stringsAsFactors = FALSE)
test$Date <- as.POSIXlt(test$Date, format = '%d-%m-%Y')
dates <- dates2features(test$Date)
test <- cbind(dates, test)
imptest <- impute.cond(DF = test, from = train)
# check
apply(imptest, 2, function(x) sum(is.na(x)))

# impute NA with regression model taking all vars without NA as predictors
y <- imptest$Direction_Of_Wind
X <- imptest[, -c(6,8:18)]
imptest$Direction_Of_Wind <- impute.regression(y,X)

y <- imptest$Average_Breeze_Speed
X <- imptest[, -c(6,8,10:18)]
imptest$Average_Breeze_Speed <- impute.regression(y,X)

y <- imptest$Max_Breeze_Speed
X <- imptest[, -c(6,8,11:18)]
imptest$Max_Breeze_Speed <- impute.regression(y,X)

y <- imptest$Min_Breeze_Speed
X <- imptest[, -c(6,8,12:18)]
imptest$Min_Breeze_Speed <- impute.regression(y,X)

y <- imptest$Var1
X <- imptest[, -c(6,8,13:18)]
imptest$Var1 <- impute.regression(y,X)

y <- imptest$Average_Atmospheric_Pressure
X <- imptest[, -c(6,8,14:18)]
imptest$Average_Atmospheric_Pressure <- impute.regression(y,X)

y <- imptest$Max_Atmospheric_Pressure
X <- imptest[, -c(6,8,15:18)]
imptest$Max_Atmospheric_Pressure <- impute.regression(y,X)

y <- imptest$Min_Atmospheric_Pressure
X <- imptest[, -c(6,8,16:18)]
imptest$Min_Atmospheric_Pressure <- impute.regression(y,X)

y <- imptest$Min_Ambient_Pollution
X <- imptest[, -c(6,8,17:18)]
imptest$Min_Ambient_Pollution <- impute.regression(y,X)

y <- imptest$Max_Ambient_Pollution
X <- imptest[, -c(6,8,18)]
imptest$Max_Ambient_Pollution <- impute.regression(y,X)

################################################################################   
# scale data
sctrain <- scale(imptrain[,-c(6,8,23)])
sctest  <- scale(imptest[,-c(6,8)], center = attr(sctrain,'scaled:center'),
                 scale = attr(sctrain, 'scaled:scale'))

# fit models to predict Footfall
model.lm <- lm(Footfall ~ ., data = data.frame(Footfall = imptrain$Footfall, sctrain))
summary(model.lm)
pred.lm <- predict(model.lm, newdata = as.data.frame(sctest))

require(glmnet)
# ridge model
model.ridge <- cv.glmnet(x = sctrain, y = imptrain$Footfall, alpha = 0)
pred.ridge <- predict(model.ridge, newx = sctest, s = "lambda.min")
pred.ridge <- round(pred.ridge)

# Lasso model
model.lasso <- cv.glmnet(x = sctrain, y = imptrain$Footfall, alpha = 1)
pred.lasso <- predict(model.lasso, newx = sctest, s = "lambda.min")
pred.lasso <- round(pred.lasso)

# Random forest
require(randomForest)
rf <- randomForest(y = imptrain$Footfall, x = sctrain, xtest = sctest, ntree = 100)
pred.rf <- round(rf$test$predicted)

# Ada boost

################################################################################   
# predict on test set and fill output file
df <- data.frame(ID = imptest$ID, Footfall = pred.rf)
write.table(df, file = 'solution6.csv', sep = ',')










