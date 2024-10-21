#-------------------------------------------------------------------------------
# Competition - Time Series Analysis
# https://datahack.analyticsvidhya.com/contest/practice-problem-time-series-2/
# author: Aleksey Shipitsyn

#-------------------------------------------------------------------------------
# Extract features from date-time
# input: dates = vector with dates in POSIXlt format
#        year = year
#        mon = month 
#        mday = day of month 
#        wday = day of week
#        yday = day of year
#        yweek = week of year
#        hour = hour as 00-23
#        minute = minutes as 00-59
#        dtime = day time in hours with minutes as decimals
#        dpart = part of a day (night, morning, afternoon, evening)
#        tofactors = TRUE to convert categorical features to factor
#        varname = assembling name of variable to create names of features
# output: data frame with extracted features

expand.dates <- function(dates, year=F, mon=F, mday=F, wday=F, yday=F, yweek=F, 
                         hour=F, minute=F, dtime=F, dpart=F, tofactors=F, 
                         return.dates=F, varname='date') {
   
   if (sum(c(year, mon, mday, wday, yday, yweek, 
             hour, minute, dtime, dpart, tofactors, return.dates)) == 0) {
      return(NULL)
   }
   
   dates <- as.POSIXlt(dates)
      
   # output data frame
   dd <- data.frame(dates=dates, stringsAsFactors = F)
   
   if (year)  { 
      dd$year <- dates$year + 1900 
      if (tofactors) { dd$year <- factor(dd$year) }
   }
   if (mon)   { 
      dd$mon  <- dates$mon + 1     
      if (tofactors) { dd$mon <- factor(dd$mon, levels=1:12) }
   }
   if (mday)  { 
      dd$mday <- dates$mday        
      if (tofactors) { dd$mday <- factor(dd$mday, levels=1:31) }
   }
   if (wday)  { 
      dd$wday <- dates$wday + 1    
      if (tofactors) { dd$wday <- factor(dd$wday, levels=1:7) }
   }
   if (yday)  { 
      dd$yday <- dates$yday + 1    
      if (tofactors) { dd$yday <- factor(dd$yday, levels=1:366) }
   }
   if (yweek) { 
      dd$yweek <- as.integer(format(dates, '%V')) # ISO 8601 convention
      if (tofactors) { dd$yweek <- factor(dd$yweek, levels=1:53) }
   } 
   if (hour)  { 
      dd$hour <- dates$hour    
      if (tofactors) { dd$hour <- factor(dd$hour, levels=0:23) }
   }
   if (minute){ 
      dd$minute <- dates$min 
   }
   if (dtime | dpart) {
      daytime <- dates$hour + dates$min / 60 + dates$sec / 3600 
      if (dtime) { 
         dd$dtime <- daytime
      }
      if (dpart) {
         lab <- c('night','morning','afternoon','evening')
         # cut to day parts 
         dd$dpart <- cut(daytime, breaks = seq(0, 24, by=6), right=F, labels=lab)
      }
   }
   # put proper names to output columns
   names(dd) <- paste(varname, names(dd), sep='.')
   
   # delete input dates
   if (!return.dates) { dd <- dd[,-1] }
   
   return(dd)
}

#-------------------------------------------------------------------------------
# Categorical to dummy, https://www.otexts.org/fpp/5/2
# for N categories of some variable creates vector of zero-ones of length N-1 
# input: dd = data frame with factor variables
# output: dd where instead of each factor variable a matrix of dummy variables

factor2dummy <- function(dd) {
   out <- data.frame(vector(mode='integer', length=nrow(dd)))
   for (i in 1:ncol(dd)) {
      v <- dd[, i]
      if(class(v)[1] != 'factor') { 
         out <- cbind(out, v) 
         names(out)[ncol(out)] <- names(dd)[i]
      }else{
         lev <- levels(v)
         if (length(lev) == 2) {
            out$v <- as.integer(v) - 1
            names(out)[ncol(out)] <- paste(names(dd)[i], lev[2], sep='.')
         }else{
            mat <- matrix(0, nrow=nrow(dd), ncol=length(lev))
            dimnames(mat)[[2]] <- paste(names(dd)[i], lev, sep = '.')
            for (j in 1:length(v)) {
               mat[j, ] <- as.integer(v[j] == lev)
            }
            out <- cbind(out, mat[,-1]) # first level all zeros
         }
      }
   }
   return(out[,-1])
}

#-------------------------------------------------------------------------------
# Data exploration
#-------------------------------------------------------------------------------
setwd("~/Documents/Competitions/Time series")
readLines('Train_SU63ISt.csv', n=10)
readLines('Test_0qrQsBZ.csv', n=10)

# train set
train <- read.csv('Train_SU63ISt.csv', stringsAsFactors = F)
train$Datetime <- as.POSIXlt(train$Datetime, format = '%d-%m-%Y %H:%M')
Ntrain <- nrow(train)

plot(train[,-1], type='l')
plot(train[1:1000, -1], type='l')
plot(train[(nrow(train)-1000):nrow(train), -1], type='l')

# test set
test <- read.csv('Test_0qrQsBZ.csv', stringsAsFactors = F)
test$Datetime <- as.POSIXlt(test$Datetime, format = '%d-%m-%Y %H:%M')
Ntest <- nrow(test)

#-------------------------------------------------------------------------------
# Data preprocessing
#-------------------------------------------------------------------------------
# combime dates of train and test, extract date features and convert to dummies
X <- expand.dates(c(train$Datetime, test$Datetime), year=T, mon=T, mday=T, 
                  wday=T, yday=T, yweek=T, hour=T, tofactors=T)
X <- factor2dummy(X)

#-------------------------------------------------------------------------------
# Poisson model
#-------------------------------------------------------------------------------
fit <- glm(Count~., 
           data=data.frame(X[1:Ntrain,], Count=train$Count, stringsAsFactors=F), 
           family=poisson(link="log"))
fit
summary(fit)

# plots of fitted values
plot(x=train$Datetime[1:1000], y=train$Count[1:1000], type='l')
lines(x=train$Datetime[1:1000], y=fit$fitted.values[1:1000], type='l', col='red')

plot(x=train$Datetime[(nrow(train)-1000):nrow(train)], 
     y=train[(nrow(train)-1000):nrow(train), 'Count'], type='l')
lines(x=train$Datetime[(nrow(train)-1000):nrow(train)], 
      y=fit$fitted.values[(nrow(train)-1000):nrow(train)], type='l', col='red')

# prediction on test set
Ytest <- predict(object=fit, newdata=X[(Ntrain+1):nrow(X),], type='response')
plot(x=test$Datetime, y=Ytest, type='l', col='red')

# write to file
res <- data.frame(ID=test$ID, Count=round(Ytest), stringsAsFactors=F)
write.table(x=res, file='res.csv', col.names=T, row.names=F, sep=',', quote=F)

#-------------------------------------------------------------------------------



