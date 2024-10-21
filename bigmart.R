################################################################################
# Bigmart competition
# author: Aleksey Shipitsyn

################################################################################
# Read and clean data
################################################################################
train <- read.csv('Train_UWu5bXk.csv', header=T, sep=',', na.strings='', stringsAsFactors = F)
test <- read.csv('Test_u94Q5KV.csv', header=T, sep=',', na.strings='', stringsAsFactors = F)
# summary(train)
# summary(test)

# unique(train$Item_Identifier)
# unique(test$Item_Identifier)
train$Item_Identifier <- factor(train$Item_Identifier)
test$Item_Identifier <- factor(test$Item_Identifier, 
                               levels = levels(train$Item_Identifier))

# adjust levels of Item_Fat_Content and make it binary integer {low = 0, regular = 1}
# unique(train$Item_Fat_Content)
# unique(test$Item_Fat_Content)
for (i in 1:nrow(train)) {
   if (train$Item_Fat_Content[i] == 'low fat' 
       | train$Item_Fat_Content[i] == 'LF'
       | train$Item_Fat_Content[i] == 'Low Fat') { 
      train$Item_Fat_Content[i] <- 'Low' 
   }
   else { train$Item_Fat_Content[i] <- 'Reg' }
}
for (i in 1:nrow(test)) {
   if (test$Item_Fat_Content[i] == 'low fat' 
       | test$Item_Fat_Content[i] == 'LF'
       | test$Item_Fat_Content[i] == 'Low Fat') { 
      test$Item_Fat_Content[i] <- 'Low' 
   }
   else { test$Item_Fat_Content[i] <- 'Reg' }
}
train$Item_Fat_Content <- factor(train$Item_Fat_Content)
test$Item_Fat_Content <- factor(test$Item_Fat_Content, 
                                levels = levels(train$Item_Fat_Content))

# check levels and transform to factors
# sort(unique(train$Item_Type)) == sort(unique(test$Item_Type))
train$Item_Type <- factor(train$Item_Type)
test$Item_Type <- factor(test$Item_Type, levels = levels(train$Item_Type))

# sort(unique(train$Outlet_Identifier)) == sort(unique(test$Outlet_Identifier))
train$Outlet_Identifier <- factor(train$Outlet_Identifier)
test$Outlet_Identifier <- factor(test$Outlet_Identifier, 
                                 levels = levels(train$Outlet_Identifier))

# Outlet_Size has NA, make NA separate category
# sort(unique(train$Outlet_Size)) == sort(unique(test$Outlet_Size))
train$Outlet_Size <- factor(train$Outlet_Size)
test$Outlet_Size <- factor(test$Outlet_Size, levels = levels(train$Outlet_Size))

# sort(unique(train$Outlet_Location_Type)) == sort(unique(test$Outlet_Location_Type))
train$Outlet_Location_Type <- factor(train$Outlet_Location_Type)
test$Outlet_Location_Type <- factor(test$Outlet_Location_Type, 
                           levels = levels(train$Outlet_Location_Type))

# sort(unique(train$Outlet_Type)) == sort(unique(test$Outlet_Type))
train$Outlet_Type <- factor(train$Outlet_Type)
test$Outlet_Type <- factor(test$Outlet_Type, levels = levels(train$Outlet_Type))

# Item_Weight imputation, imput value for the same Item from other rows
Weight <- c(train$Item_Weight, test$Item_Weight)
ID <- c(train$Item_Identifier, test$Item_Identifier)
for (i in 1:length(Weight)) {
   if (is.na(Weight[i])) { Weight[i] <- mean(Weight[ID == ID[i]], na.rm = TRUE) }
}
train$Item_Weight <- Weight[1:nrow(train)]
test$Item_Weight <- Weight[(nrow(train)+1):length(Weight)]

# check NA and NaN
apply(train, 2, function(x) {sum(is.na(x) | is.nan(x))})
apply(test,  2, function(x) {sum(is.na(x) | is.nan(x))})
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
################################################################################
# function to convert ordinal factor variable to integer {0,1,2,...}
size2integer <- function(x) {
   x <- 4 - as.integer(x)
   x[is.na(x)] <- 0
   return(x)
}
################################################################################
# binary to integer {0,1}
train$Item_Fat_Content <- as.integer(train$Item_Fat_Content) - 1
test$Item_Fat_Content <- as.integer(test$Item_Fat_Content) - 1
# ordered size to integer {0,1,2,3}
train$Outlet_Size <- size2integer(train$Outlet_Size)
test$Outlet_Size <- size2integer(test$Outlet_Size)

# transform factors to dummy vatiables
train.dummy <- factor2dummy(train)
test.dummy <- factor2dummy(test)

################################################################################
# Fitting models
################################################################################
# linear model
lm.fit <- lm(Item_Outlet_Sales ~ ., data = train.dummy)
pred.lm <- predict(lm.fit, newdata = test.dummy)

# shrinkage 
library(glmnet)
x <- scale(train.dummy[,-ncol(train.dummy)])
y <- scale(train.dummy[,ncol(train.dummy)])
x.test <- scale(test.dummy, center = attr(x, 'scaled:center'), 
                              scale = attr(x, 'scaled:scale'))
# ridge 
model.ridge <- cv.glmnet(x, y, alpha = 0)
pred.ridge <- predict(model.ridge, newx = x.test, s = "lambda.min")
pred.ridge <- pred.ridge * attr(y,'scaled:scale') + attr(y, 'scaled:center')

# lasso the best
model.lasso <- cv.glmnet(x, y, alpha = 1)
pred.lasso <- predict(model.lasso, newx = x.test, s = "lambda.min")
pred.lasso <- pred.lasso * attr(y,'scaled:scale') + attr(y, 'scaled:center')

# elastic net
model.elnet <- cv.glmnet(x, y, alpha = 0.5)
pred.elnet <- predict(model.elnet, newx = x.test, s = "lambda.min")
pred.elnet <- pred.elnet * attr(y,'scaled:scale') + attr(y, 'scaled:center')

# averaging 
pred.avg <- (pred.lm + pred.ridge + pred.lasso + pred.elnet) / 4


################################################################################
# submission
submit <- read.csv('SampleSubmission_TmnO39y.csv', header = TRUE, sep = ',', 
                   na.strings = '', stringsAsFactors = FALSE)
submit$Item_Outlet_Sales <- pred.avg
write.table(submit, file = 'submit8.csv', row.names = FALSE, col.names = TRUE, 
            quote = FALSE, sep = ',')
################################################################################

