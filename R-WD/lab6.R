housing.df <- read.csv("BostonHousing.csv")
housing.df$RAD <- as.factor(housing.df$RAD) #conver to factor 
housing.df <- housing.df[,-14]
str(housing.df)
set.seed(5)
train.index <- sample(nrow(housing.df), nrow(housing.df)*.9)
valid.index <- as.numeric(setdiff(rownames(housing.df), train.index))
housing.train <- housing.df[train.index,]
housing.valid <- housing.df[valid.index,]
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
housing.rt <- rpart(MEDV ~ ., data = housing.train )
housing.rt
prp(housing.rt, digits = 4, type = 1, extra = 1, varlen = -10, 
    box.col = ifelse(housing.rt$frame$var== "<leaf>" , "gray", "white"))
housing.train.rt.pred <- predict(housing.rt)
library(caret)
RMSE(housing.train.rt.pred, housing.train$MEDV)
housing.valid.rt.pred <- predict(housing.rt, newdata = housing.valid)
RMSE(housing.valid.rt.pred, housing.valid$MEDV)
housing.lm <- lm(MEDV ~ ., data = housing.train)
housing.train.lm.pred <- predict(housing.lm)
housing.valid.lm.pred <-  predict(housing.lm, newdata = housing.valid)
RMSE(housing.train.lm.pred, housing.train$MEDV)
RMSE(housing.valid.lm.pred, housing.valid$MEDV)

#Start Classification Trees
credit.data <- read.csv("credit_default.csv")
library("dplyr")
credit.data <- rename(credit.data, default = default.payment.next.month)

#convert categorical data into factors
credit.data$SEX <- as.factor(credit.data$SEX)
credit.data$EDUCATION <- as.factor(credit.data$EDUCATION)
credit.data$MARRIAGE <- as.factor(credit.data$MARRIAGE)

set.seed(11)
index <- sample(nrow(credit.data), nrow(credit.data) *.8)
credit.train <- credit.data[index,]
credit.valid <-  credit.data[-index,]
credit.ct0 <- rpart(default ~ ., data= credit.train, method = "class")
pred0 <- predict(credit.ct0, type = "class")
confusionMatrix(pred0, as.factor(credit.train$default), positive = "1")
credit.ct <- rpart(default ~ ., data= credit.train, method = "class",
            parms = list(loss= matrix(c(0,5,1,0), nrow = 2)))
credit.ct
prp(credit.ct, type = 1, extra = 1, varlen = -10, 
    box.col = ifelse(credit.ct$frame$var== "<leaf>", "gray", "white"))
credit.train.ct.pred1 <- predict(credit.ct, credit.train, type = "class")
confusionMatrix(credit.train.ct.pred1, as.factor(credit.train$default),  positive= "1")
credit.valid.ct.pred1 <-predict(credit.ct, credit.valid, type = "class")
confusionMatrix(credit.valid.ct.pred1, as.factor(credit.valid$default), positive = "1")

pred.prob1 <- predict(credit.ct, credit.valid, type = "prob")
head(pred.prob1)
cost <- function(r, pi){
  weight1 <- 5
  weight0 <- 1
  c1 <- (r==1) & (pi==0)
  c0 <- (r==0) & (pi==1)
  return(mean(weight1 *c1 +weight0 * c0))
  } 
cost(credit.train$default, credit.train.ct.pred1)  

credit.glm <- glm(default ~ ., data= credit.train, family = "binomial")

#get binary predicition 
credit.valid.glm.pred <- as.numeric(predict(credit.glm, credit.valid, type = "response") > .23)

#calc cost using valid set
cost(credit.valid$default, credit.valid.glm.pred)
confusionMatrix(as.factor(credit.valid.glm.pred), as.factor(credit.valid$default), positive = "1")

credit.valid.ct.prob <- predict(credit.ct,credit.valid, type = "prob")
library(pROC)
r <- roc(credit.valid$default, credit.valid.ct.prob[,2])
plot.roc(r)
auc(r)


housing.largetree <- rpart(MEDV ~ ., data = housing.train, cp = 0.001)
prp(housing.largetree, digits= 4, type= 1, extra = 1, varlen= -10, box.col = ifelse(housing.largetree$frame$var == "<leaf>", "gray", "white"))
plotcp(housing.largetree)
printcp(housing.largetree)

