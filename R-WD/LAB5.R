housing.df <- read.csv("BostonHousing.csv")
dim(housing.df)
t(t(names(housing.df)))
housing.df$RAD <- as.factor(housing.df$RAD)
dummies <- as.data.frame(model.matrix(~0 + RAD, data= housing.df))
housing.df <- cbind(housing.df[,-9], dummies)
str(housing.df)
housing.df.norm <- housing.df
housing.df.norm$CRIM <- (housing.df.norm$CRIM- min(housing.df.norm$CRIM))/(max(housing.df.norm$CRIM)- min(housing.df.norm$CRIM))
housing.df.norm$ZN <- (housing.df.norm$ZN- min(housing.df.norm$ZN))/ (max(housing.df.norm$ZN)- min(housing.df.norm$ZN))                                                                          
housing.df.norm$INDUS <- (housing.df.norm$INDUS - min(housing.df.norm$INDUS)) / (max(housing.df.norm$INDUS) - min(housing.df.norm$INDUS))
housing.df.norm$NOX <- (housing.df.norm$NOX - min(housing.df.norm$NOX)) / (max(housing.df.norm$NOX) - min(housing.df.norm$NOX))
housing.df.norm$RM <- (housing.df.norm$RM - min(housing.df.norm$RM)) / (max(housing.df.norm$RM) - min(housing.df.norm$RM))
housing.df.norm$AGE <- (housing.df.norm$AGE - min(housing.df.norm$AGE)) / (max(housing.df.norm$AGE) - min(housing.df.norm$AGE))
housing.df.norm$DIS <- (housing.df.norm$DIS - min(housing.df.norm$DIS)) / (max(housing.df.norm$DIS) - min(housing.df.norm$DIS))
housing.df.norm$TAX <- (housing.df.norm$TAX - min(housing.df.norm$TAX)) / (max(housing.df.norm$TAX) - min(housing.df.norm$TAX))
housing.df.norm$PTRATIO <- (housing.df.norm$PTRATIO - min(housing.df.norm$PTRATIO)) / (max(housing.df.norm$PTRATIO) - min(housing.df.norm$PTRATIO))
housing.df.norm$LSTAT <- (housing.df.norm$LSTAT - min(housing.df.norm$LSTAT)) / (max(housing.df.norm$LSTAT) - min(housing.df.norm$LSTAT))                         
summary(housing.df.norm)
set.seed(5)
train.index <- sample(nrow(housing.df.norm), nrow(housing.df.norm) * 0.6)
valid.index <- as.numeric(setdiff(rownames(housing.df.norm), train.index))
housing.train <- housing.df.norm[train.index, ]
housing.valid <- housing.df.norm[valid.index, ]
library(FNN)
#install.packages("FNN")
nn <- knn(train = housing.train[, c(1:11, 14:22)], test=housing.valid[, c(1:11, 14:22)], cl= housing.train$CAT..MEDV, k=1)
library(caret)
confusionMatrix(nn, as.factor(housing.valid$CAT..MEDV), positive= "1")
accuracy.df <- data.frame(k = seq(1, 20, 1), accuracy = rep(0, 20))
for (i in 1:20) {knn.pred <- knn(train = housing.train[, c(1:11, 14:22)], test = housing.valid[,c(1:11,14:22)], cl=housing.train$CAT..MEDV, k=i)
accuracy.df[i,2] <- confusionMatrix(knn.pred, as.factor(housing.valid$CAT..MEDV), positive = "1")$overall[1]
}
accuracy.df
nn2 <- knn.reg(train = housing.train[,c(1:11, 14:22)], test = housing.valid[,c(1:11,14:22)], y= housing.train$MEDV, k= 1)
nn2.results <- data.frame(cbind(pred = nn2$pred, actual = housing.valid$MEDV))
head(nn2.results, 20)
RMSE <- function(actual, pred){
  sqrt(mean((actual - pred)^2))
}

RMSE(housing.valid$MEDV, nn2$pred)
RMSE.df <-data.frame(k= seq(1, 20, 1), RMSE.k = rep(0,20)) 
for (i in 1:20) {knn.reg.pred <- knn.reg(train = housing.train[, c(1:11, 14:22)], test = housing.valid[, c(1:11, 14:22)], y= housing.train$MEDV, k=i)
RMSE.df[i, 2] <- RMSE(housing.valid$MEDV, knn.reg.pred$pred) 
  
}
RMSE.df
set.seed(5)
train.index <- sample(nrow(housing.df), nrow(housing.df) * 0.6)
valid.index <- as.numeric(setdiff(rownames(housing.df), train.index))
housing.train <- housing.df[train.index, ]
housing.valid <- housing.df[valid.index, ]
housing.df <- housing.df[, -c(12, 22)]
housing.train <- housing.train[, -c(12, 22)]
housing.valid <- housing.valid[, -c(12, 22)]

t(t(names(housing.train)))
install.packages("DiscriMiner")
library(DiscriMiner)
housing.da <- linDA(housing.df[, c(1:11, 13:20)], housing.df$CAT..MEDV, validation = "learntest", 
                    learn = train.index, 
                    test = valid.index)
housing.da$functions
propensity.high <- exp(housing.da$scores[, "1"]) / (exp(housing.da$scores[, "0"]) + exp(housing.da$scores[, "1"]))
da.results <- data.frame(Actual = housing.valid$CAT..MEDV, housing.da$classification, housing.da$scores, 
                         propensity.high = propensity.high)
options(scipen = 999)
head(da.results, 25)
confusionMatrix(housing.da$classification, 
                as.factor(housing.valid$CAT..MEDV), 
                positive = "1")
library(gains)
gain <- gains(as.numeric(housing.valid$CAT..MEDV), 
              exp(housing.da$scores[, 2]) / (exp(housing.da$scores[, 1]) + exp(housing.da$scores[, 2])), 
              groups = length(housing.valid))

# plot lift chart
plot(c(0, gain$cume.pct.of.total * sum(as.numeric(housing.valid$CAT..MEDV))) ~ c(0, gain$cume.obs),
     xlab = "# of cases", ylab = "Cumulative", main = "Lift Chart", type = "l")
lines(c(0, sum(as.numeric(housing.valid$CAT..MEDV))) ~ c(0, nrow(housing.valid)), lty = 2)
gain <- gains(as.numeric(housing.valid$CAT..MEDV), 
              exp(housing.da$scores[, 2]) / (exp(housing.da$scores[, 1]) + exp(housing.da$scores[, 2])))
heights <- gain$mean.resp / mean(as.numeric(housing.valid$CAT..MEDV))
dec.lift <- barplot(heights, names.arg = gain$depth, ylim = c(0, 6),
                    xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise Lift Chart")
housing.da.prior <- linDA(housing.df[, c(1:11, 13:20)], housing.df$CAT..MEDV, prior = c(1, 5) / 6, validation = "learntest", 
                          learn = train.index, 
                                  test = valid.index)
housing.da.prior$functions
confusionMatrix(housing.da.prior$classification, 
                as.factor(housing.valid$CAT..MEDV), 
                positive = "1")
