install.packages("neuralnet")
install.packages("nnet")
library(neuralnet)
library(nnet)
library(caret)
accidents.df <- read.csv("accidentsnn.csv")
summary(accidents.df)
accidents.df$ALCHL_I <- ifelse(accidents.df$ALCHL_I == 1,1,0)
summary(accidents.df$ALCHL_I)
accidents.df$SUR_COND <- as.factor(accidents.df$SUR_COND)
dummies <- as.data.frame(model.matrix(~ 0 + SUR_COND, data = accidents.df))
t(t(names(dummies)))
t(t(names(accidents.df)))
accidents.df <- cbind(accidents.df[,-3], dummies[,-5])
t(t(names(accidents.df)))
accidents.df$VEH_INVL <- (accidents.df$VEH_INVL - min(accidents.df$VEH_INVL))/
  (max(accidents.df$VEH_INVL) - min(accidents.df$VEH_INVL))
summary(accidents.df$VEH_INVL)
set.seed(2)
train.index <- sample(nrow(accidents.df), nrow(accidents.df) * .6)
valid.index <- setdiff(row.names(accidents.df), train.index)

#when y has multiple classes, need to dummify
acc.train <- cbind(accidents.df[train.index, c(1:3, 5:8)], 
                   class.ind(accidents.df[train.index, ]$MAX_SEV_IR))
names.acc.train <- c(names(acc.train[ , c(1:7)]), 
                     paste("MAX_SEV_IR_", c(0,1,2), sep= ""))
names(acc.train) <- c(names(acc.train[, c(1:7)]),
                      paste("MAX_SEV_IR_", c(0, 1, 2), sep = ""))




acc.valid <-  cbind(accidents.df[-train.index, c(1:3, 5:8)],
                    class.ind(accidents.df[-train.index, ]$MAX_SEV_IR))
names(acc.valid) <- c(names(acc.valid[, c(1:7)]), 
                           paste("MAX_SEV_IR_", c(0,1,2),sep= ""))
summary(acc.train)                     

nn <- neuralnet(MAX_SEV_IR_0 + MAX_SEV_IR_1 +MAX_SEV_IR_2~ 
                  ALCHL_I + PROFIL_I_R +VEH_INVL + SUR_COND1 + SUR_COND2 + 
                  SUR_COND3 +SUR_COND4, data = acc.train, hidden = 2)                    
plot(nn, rep = "best")


training.prediction <- compute(nn, acc.train[, -c(8:10)])
training.class <- apply(training.prediction$net.result, 1, which.max)- 1
confusionMatrix(as.factor(training.class), as.factor(accidents.df[train.index,]$MAX_SEV_IR))

validation.prediction <-  compute(nn, acc.valid[, -c(8:10)])
validation.class <-  apply(validation.prediction$net.result,1,which.max) -1
confusionMatrix(as.factor(validation.class), as.factor(accidents.df[valid.index,]$MAX_SEV_IR))

nn.2.2 <- neuralnet(MAX_SEV_IR_0 + MAX_SEV_IR_1 + MAX_SEV_IR_2 ~ ALCHL_I + 
                      PROFIL_I_R + VEH_INVL + SUR_COND1 + SUR_COND2 + SUR_COND3 
                    +SUR_COND4, data = acc.train, hidden = c(4,4), stepmax = 1e6)
plot(nn.2.2, rep = "best")                      
training.prediction2.2 <- compute(nn.2.2, acc.train[ , -c(8:10)])
training.class2.2 <- apply(training.prediction2.2$net.result,1,which.max)-1
confusionMatrix(as.factor(training.class2.2), as.factor(accidents.df[train.index, ]$MAX_SEV_IR))


toyota <- read.csv("ToyotaCorolla.csv")
