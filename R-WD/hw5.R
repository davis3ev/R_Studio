spambase <- read.csv("spambase.csv")
View(spambase)
t(t(names(spambase  )))
library("dplyr")
spam.df <- spambase
spam.df <- rename(spam.df, "re:" = re., "C;"=C., "C("= C..1, "C["= C..2, "C!" = C..3,
                  "C$"= C..4, "C#" =C..5)

names(spam.df)

#Partition Data
set.seed(5)
index <- sample(nrow(spam.df), nrow(spam.df)*.6)
spam.train <- spam.df[index,]
spam.valid <- spam.df[-index,]  

#Fitting Classification Tree
library(rpart)
library(rpart.plot)
spam.ct0 <- rpart(Spam ~ ., data = spam.train, method = "class", cp= .0001)
prp(spam.ct0, type = 1, extra = 1, varlen = -10,
    box.col = ifelse(spam.ct0$frame$var == "<leaf>", "gray", "white"))
#Pruning 
plotcp(spam.ct0)
printcp(spam.ct0)
spam.ct0.pruned <- prune(spam.ct0, cp= .00557103)
spam.ct0.pruned
prp(spam.ct0.pruned, digits = 4, type = 1, extra = 1, varlen = -10,
    box.col = ifelse(spam.ct0.pruned$frame$var == "<leaf>", "gray", "white"))

#ConfusionMatrix for classification tree 
library(caret)
spam.ct0.pruned.pred <- predict(spam.ct0.pruned, spam.valid, type = "class")
confusionMatrix(spam.ct0.pruned.pred, as.factor(spam.valid$Spam), positive = "1")
