spambase <- read.csv("spambase.csv")
dim(spambase)
str(spambase)
t(t(names(spambase)))

aggspam <- t(t(aggregate(.~ Spam, spambase,mean))) #calculate mean of variables by Spam or no spam aggspam)
#show values of aggspam, which is the set of mean values of all variables for Spam and Non Spam Emails
aggspam

#rename variables 
library(dplyr)
spambase <- rename (spambase, "re:" = re., "C:" = C., "C(" = C..1, "C[" = C..2, "C!" = C..3, "C$" = 
                      C..4, "C#" = C..5)
names(spambase)
#subset containing 15 most valuable predictors 
spambase15 <- spambase[,c(5,16,18:19,21,25:27,45:46,52,54,55:57,58)]

set.seed(1)

## partitioning spambase15 into training (60%) and validation (40%) 
# randomly sample 60% of the row IDs for training; the remaining 40% serve as
# validation
train15.rows <- sample(rownames(spambase15), nrow(spambase15)*0.6)

# collect all the columns with training row ID into training set:
train15.data <- spambase15[train15.rows, ]

# assign row IDs that are not already in the training set into validation
valid15.rows <- setdiff(rownames(spambase15), train15.rows)
valid15.data <- spambase15[valid15.rows, ]

#creates logistic model for Spam using training data
spam.glm0 <- glm(Spam~., family = "binomial", data = train15.data)
summary(spam.glm0)

#assesses fit of the model with validation data 
pred.glm0.valid <- predict(spam.glm0, newdata = valid15.data, type = "response")

#creation of confusion matrix for validation data
library(caret)

confusionMatrix(as.factor(ifelse(pred.glm0.valid >= .05, "1", "0")), as.factor(valid15.data$Spam), positive = "1")


View(spambase)
#Creation of lift chart
library(gains)
gain <- gains(valid15.data$Spam, pred.glm0.valid, groups = length(pred.glm0.valid))
#Plot lift chart
plot(c(0, gain$cume.pct.of.total * sum(valid15.data$Spam)) ~ c(0, gain$cume.obs), xlab= "Number of Emails", ylab = "Cumulative", main= "Lift Chart")
lines(c(0, sum(valid15.data$Spam))~ c(0, nrow(valid15.data)), lty= 2 )


#compute deciles and creation of Decile-wise lift chart
gain <- gains(valid15.data$Spam, pred.glm0.valid)
heights <- gain$mean.resp / mean(valid15.data$Spam)
dec.lift <- barplot(heights, names.arg = gain$depth, ylim = c(0,3), xlab = "Percentile", ylab = "Mean of Spam Emails", main = "Decile Lift Chart")


set.seed(1)
## partitioning into training (60%) and validation (40%) 
# randomly sample 60% of the row IDs for training; the remaining 40% serve as
# validation
train.rows <- sample(rownames(spambase), nrow(spambase)*0.6)
# collect all the columns with training row ID into training set:
train.data <- spambase[train.rows, ]
# assign row IDs that are not already in the training set into validation
valid.rows <- setdiff(rownames(spambase), train.rows)
valid.data <- spambase[valid.rows, ]


#Randomly split the data into training (60%) and validation (40%) datasets:
set.seed(1)
index <- sample(nrow(spambase), nrow(spambase) * 0.6)
spam.train <- spambase[index, ]
spam.valid <- spambase[-index, ]

#Train a Logistic Regression with All Predictors
spam.glm0 <- glm(Spam ~ ., family = "binomial", data = spam.train)
summary(spam.glm0)

#extracting some metrics
spam.glm0$deviance
AIC(spam.glm0)
BIC(spam.glm0)

#Out-of-Sample Prediction
pred.glm0.valid <- predict(spam.glm0, newdata = spam.valid, type = "response")
library(forecast)
pred.glm0.train=predict(spam.glm0,type="response")
library(pROC)
#ROC Curve
r <- roc(spam.valid$Spam, pred.glm0.valid)
plot.roc(r)
auc(r)

# creating lift charts and decile lift charts
library(gains)

gain <- gains(spam.valid$Spam, pred.glm0.valid, groups = length(pred.glm0.valid))

# plot lift chart
plot(c(0, gain$cume.pct.of.total * sum(spam.valid$Spam)) ~ c(0, gain$cume.obs),
     xlab = "# of cases", ylab = "Cumulative", main = "Lift Chart", type = "l")
lines(c(0, sum(spam.valid$Spam)) ~ c(0, nrow(spam.valid)), lty = 2)
# compute deciles and plot decile-wise lift chart
gain <- gains(spam.valid$Spam, pred.glm0.valid)
heights <- gain$mean.resp / mean(spam.valid$Spam)
dec.lift <- barplot(heights, names.arg = gain$depth, ylim = c(0, 4),
                    xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise Lift Chart")

library(caret)
confusionMatrix(as.factor(ifelse(pred.glm0.train >= 0.5, "1", "0")), as.factor(spam.train$Spam), 
                positive = "1")
pcut1=mean(spam.train$Spam)
pcut1
class.glm0.train=(pred.glm0.train>pcut1)*1
confusionMatrix(as.factor(class.glm0.train),as.factor(spam.train$Spam),positive="1")
#Variable Selection

# perform forward selection
spam.glm.null <- glm(Spam ~ 1, data = spam.train, family = "binomial")
spam.glm.fwd <- step(spam.glm.null, scope = list(spam.glm.null, upper = spam.glm0), direction = "forward")
summary(spam.glm.fwd)
spam.glm.fwd$deviance
AIC(spam.glm.fwd)
BIC(spam.glm.fwd)

# perform backward elimination
spam.glm.back <- step(spam.glm.null, direction = "backward")
summary(spam.glm.back)
spam.glm.back$deviance
AIC(spam.glm.back)
BIC(spam.glm.back)

# perform stepwise regression
spam.glm.step <- step(spam.glm.null, scope = list(spam.glm.null, upper = spam.glm0), direction = "both")
summary(spam.glm.step)
spam.glm.step$deviance
AIC(spam.glm.step)
BIC(spam.glm.step)

costfunc <- function(obs, pred.p, pcut){
  weight1 <- 5 # define the weight for "true=1 but pred=0" (false negative)
  weight0 <- 1 # define the weight for "true=0 but pred=1" (false positive)
  c1 <- (obs == 1) & (pred.p < pcut) # count for "true=1 but pred=0" (false negative)
  c0 <- (obs == 0) & (pred.p >= pcut) # count for "true=0 but pred=1" (false positive)
  cost <- mean(weight1 * c1 + weight0 * c0) # misclassification with weight
  return(cost) # you have to return a value when you write R functions
} # end of the function
# define a sequence from 0.01 to 1 by 0.01
p.seq <- seq(from = 0.01, to = 1, by = 0.01)
# write a loop for all potential cutoff values to see which provides the smallest cost
# first, you need to define a 0 vector in order to save the values of cost from all cutoffs
cost <- rep(0, length(p.seq))
for (i in 1:length(p.seq)) {
  cost[i] <- costfunc(obs = spam.train$Spam, pred.p = pred.glm0.train, pcut = p.seq[i])
} # end of the loop
plot(cost~p.seq)
logit.reg=glm(Spam~.,data=spam.train,family="binomial")
options(scipen=999)
#Run the logistic model and show coefficients and odds
#For example with Odds, it's saying that if everything is held constant, C..4 is 75x more likely
#to occur. Looking at C..4. 
log.reg.fit=glm(Spam~.,data=spam.train,family="binomial")
data.frame(summary(log.reg.fit)$coefficient,odds=exp(coef(log.reg.fit)))
#95% confidence level evaluation
library(gains)
pred.spam=predict(log.reg.fit,spam.valid)
gain.spam=gains(spam.valid$Spam,pred.spam,groups=100)
confusionMatrix(factor(ifelse(pred.spam>=0.5,1,0)),factor(spam.valid$Spam),positive="1")
#Getting best cutoff 
optimal.pcut.glm0=p.seq[which(cost==min(cost))]
optimal.pcut.glm0
confusionMatrix(as.factor(ifelse(pred.glm0.train>=optimal.pcut.glm0,"1","0")),as.factor(spam.train$Spam)
                ,positive="1")
cost=costfunc(obs=spam.train$Spam,pred.p=pred.glm0.train,pcut=optimal.pcut.glm0)
cost

