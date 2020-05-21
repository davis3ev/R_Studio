credit.data <- read.csv("credit_default.csv")
dim(credit.data)
t(t(names(credit.data)))
mean(credit.data$default.payment.next.month)
library(dplyr)
install.packages("dplyr")
str(credit.data)
summary(credit.data)
credit.data$SEX <- as.factor(credit.data$SEX)
credit.data$EDUCATION <- as.factor(credit.data$EDUCATION)
credit.data$MARRIAGE <- as.factor(credit.data$MARRIAGE)
plot(credit.data$default.payment.next.month ~ credit.data$EDUCATION)
table.edu <- table(credit.data$EDUCATION, credit.data$default.payment.next.month)
table.edu
chisq.test(table.edu)
set.seed(1)
index <- sample(nrow(credit.data), nrow(credit.data) *.8)
credit.train <- credit.data[index, ]
credit.valid <- credit.data[-index,]
credit.glm0 <- glm(default.payment.next.month ~ ., family = "binomial", data=credit.train)
summary(credit.glm0)
credit.glm0$deviance                                
AIC(credit.glm0)
BIC(credit.glm0)
library(forecast)
pred.glm0.train <- predict(credit.glm0, type= "response")
install.packages("pROC")
library(pROC)
#ROC chart
r <- roc(credit.train$default.payment.next.month, pred.glm0.train)
plot.roc(r)
auc(r)
pred.glm.valid <- predict(credit.glm0, newdata = credit.valid, type="response")
r <- roc(credit.valid$default.payment.next.month, pred.glm.valid)
plot.roc(r)
auc(r)
install.packages("gains")
library(gains)
#
gain <- gains(credit.valid$default.payment.next.month, pred.glm.valid, group=length(pred.glm.valid))
plot(c(0, gain$cume.pct.of.total* sum(credit.valid$default.payment.next.month)) ~ c(0, gain$cume.obs), xlab="Number of Cases", ylab="Cumulative", main= "Lift Chart")
lines(c(0, sum(credit.valid$default.payment.next.month)) ~ c(0, nrow(credit.valid)), lty=2)
#deciles
gain <- gains(credit.valid$default.payment.next.month, pred.glm.valid)
heights <- gain$mean.resp/ mean(credit.valid$default.payment.next.month)
dec.lifts <- barplot(heights, names.arg = gain$depth, ylim = c(0,4), xlab = "Percentile", ylab = "Mean Response", main = "Decile-wise Lift Chart")
#install.packages("caret") binary classification
library(caret)
confusionMatrix(as.factor(ifelse(pred.glm0.train >=.5, "1","0")), as.factor(credit.train$default.payment.next.month), positive = "1")
install.packages("e1071")
library(e1071)
#Naive choice cutt off
pcut1 <- mean(credit.train$default.payment.next.month)
pcut1
#get binary prediction
class.glm0.train <- (pred.glm0.train > pcut1)*1
#confusionmatrix
confusionMatrix(as.factor(class.glm0.train), as.factor(credit.train$default.payment.next.month), positive = "1")
# define a cost function with input "obs" being the observed response,
# "pred.p" being predicted probability, and "pcut" being the threshold
constfunc <- function(obs, pred.p, pcut){weight1 <- 5 ;weight0 <- 1 ;c1 <- (obs==1)& (pred.p<pcut);c0 <- (obs==0)& (pred.p>= pcut) ;cost <- mean(weight1*c1+ weight0 * c0) ; return(cost)}
p.seq <- seq(from=.01, to=1, by=.01)
# write a loop for all potential cutoff values to see which provides the smallest cost
# first, you need to define a 0 vector in order to save the values of cost from all cutoffs
cost <- rep(0, length(p.seq))
for (i in 1:length(p.seq)) {cost[i] <- constfunc(obs=credit.train$default.payment.next.month, pred.p=pred.glm0.train, pcut = p.seq[i])}
plot(cost~p.seq)
optimal.pcut.glm0 <- p.seq[which(cost==min(cost))]
optimal.pcut.glm0
confusionMatrix(as.factor(ifelse(pred.glm0.train>= optimal.pcut.glm0, "1","0")), as.factor(credit.train$default.payment.next.month), positive = "1")
cost <- constfunc(obs=credit.train$default.payment.next.month, pred.p = pred.glm0.train, pcut=optimal.pcut.glm0)
cost
cred.glm.null <- glm(default.payment.next.month ~ 1, data = credit.train, family= "binomial")
cred.glm.fwd <- step(cred.glm.null, scope=list(cred.glm.null, uppper= credit.glm0), direction = "forward")
summary(cred.glm.fwd)
cred.glm.fwd$deviance
AIC(cred.glm.fwd)
BIC(cred.glm.fwd)
