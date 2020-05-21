toyota <- read.csv("ToyotaCorolla.csv")
dim(toyota)
names(toyota)
str(toyota)
summary(toyota)
toyota <- toyota[toyota$CC !=16000, -c(1:2, 6, 15)]
version
install.packages("fastDummies")
install.packages("devtools")
install.packages("fastDummies")
library(fastDummies)
set.seed(1)
train.rows <- sample(nrow(toyota), nrow(toyota) * .8)
toyota.train <- toyota[train.rows, ]
toyota.valid <- toyota[-train.rows, ]
plot(Price ~ Age_08_04, data= toyota.train)
mode10 <- lm(Price~ Age_08_04, data=toyota.train)
abline(mode10, col="red", lty=2, lwd= 2)
sum.mode10 <- summary(mode10)
sum.mode10
confint(mode10)
pred.mode10 <- predict(mode10, newdata = data.frame(toyota.valid))
pred.mode10
predict(mode10, newdata = data.frame(Age_08_04 = c( 10,20,30)), interval = "confidence")
model1 <- lm(Price~ ., data=toyota.train)
summary(model1)
model2  <- lm(Price ~ .-Power_Steering - Airbag_1 - Airbag_2 - Mistlamps, data=toyota.train)
summary(model2)
sum.model1 <- summary(model1)
#MSE (sum.model1$sigma)^2
sum.model1$r.squared
sum.model1$adj.r.squared
AIC(model1)
BIC(model1)
library(forecast)
install.packages("forecast")
library(forecast)
pred.mode10 <- predict(mode10, newdata= toyota.valid)
pred.model1 <- predict(model1, newdata = toyota.valid)
pred.model2 <- predict(model2, newdata=toyota.valid)
accuracy(pred.mode10, toyota.valid$Price)
accuracy(pred.model1, toyota.valid$Price)
accuracy(pred.model2, toyota.valid$Price)
summary(search)
