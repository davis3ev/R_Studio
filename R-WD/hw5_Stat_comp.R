faa <- read.csv("FAA1.csv")
dim(faa)
str(faa)
summary(faa)

aggfaa <- t(t(aggregate(. ~ aircraft, faa, mean)))
aggfaa

library(gplots)

colfunc <- colorRampPalette(c("green", "white", "red"))
heatmap.2(cor(Filter(is.numeric, faa), use = "complete.obs"), Rowv = FALSE, Colv = FALSE,
          dendrogram = "none", lwid=c(0.1,4), lhei=c(0.1,4), col = colfunc(15),
          cellnote = round(cor(Filter(is.numeric, faa), use = "complete.obs"),2),
          notecol = "black", key = FALSE, trace = 'none', margins = c(15,15))

plot(faa$distance ~ faa$speed_ground, xlab = "Speed_ground", ylab= "Distance")

boxplot(faa$distance ~ faa$aircraft, xlab= "Aircraft", ylab = "Distance")

faa <- faa[faa$distance <6000, ]
# recreate boxplot without the outlier
boxplot(faa$distance ~ faa$aircraft, color = "blue", xlab = "Aircraft", 
        ylab = "Distance", main = "Distribution of Distance by Aircraft")

t(t(names(faa)))

# create new data frame containing only the variables to be used for analysis
faasub <- faa[, c(8,4)]
head(faasub)


# partitioning the data
# use set.seed() to get the same partitions when re-running the R code
set.seed(1)

## partitioning into training (60%) and validation (40%) 
# randomly sample 60% of the row IDs for training; the remaining 40% serve as
# validation
train.rows <- sample(rownames(faasub), nrow(faasub)*0.6)
# collect all the columns with training row ID into training set:
train.data <- faasub[train.rows, ]
# assign row IDs that are not already in the training set into validation
valid.rows <- setdiff(rownames(faasub), train.rows)
valid.data <- faasub[valid.rows, ]

head(train.data)
head(valid.data)

# use lm() to run a linear regression of Price on all 10 predictors in the
# training set
# use . after ~ to include all the remaining columns in train.df as predictors.
faa.sub.lm <- lm(distance ~ ., data = train.data)
# use options() to ensure numbers are not displayed in scientific notation
options(scipen = 999)
summary(faa.sub.lm)

library(forecast)
# use predict() to make predictions on a new set
valid.lm.pred <- predict(faa.sub.lm, valid.data)
options(scipen = 999, digits = 1)
# calculate the residuals
valid.resid <- valid.data$distance - valid.lm.pred
# look at the first 20 residuals
data.frame("Predicted" = valid.lm.pred[1:20], "Actual" = valid.data$distance[1:20],
           "Residual" = valid.resid[1:20])

# use accuracy() to compute common accuracy measures
accuracy(valid.lm.pred, valid.data$distance)

# view the distribution of residuals
library(tidyverse)
ggplot(data = as.data.frame(valid.resid), aes(y = valid.resid)) + geom_boxplot()

faa.sub.airbus <- subset(faa, aircraft=="airbus")
faa.sub.airbus

faa.sub.boeing <- subset(faa, aircraft =="boeing")
faa.sub.boeing
str(faa.sub.boeing)

set.seed(1)

## partitioning into training (60%) and validation (40%) 
# randomly sample 60% of the row IDs for training; the remaining 40% serve as
# validation
train.airbus.rows <- sample(rownames(faa.sub.airbus), nrow(faa.sub.airbus)*0.6)
# collect all the columns with training row ID into training set:
train.data.airbus <- faasub[train.rows, ]
# assign row IDs that are not already in the training set into validation
valid.rows <- setdiff(rownames(faa.sub.airbus), train.airbus.rows)
valid.data <- faa.sub.airbus[valid.rows, ]

head(train.data.airbus)
head(valid.data)

# use lm() to run a linear regression of Price on all 10 predictors in the
# training set
# use . after ~ to include all the remaining columns in train.df as predictors.
faa.airbus.lm <- lm(distance ~ ., data = train.data.airbus)
# use options() to ensure numbers are not displayed in scientific notation
options(scipen = 999)
summary(faa.airbus.lm)

set.seed(1)

## partitioning into training (60%) and validation (40%) 
# randomly sample 60% of the row IDs for training; the remaining 40% serve as
# validation
train.boeing.rows <- sample(rownames(faa.sub.boeing), nrow(faa.sub.boeing)*.6)
# collect all the columns with training row ID into training set:
train.boeing.rows
train.data.boeing1 <- faa.sub.boeing[train.boeing.rows, ]
# assign row IDs that are not already in the training set into validation
valid.rows <- setdiff(rownames(faa.sub.boeing), train.boeing.rows)
valid.data.boeing <- faa.sub.boeing[valid.rows, ]

head(train.data.boeing)
head(valid.data.boeing)

# use lm() to run a linear regression of Price on all 10 predictors in the
# training set
# use . after ~ to include all the remaining columns in train.df as predictors.
faa.boeing.lm <- lm(distance ~ speed_ground, data = train.data.boeing1)
# use options() to ensure numbers are not displayed in scientific notation
options(scipen = 999)
summary(faa.boeing.lm)

faa.boeing.lm


