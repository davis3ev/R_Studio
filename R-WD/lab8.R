install.packages("arules")
library(arules)
data("Groceries")
summary(Groceries)
x <- Groceries[size(Groceries) > 25]
inspect(x)
itemFrequencyPlot(Groceries, support= .1, cex.names = .8)
basket_rules <- apriori(Groceries, parameter = list(sup= .003, conf = .5, 

                                                      target = "rules"))
summary(basket_rules)
inspect(head(sort(basket_rules, by = "lift")))                                                   
inspect(subset(basket_rules, size(basket_rules) > 4))
yogurt.rhs <- subset(basket_rules, subset = rhs %in% "yogurt"& lift > 3)
inspect(yogurt.rhs)
meat.lhs <- subset(basket_rules, subset= lhs %in% "meat" & lift > 1.5)
inspect(meat.lhs)
#clustering 
seed <- read.table("seeds_dataset.txt")
seed <- seed[,1:7]
colnames(seed) <- c("area", "perimeter", "compactness", "length", "width", "asymmetry", "groovelength")
seed <- scale(seed)



