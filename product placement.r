install.packages("arules")
require("arules")
install.packages("arulesViz")
require("arulesViz")

groc <- read.transactions("https://raw.githubusercontent.com/jcbonilla/BusinessAnalytics/master/BAData/groceries-vertical.csv", sep = ",")
summary(groc)

inspect(groc[1:4])
itemFrequency(groc[,100:110])
itemFrequencyPlot(groc, support = 0.10)
itemFrequencyPlot(groc, topN = 10, ylim = c(0, 0.30), main = "Relative appearance of top 10 items \n in the shopping cart", xlab = "Items", ylab = "Relative frequency of items")

m1 <- apriori(groc)
m1

m2 <- apriori(groc, parameter = list(support = 0.007, confidence = 0.25, minlen = 2))
m2
summary(m2)

inspect(m2[1:4])
inspect(sort(m2, by = "lift")[1:5])
inspect(sort(m2, by = "lift")[6:10])
inspect(sort(m2, by = "lift")[1:15])

m3<- apriori(groc, parameter = list(support = 0.007, confidence = 0.25, minlen = 4))
m3
inspect(sort(m3, by = "lift")[1:4])