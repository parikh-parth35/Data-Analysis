# problem 1 - stock price problems

# reading the file from my computer
data <- read.csv("F:\\Parth Parikh\\us\\NYU\\2nd semester\\Business Analytics\\HW-1\\Stock prices HMK1.csv", header = T, stringsAsFactors = F)

# caluclating the average price of each company's shares for the given year
mean(data$AMZN) # for amazon
mean(data$KMX) # for car max
mean(data$GOOG) # for google
mean(data$GE) # for ge

# finding the data types of the variables
str(data)

# finding the returns for each company
ramzn <- (diff(data$AMZN) * -1) / tail(data$AMZN, -1)
ramzn

rkmx <- (diff(data$KMX) * -1) / tail(data$KMX, -1)
rkmx

rgoog <- (diff(data$GOOG) * -1) / tail(data$GOOG, -1)
rgoog

rge <- (diff(data$GE)* -1) / tail(data$GE, -1)
rge

# finding the cumulative returns for each company
? cumsum
cumamzn <- cumsum(ramzn)
cumamzn

cumkmx <- cumsum(rkmx)
cumkmx

cumgoog <- cumsum(rgoog)
cumgoog

cumge <- cumsum(rge)
cumge

# checking for amazon if the cumulative returns were correct or not
sum((diff(data$AMZN) * -1) / tail(data$AMZN, -1))

# sorting the returns in decreasing order
soramzn <- (ramzn[order(ramzn, decreasing = T)])
soramzn
View(soramzn)

sorkmx <- (rkmx[order(rkmx, decreasing = T)])
sorkmx

sorgoog <- (rgoog[order(rgoog, decreasing = T)])
sorgoog

sorge <- (rge[order(rge, decreasing = T)])
sorge

# selecting the top 5 and bottom 5 from the sorted values
head(soramzn, 5)
head(sorkmx, 5)
head(sorgoog, 5)
head(sorge, 5)

tail(soramzn, 5)
tail(sorkmx, 5)
tail(sorgoog, 5)
tail(sorge, 5)

# plotting the values on a graph
? plot
plot(ramzn, ylim = c(-0.04, 0.03), ylab = "returns")
plot(rkmx, ylim = c(-0.05, 0.08), ylab = "returns")
plot(rgoog, ylim = c(-0.04, 0.05), ylab = "returns")
plot(rge, ylim = c(-0.08, 0.04), ylab = "returns")
