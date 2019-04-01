# cheese makers dataset

# reading the file in csv format
cheese <- read.csv("F:\\Parth Parikh\\us\\NYU\\2nd semester\\Business Analytics\\HW-1\\Cheesemakers_v2.csv", header = T)

str(cheese)

# summary statistics of gross profit in cheese
install.packages("psych")
library("psych")

describe(cheese$Gross.profit)
summary(cheese$Gross.profit)

# box plot and histogram of gross profits
boxplot(cheese$Gross.profit, main = "Box plot distribution of \n Gross Profits")
boxplot(cheese$Gross.profit, ylim = c(0,100), main = "Box plot distribution of \n Gross Profits")
hist(cheese$Gross.profit, xlab = "Gross Profits", main = "Frequency distribution of \n Gross Profits")
hist(cheese$Gross.profit, ylim = c(0, 97000), xlim = c(0,5000), xlab = "Gross Profits", main = "Frequency distribution of \n Gross Profits")
hist(cheese$Gross.profit, ylim = c(0, 1000), xlim = c(0,5000), xlab = "Gross Profits", main = "Frequency distribution of \n Gross Profits")

cheese[cheese$Gross.profit >= 2000, ]

# finding the recurring and non-recurring customers
freq <- data.frame(table(cheese$Customer.ID))
recurring <- cheese[cheese$Customer.ID %in% freq$Var1[freq$Freq >1],]
nonrecurring <- cheese[cheese$Customer.ID %in% freq$Var1[freq$Freq ==1],]

recurring1 <- data.frame(table(recurring$Customer.ID))
mean(recurring1$Freq)
mean(recurring$Sale.amount)

a <- mean(recurring$Gross.profit)
b <- mean(nonrecurring$Gross.profit)
c <- ((a-b)/b)*100
c

install.packages("plyr")
library("plyr")
most <- ddply(cheese, "Customer.ID", numcolwise(sum))

pmost <- most[order(most$Gross.profit, decreasing = T),]
head(pmost, 10) # top 10 most profitable clients

mean(cheese$Sale.amount)
var(cheese$Sale.amount)
std.dev <- sqrt(var(cheese$Sale.amount))
std.dev

ulimit <- mean(cheese$Sale.amount) + (2*std.dev)
ulimit

more <- cheese[cheese$Sale.amount > ulimit,]
# therefore, 1141 customers have payed more than 561.35. It means that there are only a few customers who have payed more than 651.35. If more customers would have been paying the amount more than 561.35, then the company would be making more gross profits

# number of unique clients per state
state <- data.frame(table(cheese$State))
state

# normalizing the state frequency data, sales amount, sales target, and gross profit
sf <- state$Freq
nsf <- (sf-min(sf)) / (max(sf)-min(sf))
nsf[order(nsf, decreasing = T)]

sa <- cheese$Sale.amount
nsa <- (sa - min(sa)) / (max(sa)-min(sa))
nsa[order(nsa, decreasing = T)]

st <- cheese$Sales.target
nst <- (st-min(st)) / (max(st)-min(st))
nst[order(nst, decreasing = T)]

gp <- cheese$Gross.profit
ngp <- (gp-min(gp)) / (max(gp)-min(gp))
ngp[order(ngp, decreasing = T)]

# correlation between sales and client volume
cor(cheese$Customer.ID, cheese$Sale.amount)
