# reading the file grom github

url <- ("https://raw.githubusercontent.com/jcbonilla/BusinessAnalytics/master/BAData/zagat.CSV")
zagat <- read.csv(url, header = T, stringsAsFactors = F)
View(zagat)

# calculating the central tendencies and spread and dispersion of food, decor, service, and price
food <- zagat$Food
decor <- zagat$Decor
service <- zagat$Service
price <- zagat$Price

install.packages("psych")
library("psych")

describe(food)
describe(decor)
describe(service)
describe(price)

food1 <- as.data.frame(table(food))
max(food1$Freq)

decor1 <- as.data.frame(table(decor))
max(decor1$Freq)

service1 <- as.data.frame(table(service))
max(service1$Freq)

price1 <- as.data.frame(table(price))
max(price1$Freq)

summary(food)
summary(decor)
summary(service)
summary(price)

# calculating the coefficient of variation for the ratings
(sd(food)/mean(food))*100
(sd(decor)/mean(decor))*100
(sd(service)/mean(service))*100
(sd(price)/mean(price))*100

#calculating the variance of the ratings
var(food)
var(decor)
var(service)
var(price)

# finding the correlation between the ratings
install.packages("Hmisc")
library("Hmisc")
?rcorr

zagat1 <- zagat
zagat1$Name = NULL
View(zagat1)
rcorr(as.matrix(zagat1))
cor(food, decor)
cor(food, price)