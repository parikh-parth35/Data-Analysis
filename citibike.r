# reading the data from github

url <- ("https://raw.githubusercontent.com/jcbonilla/BusinessAnalytics/master/BAData/JC-201709-citibike-tripdata.csv")
citi <- read.csv(url, header = T, stringsAsFactors = F)
View(citi)
str(citi)

# summary stats of trip duration
trip <- summary(citi$tripduration)
trip
install.packages("psych")
library("psych")
describe(citi$tripduration)
var(citi$tripduration) # calculating the variance of the tripduration data

#summary stats of current age of the riders

? date
?substr
year <- substr(date(), 21, 24)
year1 <- as.numeric(year)
age <- citi$birth.year
age1 <- as.numeric(age)
currage <- year1-age1
View(currage)

summary(currage)
describe(currage)
variance <- 10.05*10.05 # since the sd obtained from describe() is 10.05
variance

# summary stats of tripduration in minutes
class(citi$tripduration)
trip <- (citi$tripduration) / 60
round(trip, 2)
View(trip)

summary(trip)
describe(trip)
var(trip)

#correlation between trip duration and age
?cor
cor(currage, trip, use = "complete.obs")

#getting the rows where tripduration is > 45 minutes and < 45 mins
trip1 <- subset(trip, trip < 45)
View(trip1)
NROW(trip1)
trip2 <- subset(trip, trip >= 45)
View(trip2)
NROW(trip2)
trip3 <- subset(trip, trip >= 20)
NROW(trip3)

# finding the total revenue
revenue <- (NROW(trip1)*3)+(NROW(trip2)*5)
revenue
revenue1 <- (NROW(trip1)*4)+(NROW(trip2)*5)
revenue2 <- (NROW(trip3)*3)+((NROW(trip)-NROW(trip3))*5)

# variance of tripduration in minutes
var(trip)
sd(trip)
boxplot(trip)
boxplot(trip, ylim = c(0, 50))
describe(trip)

# to answer the question about inventory management
class(citi$bikeid)
bike <- as.factor(citi$bikeid)
View(bike)
class(bike)
bike1 <- as.data.frame(table(bike))
View(bike1)
nrow(bike1)