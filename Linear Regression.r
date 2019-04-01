# reading the problem 1 file

data <- read.csv("F:\\Parth Parikh\\us\\NYU\\2nd semester\\Business Analytics\\HW - 4 Linear Modeling\\problem 1 bicycling world.csv")
View(data)

# scatter plot
plot(data$Price.... ~ data$Weight..lbs., xlab = "Weight", ylab = "Price", main = "Distribution of price with weight")

# linear regression
mod <- lm(data$Price.... ~ data$Weight..lbs.)
summary(mod)
abline(mod, col = "red", lwd = 2)

# reading the problem 2 file

assem <- read.csv("F:\\Parth Parikh\\us\\NYU\\2nd semester\\Business Analytics\\HW - 4 Linear Modeling\\problem 2 assembly line.csv")

# scatter plot
plot(assem$Number.of.defectve.parts.found ~ assem$Line.speed..ft.min., xlab = "Line Speed", ylab = "Number of defective parts", main = "Distribution of defective parts \n with line speed")

# regression model
mod1 <- lm(assem$Number.of.defectve.parts.found ~ assem$Line.speed..ft.min.)
summary(mod1)
abline(mod1, col = "blue")

# reading the problem 3 file

auto <- read.csv("F:\\Parth Parikh\\us\\NYU\\2nd semester\\Business Analytics\\HW - 4 Linear Modeling\\problem 3 auto problem.csv")
View(auto)

#scatter plot
plot(auto$Annual.Maintenance.Expense...100s. ~ auto$Weekly.usage..hours., xlab = "Weekly usage in hours", ylab = "Annual maintenance expense", main = "Distribution of expense based \n on usage")

# regression model
mod3 <- lm(auto$Annual.Maintenance.Expense...100s. ~ auto$Weekly.usage..hours.)
summary(mod3)
abline(mod3, col = "green")

# reading the problem 4 file

toyota <- read.csv("F:\\Parth Parikh\\us\\NYU\\2nd semester\\Business Analytics\\HW - 4 Linear Modeling\\problem 4 toyota problem.csv")
View(toyota)

plot(toyota$Price...1000s. ~ toyota$Miles..1000s., xlab = "Miles (in 1000s)", ylab = "Price (in 1000s)", main = "Distribution of Price with miles")

# regression model
mod4 <- lm(toyota$Price...1000s. ~ toyota$Miles..1000s.)
summary(mod4)
abline(mod4, col = "purple")

toyota$predicted.price <- 16.46976 - (0.05877 * toyota$Miles..1000s.)
toyota$residuals <- toyota$Price...1000s.-toyota$predicted.price
toyota

new_price <- 16.46976 - (0.05877 * 60)
new_price

# reading the problem 5 file

dod <- read.csv("https://raw.githubusercontent.com/jcbonilla/BusinessAnalytics/master/BAData/dodgers.csv", header = T)
View(dod)

#counting the cap promotions
cap <- data.frame(table(dod$cap))
cap

#counting the cap promotions
shirt <- data.frame(table(dod$shirt))
shirt

#counting the cap promotions
fire <- data.frame(table(dod$fireworks))
fire

#counting the bobblehead promotions
bobb <- data.frame(table(dod$bobblehead))
bobb

dod1 <- dod
dod1$promotions <- with(dod1, ifelse(cap == "YES" | shirt == "YES" | fireworks == "YES" | bobblehead == "YES", "YES", "NO"))
View(dod1)

dod2 <- data.frame(dod1$attend, dod1$promotions)
View(dod2)
names(dod2) <- c("attend", "promotions")

install.packages("plyr")
require("plyr")

dod3 <- ddply(dod2, "promotions", numcolwise(mean))
dod3$attend <- round(dod3$attend)
dod3

dod4 <- data.frame(dod1$attend, dod1$month, dod1$day_of_week, dod1$skies, dod1$day_night, dod1$opponent)
View(dod4)
names(dod4) <- c("attend", "month", "day_of_week", "skies", "day_night", "opponent")

month <- ddply(dod4, "month", numcolwise(mean))
month$attend <- round(month$attend)
month

day <- ddply(dod4, "day_of_week", numcolwise(mean))
day$attend <- round(day$attend)
day

skies <- ddply(dod4, "skies", numcolwise(mean))
skies$attend <- round(skies$attend)
skies

day_night <- ddply(dod4, "day_night", numcolwise(mean))
day_night$attend <- round(day_night$attend)
day_night

att_mean <- mean(dod1$attend)
round(att_mean)

opp <- ddply(dod4, "opponent", numcolwise(mean))
opp$attend <- round(opp$attend)
opp

opp$result <- opp$attend > att_mean
opp

# regression model
dod5 <- dod1

mod7 <- lm(attend ~ bobblehead, data = dod5)
summary(mod7)

mod8 <- lm(attend ~ cap + bobblehead + fireworks + shirt, data = dod5)
summary(mod8)

mod9 <- lm(attend ~ shirt, data = dod5)
summary(mod9)

mod10 <- lm(attend ~ fireworks, data = dod5)
summary(mod10)

mod11 <- lm(attend ~ cap, data = dod5)
summary(mod11)

mod12 <- lm(attend ~ cap + fireworks, data = dod5)
summary(mod12)

mod13 <- lm(attend ~ cap + shirt, data = dod5)
summary(mod13)

mod14 <- lm(attend ~ shirt + fireworks, data = dod5)
summary(mod14)

month1 <- month[c(2, 3, 4),]
month1
round(mean(month1$attend))