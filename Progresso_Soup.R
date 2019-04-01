# reading the file from GitHub

pro <- read.csv("https://raw.githubusercontent.com/jcbonilla/BusinessAnalytics/master/BAData/Progresso_Soup.csv")
View(pro)
head(pro)
str(pro)

pro$Month <- factor(pro$Month)

monthnames <- factor(pro$Month, levels = 1:12, labels = c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"))
pro$Month <- monthnames

# creating and modifying the winter months
pro$Oct <- as.logical(0)
pro$Nov <- as.logical(0)
pro$Dec <- as.logical(0)
pro$Jan <- as.logical(0)
pro$Feb <- as.logical(0)

for (i in 1:nrow(pro)){
  if (pro$Month[i] == "Oct")
    pro$Oct[i] <- as.logical(1)
  else if (pro$Month[i] == "Nov")
    pro$Nov[i] <- as.logical(1)
  else if (pro$Month[i] == "Dec")
    pro$Dec[i] <- as.logical(1)
  else if (pro$Month[i] == "Jan")
    pro$Jan[i] <- as.logical(1)
  else if (pro$Month[i] == "Feb")
    pro$Feb[i] <- as.logical(1)
}

# sales during the winter months
pro$comb <- with(pro, ifelse(pro$Oct == T | pro$Nov == T | pro$Dec == T | pro$Jan == T | pro$Feb == T, 1, 0))

win <- subset(pro, Month == "Oct" | Month == "Nov" | Month == "Dec" | Month == "Jan" | Month == "Feb")
View(win)

install.packages("plyr")
require("plyr")

sales <- ddply(pro, "Month", numcolwise(mean))
View(sales)
plot(Sales.Progresso ~ Month, data = sales)

winsales <- ddply(win, "Month", numcolwise(sum))
View(winsales)

nonwin <- subset(pro, Month == "Mar" | Month == "Apr" | Month == "May" | Month == "Jun" | Month == "Jul" | Month == "Aug" | Month == "Sep")
View(nonwin)

nonwinsales <- ddply(nonwin, "Month", numcolwise(sum))
View(nonwinsales)

# finding the market share
share1 <- (sum(winsales$Sales.Progresso) / sum(pro$Category_Sales)) * 100
share1

share2 <- (sum(nonwinsales$Sales.Progresso) / sum(pro$Category_Sales)) * 100
share2

# developing a regression model
pro1 <- pro
View(pro1)

pro1$Jan <- as.logical(0)
pro1$Feb <- as.logical(0)
pro1$Mar <- as.logical(0)
pro1$Apr <- as.logical(0)
pro1$May <- as.logical(0)
pro1$Jun <- as.logical(0)
pro1$Jul <- as.logical(0)
pro1$Aug <- as.logical(0)
pro1$Sep <- as.logical(0)
pro1$Oct <- as.logical(0)
pro1$Nov <- as.logical(0)
pro1$Dec <- as.logical(0)

for (i in 1:nrow(pro1)){
  if (pro1$Month[i] == "Oct")
    pro1$Oct[i] <- as.logical(1)
  else if (pro1$Month[i] == "Nov")
    pro1$Nov[i] <- as.logical(1)
  else if (pro1$Month[i] == "Dec")
    pro1$Dec[i] <- as.logical(1)
  else if (pro1$Month[i] == "Jan")
    pro1$Jan[i] <- as.logical(1)
  else if (pro1$Month[i] == "Feb")
    pro1$Feb[i] <- as.logical(1)
  else if (pro1$Month[i] == "Mar")
    pro1$Mar[i] <- as.logical(1)
  else if (pro1$Month[i] == "Apr")
    pro1$Apr[i] <- as.logical(1)
  else if (pro1$Month[i] == "May")
    pro1$May[i] <- as.logical(1)
  else if (pro1$Month[i] == "Jun")
    pro1$Jun[i] <- as.logical(1)
  else if (pro1$Month[i] == "Jul")
    pro1$Jul[i] <- as.logical(1)
  else if (pro1$Month[i] == "Aug")
    pro1$Aug[i] <- as.logical(1)
  else if (pro1$Month[i] == "Sep")
    pro1$Sep[i] <- as.logical(1)
}

pro1$South <- as.logical(0)
pro1$MidWest <- as.logical(0)
pro1$East <- as.logical(0)
pro1$West <- as.logical(0)

for (i in 1:nrow(pro1)){
  if (pro1$Region[i] == "West")
    pro1$West[i] <- as.logical(1)
  else if (pro1$Region[i] == "East")
    pro1$East[i] <- as.logical(1)
  else if (pro1$Region[i] == "South")
    pro1$South[i] <- as.logical(1)
  else if (pro1$Region[i] == "MidWest")
    pro1$MidWest[i] <- as.logical(1)
}

model <- lm(Sales.Progresso ~ Jan + Feb + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec + East + West + South + Price.Campbell + Price.PL + Price.Progresso + Low_Income + High_Income, data = pro1)
summary(model)


model2 <- lm(Sales.Progresso ~ Jan + Feb + Apr + May + Jun + Jul + Aug + Sep + Oct + Nov + Dec + East + West + South , data = pro1)
summary(model2)
 
model3 <- lm(Sales.Progresso ~  Price.Campbell + Price.PL + Price.Progresso + Low_Income + High_Income, data = pro1)
summary(model3)

model4 <- lm(Sales.Progresso ~ Low_Income + High_Income, data = pro1)
summary(model4)

model5 <- lm(Sales.Progresso ~ Price.Campbell + Price.PL + Price.Progresso, data = pro1)
summary(model5)