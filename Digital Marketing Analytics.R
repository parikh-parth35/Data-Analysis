# reading the file

users <- read.csv("F:\\Parth Parikh\\us\\NYU\\2nd semester\\Business Analytics\\Project 1 - Email Engagement\\CRT Users.csv", na.strings = c("", "NA"))
str(users)
View(users)

email <- read.csv("F:\\Parth Parikh\\us\\NYU\\2nd semester\\Business Analytics\\Project 1 - Email Engagement\\CRT emails.csv")
str(email)
View(email)

# getting the details of the null element id from users

users1 <- users[na.exclude(users$Element.Id),]
View(users1)

?na.omit
?merge
? is.na

library("plyr")
email1 <- ddply(email, "Element.Id", numcolwise(sum))
email1 <- email1[2:62047,]
View(email1)

final <- merge.data.frame(users, email1, by.x = "Element.Id", by.y = "Element.Id")
View(final)

# finding the number of emails taken to open 1 email
trial <- email[, c(2, 3, 16, 34)]
View(trial)

trial1 <- subset(trial, Action == "emailDelivered" | Action == "emailOpened")
View(trial1)
class(trial1$Timestamp)

# installing the lubridate package
require("lubridate")

class(trial1$Timestamp)
trial1$Timestamp <- mdy_hms(trial1$Timestamp)
class(trial1$Timestamp)
View(trial1)

# sorting trial1 in asscending order of date
trial2 <- trial1[order(trial1$Timestamp),]
View(trial2)

# bifurcating opened and delivered data
biopen <- subset(trial2, Action == "emailOpened")
View(biopen)
biopen1 <- biopen[unique(biopen$Element.Id),]
View(biopen1)
biopen2 <- biopen1[order(biopen1$Timestamp),]
View(biopen2)

bideli <- subset(trial2, Action == "emailDelivered")
View(bideli)
bideli1 <- bideli[order(bideli$Timestamp),]
View(bideli1)

trial3 <- trial2[trial2$Element.Id %in% biopen2$Element.Id[trial2$Timestamp <= biopen2$Timestamp],]
View(trial3)

trial4 <- subset(trial3, Action == "emailDelivered")
View(trial4)

trial5 <- trial4[trial4$Element.Id %in% users1$Element.Id,]
View(trial5)

trial6 <- ddply(trial5, "Element.Id", numcolwise(sum))
View(trial6)
mean(trial6$Number.of.Records)
