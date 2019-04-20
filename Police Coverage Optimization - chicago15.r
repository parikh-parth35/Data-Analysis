# reading the csv file

chicago15 <- read.csv("F:\\Parth Parikh\\us\\NYU\\Excel and R practice\\Crimes in Chicago\\chicago15.csv", header = T)
View(chicago15)
str(chicago15)

chicago15$zipcode <- factor(chicago15$zipcode)

# converting the time to night, morning, evening

chicago15$Date <- as.character(chicago15$Date)

install.packages("dplyr")
require("dplyr")

chicago15 <- chicago15 %>%
  dplyr::mutate(Date2 = dplyr::case_when(
    grepl('-', Date) ~ as.POSIXct(Date, format = '%m-%d-%y %H:%M'),
    TRUE ~ as.POSIXct(Date, format = '%m/%d/%Y %I:%M:%S %p')
  )) %>%
  dplyr::mutate(Time_of_Day = dplyr::case_when(
    as.numeric(format(Date2, '%H')) >= 21 ~ 'night',
    as.numeric(format(Date2, '%H')) >= 12 ~ 'evening',
    as.numeric(format(Date2, '%H')) >= 4 ~ 'morning',
    TRUE ~ 'night'
  ))
View(chicago15)

# creating dummy variables of time_of_day

chicago15$night <- as.logical(0)
chicago15$morning <- as.logical(0)
chicago15$evening <- as.logical(0)

for (i in 1:nrow(chicago15)) {
  if(chicago15$Time_of_Day[i] == 'night')
    chicago15$night[i] <- as.logical(1)
  else if (chicago15$Time_of_Day[i] == 'morning')
    chicago15$morning[i] <- as.logical(1)
  else if (chicago15$Time_of_Day[i] == 'evening')
    chicago15$evening[i] <- as.logical(1)
}


# finding the columns which have na values
t <- "NULL"
p <- "NULL"
for(i in 1:ncol(chicago15)){
  if(sum(is.na(chicago15[i])) >= 1)
    t <- c(t,names(chicago15[i]))
  else
    p <- c(p,names(chicago15[i]))
}

names(chicago15[5])
t
p
t <- t[-1]
p <- p[-1]

# data cleaning

chicago15[is.na(chicago15)] <- "Not Available"

final <- chicago15
final$Beat <- as.factor(final$Beat)
final$District <- as.factor(final$District)
final$Ward <- as.factor(final$Ward)
final$Community.Area <- as.factor(final$Community.Area)
final$Time_of_Day <- as.factor(final$Time_of_Day)

final$Arrest <- as.numeric(final$Arrest)
final$Domestic <- as.numeric(final$Domestic)

final$Arrest <- as.factor(final$Arrest)
final$Domestic <- as.factor(final$Domestic)

str(final)

# useful data set is "ultimate"
ultimate <- final[,6:16]
ultimate <- cbind(ultimate, final[, 24], final[, 26])

colnames(ultimate)[12] <- "Zipcode"
colnames(ultimate)[13] <- "Time_of_Day"

str(ultimate)
ultimate$Zipcode <- as.factor(ultimate$Zipcode)

prop.table(table(ultimate$Arrest))

library("caret")

fitControl <- trainControl(method = "none")
set.seed(165)
split <- (0.7)
index <- sample(1:nrow(chicago15), split*nrow(chicago15))
train<- ultimate[index,]
test <- ultimate[-index,]

names(getModelInfo())


model20 <- train(Arrest ~ Primary.Type + Description + Zipcode, data = train,
                 method = "glm", family = "binomial", trControl = fitControl)
model20

prediction <- predict(model20, test, type = "raw") 
actual_predictions <- data.frame(cbind(actual = test$Arrest, 
                                       predicted = prediction))
head(actual_predictions, 50)

# finding out the accuracy
correlation <- cor(actual_predictions$actual, actual_predictions$predicted)
correlation

model100 <- train(Arrest ~ ., data = train, method = "glm",
                  family = "binomial", trControl = fitControl)
model100

p <- predict(model100, test, type = "raw") 
actual_p <- data.frame(cbind(actual = test$Arrest, 
                                       predicted = p))
head(actual_p, 50)

# finding out the accuracy
correlation <- cor(actual_p$actual, actual_p$predicted)
correlation
