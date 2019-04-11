# reading the file

car <- read.csv("https://raw.githubusercontent.com/jcbonilla/BusinessAnalytics/master/BAData/car-data.csv", stringsAsFactors = T)
View(car)
str(car)

#Creating a split to form training and testing data sets
set.seed(100)
split <- (0.8)

index <- sample(1:nrow(car),(split)*nrow(car))
train <- car[index, ]
test <- car[-index, ]

car$Doors <- as.factor(car$Doors)
car$Persons <- as.factor(car$Persons)
car$Boot.Space <- as.factor(car$Boot.Space)
car$Safety <- as.factor(car$Safety)

#Developing a model
library(rpart.plot)
model <- rpart(Car.Acceptability ~., data <- train, method = "class" )
rpart.plot(model, roundint = F)
printcp(model)

# Runnig the model on test data
prediction <- predict(model, test, type ="class")

car.table <- data.frame(Predicted = prediction, actual = test$Car.Acceptability)
table(car.table)

p.prob <- predict(model, test, type = "prob")
library(pROC)
plot(roc(test$Car.Acceptability, p.prob[,2]))
auc(test$Car.Acceptability, p.prob[,2])

# forming a confusion matrix
actual <- as.factor(test$Car.Acceptability)
require(caret)

confusionMatrix(prediction, actual, positive = "good")

recall <- sensitivity(prediction, actual)
recall
precision <- posPredValue(prediction, actual)
precision
F.score <- 2*((recall*precision)/(recall+precision))
F.score
G.score <- sqrt(recall*precision)
G.score