# reading the file

bank <- read.csv("https://raw.githubusercontent.com/jcbonilla/BusinessAnalytics/master/BAData/bank_marketing.csv")

str(bank)

# creating training and testing data
set.seed(104)
split <- 0.8

index <- sample(1:nrow(bank), split*nrow(bank))
train <- bank[index,]
test <- bank[-index,]

# creating a model
ytree <- rpart(y ~ ., data = train, method = "class")
ytree
?printcp
printcp(ytree)
rpart.plot(ytree)

# predicting from the test data set
prediction <- predict(ytree, test, type = "class")
prediction

conf.table <- data.frame(predicted = prediction, actual = test$y)
conf.table1 <- table(conf.table)
conf.table1

# calculating the accuracy
accuracy <- sum(diag(conf.table1)) / sum(conf.table1)
accuracy

# finding the misclassification error
mean(test$y != prediction)
summary(ytree)


# plotting the sensitivity vs specificity graph using the pROC library
prediction.prob <- predict(ytree, test, type = "prob")
prediction.prob

plot(roc(test$y, prediction.prob[,2]))
auc(test$y, prediction.prob[,2])

actual <- test$y
confusionMatrix(prediction, actual, positive = "yes")