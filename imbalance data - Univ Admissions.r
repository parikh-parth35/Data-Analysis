# reading the file

uni <- read.csv("https://raw.githubusercontent.com/jcbonilla/BusinessAnalytics/master/BAData/Univ%20Admissions.csv", header = T)

str(uni)

table(uni$x.Status.1)
prop.table(table(uni$x.Status.1))
barplot(prop.table(table(uni$x.Status.1)))
# therefore the data is highly imbalanced and the 'applicants' are in the minority section

# creating the positive and negative class
uni$app <- as.logical(0)
uni$nonapp <- as.logical(0)

# modifying the values of dummy variables

for(i in 1:nrow(uni)){
  if(uni$x.Status.1[i] == "APPLICANT")
    uni$app[i] <- as.logical(1)
  else
    uni$nonapp[i] <- as.logical(1)
}

# creating the samples for training and testing the model

install.packages("caret")
require("caret")

set.seed(100)
index <- createDataPartition(uni$app, p = 0.7, list = F)
trainingset <- uni[index,]
testset <- uni[-index,]

table(trainingset$app)
prop.table(table(trainingset$app))
barplot(prop.table(table(trainingset$app)))

# performing the under, over and both under and over sampling to overcome imbalane in the data

install.packages("ROSE")
require("ROSE")

?ovun.sample

trainingset$app <- as.integer(trainingset$app)
train.under <- ovun.sample(app~., data = trainingset, method =  "under", N = 17500)$data
prop.table(table(train.under$app))

train.over <- ovun.sample(app~., data = trainingset, method = "over", N = 297522)$data
prop.table(table(train.over$app))

train.both <- ovun.sample(app~., data = trainingset, method = "both", N = 297522)$data
prop.table(table(train.both$app))


# training the model

install.packages("rpart")
require("rpart")

names(uni)
model.imbalance <- rpart(app ~ x.Country + x.State + x.Gender + x.Source + x.GPA + x.SAT_Score + x.DistancetoCampus_miles + x.HouseholdIncome + x.InState, data = trainingset, method = "class")

model.under <- rpart(app ~ x.Country + x.State + x.Gender + x.Source + x.GPA + x.SAT_Score + x.DistancetoCampus_miles + x.HouseholdIncome + x.InState, data = train.under, method = "class")

model.over <- rpart(app ~ x.Country + x.State + x.Gender + x.Source + x.GPA + x.SAT_Score + x.DistancetoCampus_miles + x.HouseholdIncome + x.InState, data = train.over, method = "class")

model.both <- rpart(app ~ x.Country + x.State + x.Gender + x.Source + x.GPA + x.SAT_Score + x.DistancetoCampus_miles + x.HouseholdIncome + x.InState, data = train.both, method = "class")

testset$app <- as.integer(testset$app)

# predicting the values by the use of testset
predict.imbalance <- predict(model.imbalance, testset, type = "class")
predict.under <- predict(model.under, testset, type = "class")
predict.over <- predict(model.over, testset, type = "class")
predict.both <- predict(model.both, testset, type = "class")

actual <- as.factor(testset$app)
table(predict.imbalance, actual)
table(predict.under, actual)
table(predict.over, actual)
table(predict.both, actual)

install.packages("e1071")
require("e1071")
confusionMatrix(predict.imbalance, actual, positive = "1")
confusionMatrix(predict.under, actual, positive = "1")
confusionMatrix(predict.over, actual, positive = "1")
confusionMatrix(predict.both, actual, positive = "1")