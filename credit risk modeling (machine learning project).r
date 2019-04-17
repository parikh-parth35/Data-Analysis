# reading the file

app <- read.csv("C:\\Users\\ppp318\\Desktop\\application_train.csv",header = T, stringsAsFactors = F)

str(app)

# seperating the character columns and numerical columns

names(app)
app <- app[-which(names(app) == "SK_ID_CURR")]
app <- app[-which(names(app) == "ORGANIZATION_TYPE")]
app$TARGET <- as.factor(app$TARGET)

target <- app$TARGET

chr <- app[, sapply(app, is.character)]
num <- app[, sapply(app, is.numeric)]

chr[is.na(chr)] <- "Not Available"
num[is.na(num)] <- 0

chrfac <- as.data.frame(apply(chr, 2, factor))

temp <- cbind(chrfac, num)
final <- cbind(temp, target)

# checking if the data set has NA values
sum(is.na(final))

install.packages("caTools")
require("caTools")

# splitting the data set to training and testing data
set.seed(101)
index <- sample.split(final$target, SplitRatio = 0.7)
train <- subset(final, index == TRUE)
test <- subset(final, index == FALSE)

# knowing the balance of target variable in the train data
table(train$target)
prop.table(table(train$target))

# so, the data is not balanced. so, using the "both" method of sampling because it divides the observations in roughly equal values i.e. 50% each
install.packages("caret")
require("caret")
install.packages("ROSE")
require("ROSE")

train.both <- ovun.sample(target ~., data = train, method= "both")$data
table(train.both$target)
prop.table(table(train.both$target))

#developing a predictive random forest model  
install.packages("randomForest")
library(randomForest)

# again splitting the data becuase it is very large
set.seed(105)
split <- sample.split(train.both$target, SplitRatio = 0.1)
train.main <- subset(train.both, split == T)
prop.table(table(train.main$target))

rf <- randomForest(target ~ ., data = train.main, importance = T, do.trace = T)
print(rf)
p <- predict(rf, test, type = "response")

install.packages("e1071")
require("e1071")
confusionMatrix(p, test$target, mode = "everything")

# tuning the random forest model
plot(rf)
t <- tuneRF(train.main[,-120], train.main[,120],
            improve = 0.05,
            trace = T,
            ntreeTry = 300,
            plot = T,
            stepFactor = 0.5) 
rf1 <- randomForest(target ~ ., data = train.main, importance = T, ntree = 300, mtry = 10,
                    do.trace = T)
print(rf1)
p1 <- predict(rf1, test, type = "response")
confusionMatrix(p1, test$target, positive = "1")

# number of trees on a node
hist(treesize(rf), main = "Number of nodes for the Tree", col = "red")

# varialbe importance of the random forest
varImp(rf)
varImpPlot(rf, type = 1)

#######################################
#trying with more number of train data
#######################################
set.seed(105)
split1 <- sample.split(train.both$target, SplitRatio = 0.3)
train.main1 <- subset(train.both, split1 == T)
prop.table(table(train.main1$target))

rf20 <- randomForest(target ~ ., data = train.main1, importance = T,
                     do.trace = T, ntree = 300)
print(rf20)
p20 <- predict(rf20, test, type = "response")
confusionMatrix(p20, test$target)


#######################################
#trying with the actual train data
#######################################

rf100 <- randomForest(target ~ ., data = train, importance = T, do.trace = T, ntree = 300)
print(rf100)
plot(rf100)
p100 <- predict(rf100, test, type = "response")
confusionMatrix(p100, test$target)

###############################
# trying the gbm model
###############################
set.seed(110)
split2 <- sample.split(train.both$target, SplitRatio = 0.01)
train.main2 <- subset(train.both, split2 == T)
prop.table(table(train.main2$target))

fitControl <- trainControl(method = "repeatedcv",
                           number = 5,
                          repeats = 3)


train.predictors <- train[,-120]
train.target <- train[,120]

install.packages("gbm")
library("gbm")


gbm <- train(train.predictors, train.target, 
             method = "gbm", trControl = fitControl)

gbm$bestTune

gbmImp <- varImp(gbm, scale = T)
gbmImp
plot(gbmImp, 20)

summary(gbm, 20)

gbm

test.predictors <- test[,-120]
test.target <- test[,120]
pgbm <- predict(gbm, test.predictors, type = "raw")

confusionMatrix(pgbm, test.target, positive = "0")

# tuning the gbm model

myGrid <- expand.grid(n.trees = c(150, 175, 200, 225), 
                      interaction.depth = c(4, 5, 6, 7, 8),
                      shrinkage = c(0.075, 0.1, 0.125, 0.15, 0.2),
                      n.minobsinnode = c(7, 10, 12, 15))

set.seed(2000)

gbm_tuned <- train(train.predictors, train.target, 
             method = "gbm", trControl = fitControl, tuneGrid = myGrid)

pgbm_tuned <- predict(gbm_tuned, test.predictors, type = "raw")
confusionMatrix(pgbm_tuned, test.target, positive = "0")

myGrid <- gbm_tuned$bestTune

gbm_tuned2 <- train(train.predictors, train.target, method = "gbm",
                    trControl = fitControl, tuneGrid = myGrid)

pgbm_tuned2 <- predict(gbm_tuned2, test.predictors, type = "raw")
confusionMatrix(pgbm_tuned2, test.target, positive = "0")

getModelInfo(gbm)



# reading the bonus files

previous <- read.csv("F:\\Parth Parikh\\us\\NYU\\2nd semester\\Business Analytics\\Project 2 - Credit Card default\\previous_application.csv")
credit <- read.csv("F:\\Parth Parikh\\us\\NYU\\2nd semester\\Business Analytics\\Project 2 - Credit Card default\\credit_card_balance.csv")
sco <- read.csv("F:\\Parth Parikh\\us\\NYU\\2nd semester\\Business Analytics\\Project 2 - Credit Card default\\new_records_for_scoring.csv")

##################################################################
# merging datasets by cleaning the original data set
##################################################################
appli <- read.csv("application_train.csv",header = T)
previous <- read.csv("previous_application.csv",header = T)
credit <- read.csv("credit_card_balance.csv",header = T)
previous <- read.csv("previous_cleaned.csv",header = T)

previous <- subset(previous,previous$AMT_APPLICATION!=0)
previous <- subset(previous,previous$NAME_CONTRACT_STATUS!="Unused offer")
previous <- subset(previous,previous$FLAG_LAST_APPL_PER_CONTRACT=="Y")
previous <- subset(previous,previous$NFLAG_LAST_APPL_IN_DAY==1)
write.csv(previous,"F:\\Parth Parikh\\us\\NYU\\2nd semester\\Business Analytics\\Project 2 - Credit Card default\\previous_cleaned.csv",row.names = F)

table(previous$NAME_CONTRACT_STATUS)
prevtimes <- as.data.frame(table(previous$SK_ID_CURR))
names(prevtimes) <- c("SK_ID_CURR","AMT_PREV_TIME")

approtime <- subset(previous,previous$NAME_CONTRACT_STATUS=="Approved")
approtable <- as.data.frame(table(approtime$SK_ID_CURR))
names(approtable) <- c("SK_ID_CURR","AMT_APPROVAL")

presum <- merge(prevtimes,approtable,by="SK_ID_CURR")

install.packages("dplyr")
library("dplyr")
previous$DAYS_BEFORE_DUE <- previous$DAYS_TERMINATION-previous$DAYS_LAST_DUE
dayspay <- select(previous,SK_ID_CURR,DAYS_LAST_DUE,DAYS_TERMINATION,DAYS_BEFORE_DUE)
dayspay <- subset(dayspay,dayspay$DAYS_LAST_DUE!=365243)
dayspay <- subset(dayspay,dayspay$DAYS_TERMINATION!=365243)

?aggregate
paybeforedue <- aggregate(dayspay$DAYS_BEFORE_DUE,by =list(dayspay$SK_ID_CURR),FUN = mean)
names(paybeforedue) <- c("SK_ID_CURR","DAYS_BEFORE_DUE")
presum <- merge(presum,paybeforedue,by="SK_ID_CURR")

table(previous$NAME_CONTRACT_TYPE)
loantype <- as.data.frame(table(previous$SK_ID_CURR,previous$NAME_CONTRACT_TYPE))

loan <- subset(loantype,loantype$Var2=="Revolving loans")
loan <- loan[-2]
names(loan) <- c("SK_ID_CURR","CNT_REVOLVING_LOAN")
presum <- merge(presum,loan,by="SK_ID_CURR")

amt <- aggregate(previous$AMT_CREDIT,by=list(previous$SK_ID_CURR),FUN = mean)
names(amt) <- c("SK_ID_CURR","AMT_CREDIT_AVG")
presum <- merge(presum,amt,by="SK_ID_CURR")

typecus <- select(previous,SK_ID_CURR,NAME_YIELD_GROUP)
rate <- as.data.frame(table(typecus$SK_ID_CURR,typecus$NAME_YIELD_GROUP))
rate1 <- subset(rate,rate$Var2=="middle")
rate1 <- rate1[-2]
names(rate1) <- c("SK_ID_CURR","AMT_MIDDLE_RATE")
presum <- merge(presum,rate1,by="SK_ID_CURR")
write.csv(presum,"F:\\Parth Parikh\\us\\NYU\\2nd semester\\Business Analytics\\Project 2 - Credit Card default\\previous_summary.csv",row.names = F)


amt <- aggregate(credit$AMT_BALANCE,by=list(credit$SK_ID_CURR),FUN = mean)
names(amt) <- c("SK_ID_CURR","AMT_BALANCE_AVG")

lim <- aggregate(credit$AMT_CREDIT_LIMIT_ACTUAL,by=list(credit$SK_ID_CURR),FUN = mean)
names(lim) <- c("SK_ID_CURR","AMT_LIMIT_AVG")
cresum <- merge(amt,lim,by="SK_ID_CURR")

paycurr <- aggregate(credit$AMT_INST_MIN_REGULARITY,by=list(credit$SK_ID_CURR),FUN = mean)
names(paycurr) <- c("SK_ID_CURR","AMT_INST_MIN_AVG")
cresum <- merge(cresum,paycurr,by="SK_ID_CURR")

cnt <- as.data.frame(table(credit$SK_ID_CURR))
names(cnt) <- c("SK_ID_CURR","CNT_CREDIT_RECORDS")
cresum <- merge(cresum,inscnt,by="SK_ID_CURR")

inscnt <- aggregate(credit$CNT_INSTALMENT_MATURE_CUM,by=list(credit$SK_ID_CURR),FUN = max)
names(inscnt) <- c("SK_ID_CURR","CNT_INSTALL")
dpd <- aggregate(credit$SK_DPD_DEF,by=list(credit$SK_ID_CURR),FUN = max)
names(dpd) <- c("SK_ID_CURR","DPD_DEF")
cresum <- merge(cresum,dpd,by="SK_ID_CURR")

write.csv(cresum,"F:\\Parth Parikh\\us\\NYU\\2nd semester\\Business Analytics\\Project 2 - Credit Card default\\credit_summary.csv",row.names = F)


app <- read.csv("p2data.csv",header = T)
credit <- read.csv("credit_summary.csv",header = T)
previous <- read.csv("previous_summary.csv",header = T)
score <- read.csv('extradata.csv',header = T)

score <- score[-1]
score <- cbind(sco$SK_ID_CURR,score)

appnew <- merge(app,previous,by="SK_ID_CURR",all.x = T)
appnew <- merge(appnew,credit,by="SK_ID_CURR",all.x = T)
scorenew <- merge(score,previous,by="SK_ID_CURR",all.x = T)
scorenew <- merge(scorenew,credit,by="SK_ID_CURR",all.x = T)
app <- cbind(appl$SK_ID_CURR,app)
name <- names(score)
name[1] <- "SK_ID_CURR"
names(score) <- name

write.csv(appnew,"F:\\Parth Parikh\\us\\NYU\\2nd semester\\Business Analytics\\Project 2 - Credit Card default\\bonus_application_original.csv",row.names =F)
write.csv(scorenew,"F:\\Parth Parikh\\us\\NYU\\2nd semester\\Business Analytics\\Project 2 - Credit Card default\\bonus_scoring_original.csv",row.names =F)

##################
#Scoring Part
##################

rf200 <- randomForest(TARGET ~ ., data = scorenew, importance = T, do.trace = T,
                      ntree = 300)
print(rf200)
plot(rf200)
p200 <- predict(rf200, soc, type = "response")
confusionMatrix(p200, soc$target, positive = "1", mode = "everything")