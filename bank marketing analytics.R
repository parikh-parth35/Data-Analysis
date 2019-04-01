# reading the file from GitHub

bank <- read.csv("https://raw.githubusercontent.com/jcbonilla/BusinessAnalytics/master/BAData/bank_marketing.csv")
str(bank)
View(bank)

# creating dummy variables

bank$housemaid <- as.logical(0)
bank$services <- as.logical(0)
bank$admin. <- as.logical(0)
bank$blue_collar <- as.logical(0)
bank$technician <- as.logical(0)
bank$retired <- as.logical(0)
bank$management <- as.logical(0)
bank$self_employed <- as.logical(0)
bank$unknown <- as.logical(0)
bank$entrepreneur <- as.logical(0)
bank$unemployed <- as.logical(0)
bank$student <- as.logical(0)

for (i in 1:nrow(bank)){
  if (bank$job[i] == "housemaid")
    bank$housemaid[i] <- as.logical(1)
  else if (bank$job[i] == "services")
    bank$services[i] <- as.logical(1)
  else if (bank$job[i] == "admin.")
    bank$admin.[i] <- as.logical(1)
  else if (bank$job[i] == "blue-collar")
    bank$blue_collar[i] <- as.logical(1)
  else if (bank$job[i] == "technician")
    bank$technician[i] <- as.logical(1)
  else if (bank$job[i] == "retired")
    bank$retired[i] <- as.logical(1)
  else if (bank$job[i] == "management")
    bank$management[i] <- as.logical(1)
  else if (bank$job[i] == "self-employed")
    bank$self_employed[i] <- as.logical(1)
  else if (bank$job[i] == "unknown")
    bank$unknown[i] <- as.logical(1)
  else if (bank$job[i] == "entrepreneur")
    bank$entrepreneur[i] <- as.logical(1)
  else if (bank$job[i] == "unemployed")
    bank$unemployed[i] <- as.logical(1)
  else if (bank$job[i] == "student")
    bank$student[i] <- as.logical(1)
}

bank$married <- as.logical(0)
bank$single <- as.logical(0)
bank$divorced <- as.logical(0)
bank$unknown <- as.logical(0)

for (i in 1:nrow(bank)){
  if (bank$marital[i] == "married")
    bank$married[i] <- as.logical(1)
  else if (bank$marital[i] == "divorced")
    bank$divorced[i] <- as.logical(1)
  else if (bank$marital[i] == "single")
    bank$single[i] <- as.logical(1)
  else if (bank$marital[i] == "unknown")
    bank$unknown[i] <- as.logical(1)
}

bank$hsyes <- as.logical(0)
bank$hsno <- as.logical(0)
bank$hsunknown <- as.logical(0)

for (i in 1:nrow(bank)){
  if (bank$housing[i] == "yes")
    bank$hsyes[i] <- as.logical(1)
  else if (bank$housing[i] == "no")
    bank$hsno[i] <- as.logical(1)
  else if (bank$housing[i] == "unknown")
    bank$hsunknown[i] <- as.logical(1)
}

bank$loanyes <- as.logical(0)
bank$loanno <- as.logical(0)
bank$loanunknown <- as.logical(0)

for (i in 1:nrow(bank)){
  if (bank$loan[i] == "yes")
    bank$loanyes[i] <- as.logical(1)
  else if (bank$loan[i] == "no")
    bank$loanno[i] <- as.logical(1)
  else if (bank$loan[i] == "unknown")
    bank$loanunknown[i] <- as.logical(1)
}

bank$defyes <- as.logical(0)
bank$defno <- as.logical(0)
bank$defunknown <- as.logical(0)

for (i in 1:nrow(bank)){
  if (bank$default[i] == "yes")
    bank$defyes[i] <- as.logical(1)
  else if (bank$default[i] == "no")
    bank$defno[i] <- as.logical(1)
  else if (bank$default[i] == "unknown")
    bank$defunknown[i] <- as.logical(1)
}

bank$poutfailure <- as.logical(0)
bank$poutsuccess <- as.logical(0)
bank$poutnonexistent <- as.logical(0)

for (i in 1:nrow(bank)){
  if (bank$poutcome[i] == "failure")
    bank$poutfailure[i] <- as.logical(1)
  else if (bank$poutcome[i] == "nonexistent")
    bank$poutnonexistent[i] <- as.logical(1)
  else if (bank$poutcome[i] == "success")
    bank$poutsuccess[i] <- as.logical(1)
}


bank$jan <- as.logical(0)
bank$feb <- as.logical(0)
bank$mar <- as.logical(0)
bank$apr <- as.logical(0)
bank$may <- as.logical(0)
bank$jun <- as.logical(0)
bank$jul <- as.logical(0)
bank$aug <- as.logical(0)
bank$sep <- as.logical(0)
bank$oct <- as.logical(0)
bank$nov <- as.logical(0)
bank$dec <- as.logical(0)

for (i in 1:nrow(bank)){
  if (bank$month[i] == "jan")
    bank$jan[i] <- as.logical(1)
  else if (bank$month[i] == "feb")
    bank$feb[i] <- as.logical(1)
  else if (bank$month[i] == "mar")
    bank$mar[i] <- as.logical(1)
  else if (bank$month[i] == "apr")
    bank$apr[i] <- as.logical(1)
  else if (bank$month[i] == "may")
    bank$may[i] <- as.logical(1)
  else if (bank$month[i] == "jun")
    bank$jun[i] <- as.logical(1)
  else if (bank$month[i] == "jul")
    bank$jul[i] <- as.logical(1)
  else if (bank$month[i] == "aug")
    bank$aug[i] <- as.logical(1)
  else if (bank$month[i] == "sep")
    bank$sep[i] <- as.logical(1)
  else if (bank$month[i] == "oct")
    bank$oct[i] <- as.logical(1)
  else if (bank$month[i] == "nov")
    bank$nov[i] <- as.logical(1)
  else if (bank$month[i] == "dec")
    bank$dec[i] <- as.logical(1)
}

bank$mon <- as.logical(0)
bank$tue <- as.logical(0)
bank$wed <- as.logical(0)
bank$thu <- as.logical(0)
bank$fri <- as.logical(0)

for (i in 1:nrow(bank)){
  if (bank$day_of_week[i] == "mon")
    bank$mon[i] <- as.logical(1)
  else if (bank$day_of_week[i] == "tue")
    bank$tue[i] <- as.logical(1)
  else if (bank$day_of_week[i] == "wed")
    bank$wed[i] <- as.logical(1)
  else if (bank$day_of_week[i] == "thu")
    bank$thu[i] <- as.logical(1)
  else if (bank$day_of_week[i] == "fri")
    bank$fri[i] <- as.logical(1)
}

# logistic regression

model <- glm(family = "binomial", data = bank, y ~ age + housemaid + services + management + admin. + blue_collar + technician + retired + self_employed + entrepreneur + unemployed + student + married + single + divorced + defyes + defno + hsyes + hsno + loanyes + loanno + poutfailure + poutsuccess + duration + previous + emp.var.rate)

summary(model)

model1 <- glm(family = "binomial", data = bank, y ~ age + housemaid + services + management + admin. + blue_collar + technician + retired + self_employed + entrepreneur + unemployed + student + married + single + divorced + defyes + defno + hsyes + hsno + loanyes + loanno + poutfailure + poutsuccess + previous + emp.var.rate)

summary(model1)


model4 <- glm(family = "binomial", data = bank, y ~ age + housemaid + services + management + admin. + blue_collar + technician + retired + self_employed + entrepreneur + unemployed + student + married + single + divorced + defyes + defno + hsyes + hsno + loanyes + loanno + poutfailure + poutsuccess + previous + emp.var.rate + cons.price.idx + cons.conf.idx + campaign  + contact + jan + feb + mar + apr + may + jun + jul + aug + sep + oct + nov + mon + tue + wed + thu)

summary(model4)

new <- data.frame(y = NA, age = 50, housemaid = T, services = F, management = F, admin. = F, blue_collar = F, technician = F, retired = F, self_employed = F, entrepreneur = F, unemployed = F, student = F, married = F, single = F, divorced = F, defyes = F, defno = T, hsyes = F, hsno = F, loanyes = T, loanno = F, poutfailure = F,  poutsuccess = T, previous = 2, emp.var.rate = 1.3, cons.price.idx = 92.631, cons.conf.idx = -40, campaign = 2, contact = "cellular", jan = F, feb = F, mar = T, apr = F, may = F, jun = F, jul = F, aug = F, sep = F, oct = F, nov = F, mon = F, tue = F, wed = T, thu = F)

predict(model4, new, type = "response")

new1 <- data.frame(y = NA, age = 50, housemaid = T, services = F, management = F, admin. = F, blue_collar = F, technician = F, retired = F, self_employed = F, entrepreneur = F, unemployed = F, student = F, married = F, single = F, divorced = F, defyes = F, defno = T, hsyes = F, hsno = F, loanyes = T, loanno = F, poutfailure = F,  poutsuccess = T, previous = 2, emp.var.rate = 1.3)

predict(model1, new1, type = "response")