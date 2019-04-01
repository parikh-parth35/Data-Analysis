# reading the csv file

chicago15 <- read.csv("F:\\Parth Parikh\\us\\NYU\\Excel and R practice\\Crimes in Chicago\\chicago15.csv", header = T)
View(chicago15)
str(chicago15)

chicago15$zipcode <- factor(chicago15$zipcode)

chicago15$ARSON <- as.logical(0)
chicago15$ASSAULT <- as.logical(0)
chicago15$BATTERY <- as.logical(0)
chicago15$BURGLARY <- as.logical(0) 
chicago15$CONCEALEDCARRYLICENSEVIOLATION <- as.logical(0)
chicago15$CRIMSEXUALASSAULT <- as.logical(0)
chicago15$CRIMINALDAMAGE <- as.logical(0)
chicago15$CRIMINALTRESPASS <- as.logical(0)
chicago15$DECEPTIVEPRACTICE <- as.logical(0)
chicago15$GAMBLING <- as.logical(0)
chicago15$HOMICIDE <- as.logical(0)
chicago15$HUMANTRAFFICKING <- as.logical(0)
chicago15$INTERFERENCEWITHPUBLICOFFICER <- as.logical(0)
chicago15$INTIMIDATION <- as.logical(0)    
chicago15$KIDNAPPING <- as.logical(0)
chicago15$LIQUORLAWVIOLATION <- as.logical(0)
chicago15$MOTORVEHICLETHEFT <- as.logical(0)
chicago15$NARCOTICS <- as.logical(0)
chicago15$NONCRIMINAL <- as.logical(0)
chicago15$NON_CRIMINAL <- as.logical(0)
chicago15$OBSCENITY <- as.logical(0)
chicago15$OFFENSEINVOLVINGCHILDREN <- as.logical(0)
chicago15$OTHERNARCOTICVIOLATION <- as.logical(0)
chicago15$OTHEROFFENSE <- as.logical(0)
chicago15$PROSTITUTION <- as.logical(0)    
chicago15$PUBLICINDECENCY <- as.logical(0)
chicago15$PUBLICPEACEVIOLATION <- as.logical(0)
chicago15$ROBBERY <- as.logical(0)
chicago15$SEXOFFENSE <- as.logical(0)
chicago15$STALKING <- as.logical(0)
chicago15$THEFT <- as.logical(0)
chicago15$WEAPONSVIOLATION <- as.logical(0)

for (i in 1:nrow(chicago15)){
  if (chicago15$Primary.Type[i] == "ARSON")
    chicago15$ARSON[i] <- as.logical(1)
  else if (chicago15$Primary.Type[i] == "ASSAULT")
    chicago15$ASSAULT[i] <- as.logical(1)
  else if (chicago15$Primary.Type[i] == "BATTERY")
    chicago15$BATTERY[i] <- as.logical(1)
  else if (chicago15$Primary.Type[i] == "BURGLARY")
    chicago15$BURGLARY[i] <- as.logical(1)
  else if (chicago15$Primary.Type[i] == "CONCEALED CARRY LICENSE VIOLATION")
    chicago15$CONCEALEDCARRYLICENSEVIOLATION[i] <- as.logical(1)
  else if (chicago15$Primary.Type[i] == "CRIM SEXUAL ASSAULT")
    chicago15$CRIMSEXUALASSAULT[i] <- as.logical(1)
  else if (chicago15$Primary.Type[i] == "CRIMINAL DAMAGE")
    chicago15$CRIMINALDAMAGE[i] <- as.logical(1)
  else if (chicago15$Primary.Type[i] == "CRIMINAL TRESPASS")
    chicago15$CRIMINALTRESPASS[i] <- as.logical(1)
  else if (chicago15$Primary.Type[i] == "DECEPTIVE PRACTICE")
    chicago15$DECEPTIVEPRACTICE[i] <- as.logical(1)
  else if (chicago15$Primary.Type[i] == "GAMBLING")
    chicago15$GAMBLING[i] <- as.logical(1)
  else if (chicago15$Primary.Type[i] == "HOMICIDE")
    chicago15$HOMICIDE[i] <- as.logical(1)
  else if (chicago15$Primary.Type[i] == "HUMAN TRAFFICKING")
    chicago15$HUMANTRAFFICKING[i] <- as.logical(1)
  else if (chicago15$Primary.Type[i] == "INTERFERENCE WITH PUBLIC OFFICER")
    chicago15$INTERFERENCEWITHPUBLICOFFICER[i] <- as.logical(1)
  else if (chicago15$Primary.Type[i] == "INTIMIDATION")
    chicago15$INTIMIDATION[i] <- as.logical(1)
  else if (chicago15$Primary.Type[i] == "KIDNAPPING")
    chicago15$KIDNAPPING[i] <- as.logical(1)
  else if (chicago15$Primary.Type[i] == "LIQUOR LAW VIOLATION")
    chicago15$LIQUORLAWVIOLATION[i] <- as.logical(1)
  else if (chicago15$Primary.Type[i] == "MOTOR VEHICLE THEFT")
    chicago15$MOTORVEHICLETHEFT[i] <- as.logical(1)
  else if (chicago15$Primary.Type[i] == "NARCOTICS")
    chicago15$NARCOTICS[i] <- as.logical(1)
  else if (chicago15$Primary.Type[i] == "NON - CRIMINAL")
    chicago15$NONCRIMINAL[i] <- as.logical(1)
  else if (chicago15$Primary.Type[i] == "NON-CRIMINAL")
    chicago15$NON_CRIMINAL[i] <- as.logical(1)
  else if (chicago15$Primary.Type[i] == "OBSCENITY")
    chicago15$OBSCENITY[i] <- as.logical(1)
  else if (chicago15$Primary.Type[i] == "OFFENSE INVOLVING CHILDREN")
    chicago15$OFFENSEINVOLVINGCHILDREN[i] <- as.logical(1)
  else if (chicago15$Primary.Type[i] == "OTHER NARCOTIC VIOLATION")
    chicago15$OTHERNARCOTICVIOLATION[i] <- as.logical(1)
  else if (chicago15$Primary.Type[i] == "OTHER OFFENCE")
    chicago15$OTHEROFFENCE[i] <- as.logical(1)
  else if (chicago15$Primary.Type[i] == "PROSTITUTION")
    chicago15$PROSTITUTION[i] <- as.logical(1)
  else if (chicago15$Primary.Type[i] == "PUBLIC INDECENCY")
    chicago15$PUBLICINDECENCY[i] <- as.logical(1)
  else if (chicago15$Primary.Type[i] == "PUBLIC PEACE VIOLATION")
    chicago15$PUBLICPEACEVIOLATION[i] <- as.logical(1)
  else if (chicago15$Primary.Type[i] == "ROBBERY")
    chicago15$ROBBERY[i] <- as.logical(1)
  else if (chicago15$Primary.Type[i] == "SEX OFFENSE")
    chicago15$SEXOFFENSE[i] <- as.logical(1)
  else if (chicago15$Primary.Type[i] == "STALKING")
    chicago15$STALKING[i] <- as.logical(1)
  else if (chicago15$Primary.Type[i] == "THEFT")
    chicago15$THEFT[i] <- as.logical(1)
  else if (chicago15$Primary.Type[i] == "WEAPONS VIOLATION")
    chicago15$WEAPONSVIOLATION[i] <- as.logical(1)
}

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

set.seed(100)
split <- (0.8)
trainingrowindex <- sample(1:nrow(chicago15), split*nrow(chicago15))
trainingdata <- chicago15[trainingrowindex,]
testdata <- chicago15[-trainingrowindex,]

# developing the model on training data
model <- glm(family = "binomial", data = trainingdata, Arrest ~ ARSON + ASSAULT + BATTERY + BURGLARY +  CONCEALEDCARRYLICENSEVIOLATION + CRIMSEXUALASSAULT + CRIMINALDAMAGE + CRIMINALTRESPASS + DECEPTIVEPRACTICE + GAMBLING + HOMICIDE + HUMANTRAFFICKING + INTERFERENCEWITHPUBLICOFFICER + INTIMIDATION + KIDNAPPING + LIQUORLAWVIOLATION + MOTORVEHICLETHEFT + NARCOTICS + NONCRIMINAL + NON_CRIMINAL + OBSCENITY + OFFENSEINVOLVINGCHILDREN + OTHERNARCOTICVIOLATION + PROSTITUTION + PUBLICINDECENCY + PUBLICPEACEVIOLATION + ROBBERY + SEXOFFENSE + STALKING + THEFT + WEAPONSVIOLATION + night + evening)

summary(model)

new <- data.frame(Arrest=NA, ARSON=F, ASSAULT=F, BATTERY=F, BURGLARY=F, CONCEALEDCARRYLICENSEVIOLATION=F, CRIMSEXUALASSAULT=F, CRIMINALDAMAGE=F, CRIMINALTRESPASS=F, DECEPTIVEPRACTICE=F, GAMBLING=F, HOMICIDE=F, HUMANTRAFFICKING=F, INTERFERENCEWITHPUBLICOFFICER=F, INTIMIDATION=F, KIDNAPPING=F, LIQUORLAWVIOLATION=F, MOTORVEHICLETHEFT=F, NARCOTICS=F, NONCRIMINAL=F, NON_CRIMINAL=F, OBSCENITY=F, OFFENSEINVOLVINGCHILDREN=F, OTHERNARCOTICVIOLATION=F, PROSTITUTION=F, PUBLICINDECENCY=F, PUBLICPEACEVIOLATION=T, ROBBERY=F, SEXOFFENSE=F, STALKING=F, THEFT=F, WEAPONSVIOLATION=F, night = T, evening = F)

response <- ifelse(predict(model, testdata, type = "response") > 0.5,1, 0) 
actual_preds <- data.frame(cbind(actual = testdata$Arrest, predicted = response))
head(actual_preds, 100)

# finding out the accuracy
correlation <- cor(actual_preds$actual, actual_preds$predicted)
correlation 
