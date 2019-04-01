# reading both the files from my computer

scores <- read.csv("F:\\Parth Parikh\\us\\NYU\\2nd semester\\Business Analytics\\HW-3\\scores.csv")
income <- read.csv("F:\\Parth Parikh\\us\\NYU\\2nd semester\\Business Analytics\\HW-3\\IRSIncomeByZipCode_NoStateTotalsNoSmallZips.csv")

#taking only the required columns in the scores1 dataset
scores1 <- scores[,c(8,19:21)]
View(scores1)

#taking only the required rows in the income1 dataset
income1 <- income[income$STATE == "NY", ]
View(income1)

# taking the mean of the SAT scores for a specific zip code
install.packages("plyr")
library("plyr")
install.packages("dplyr")
library("dplyr")
scores2 <- ddply(scores1, "Zip.Code", numcolwise(mean))
View(scores2)

scores3 <- scores2[!is.na(scores2$Average.Score..SAT.Math.),]
View(scores3)

same <- income1[income1$ZIPCODE %in% scores3$Zip.Code,]
View(same)

same1 <- same[, c(2, 7)]
View(same1)

#dividing the income values by 15 since the values were very large
same1$Total.income.amount <- same1$Total.income.amount / 15

# merging both the dataframes
final <- merge.data.frame(same1, scores3, by.x = "ZIPCODE", by.y = "Zip.Code")
View(final)


# plotting all the scores and the income
install.packages("ggplot2")
library(ggplot2)
?ggplot

LEGEND <- c("Math Score")
reading_score <- c("Reading Score")
writing_score <- c("Writing Score")

p <- ggplot(data = final)
p + geom_smooth(aes(x = final$Total.income.amount, y =  final$Average.Score..SAT.Math., color = LEGEND), fill = NA) + geom_smooth(aes(x = final$Total.income.amount, y =  final$Average.Score..SAT.Reading., color = reading_score), fill = NA) + geom_smooth(aes(x = final$Total.income.amount, y =  final$Average.Score..SAT.Writing., color = writing_score), fill = NA) + xlab("Total Income (in dollars)") + ylab("SAT Scores") + coord_cartesian(ylim = c(375, 575), xlim = c(20000, 1000000))
