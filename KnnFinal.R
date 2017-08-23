install.packages("gmodels")
library(gmodels)
install.packages("ggplot2")
library(ggplot2)
setwd("C:/Users/Administrator/Documents/r/QAC Data Science - Day 1")
rawBCD <- read.table("BCD_Large.data", header = FALSE, sep = ",")
rawBCD <- rawBCD[-1]
table(rawBCD$V2)
rawBCD$V2 <- as.factor(rawBCD$V2)
summary(rawBCD)
normalise <- function(x)
  return((x - min(x)) / (max(x) - min(x)))
test_Norm <- normalise(c(1,2,3,4,5))
show(test_Norm)
BCD_N <- as.data.frame(lapply(rawBCD[2:31], normalise))
BCD_Train <- BCD_N[1:469,]
BCD_Test <- BCD_N[470:569,]
BCD_Train_Labels <- rawBCD [1:469, 1]
BCD_Test_Labels <- rawBCD [470:569, 1]

BCD_Pred1 <- knn(train = BCD_Train, test = BCD_Test, cl = BCD_Train_Labels, k = 1)
BCD_Test_Pred1 <- knn(train = BCD_Train, test = BCD_Test, cl = BCD_Train_Labels, k = 1)
Pred1Errors <- (sum(BCD_Test_Labels == BCD_Test_Pred1)/length(BCD_Test_Labels))*100

BCD_Pred2 <- knn(train = BCD_Train, test = BCD_Test, cl = BCD_Train_Labels, k = 2)
BCD_Test_Pred2 <- knn(train = BCD_Train, test = BCD_Test, cl = BCD_Train_Labels, k = 2)
Pred2Errors <- (sum(BCD_Test_Labels == BCD_Test_Pred2)/length(BCD_Test_Labels))*100

BCD_Pred5 <- knn(train = BCD_Train, test = BCD_Test, cl = BCD_Train_Labels, k = 5)
BCD_Test_Pred5 <- knn(train = BCD_Train, test = BCD_Test, cl = BCD_Train_Labels, k = 5)
Pred5Errors <- (sum(BCD_Test_Labels == BCD_Test_Pred5)/length(BCD_Test_Labels))*100

BCD_Pred7 <- knn(train = BCD_Train, test = BCD_Test, cl = BCD_Train_Labels, k = 7)
BCD_Test_Pred7 <- knn(train = BCD_Train, test = BCD_Test, cl = BCD_Train_Labels, k = 7)
Pred7Errors <- (sum(BCD_Test_Labels == BCD_Test_Pred7)/length(BCD_Test_Labels))*100

BCD_Pred10 <- knn(train = BCD_Train, test = BCD_Test, cl = BCD_Train_Labels, k = 10)
BCD_Test_Pred10 <- knn(train = BCD_Train, test = BCD_Test, cl = BCD_Train_Labels, k = 10)
Pred10Errors <- (sum(BCD_Test_Labels == BCD_Test_Pred10)/length(BCD_Test_Labels))*100

BCD_Pred15 <- knn(train = BCD_Train, test = BCD_Test, cl = BCD_Train_Labels, k = 15)
BCD_Test_Pred15 <- knn(train = BCD_Train, test = BCD_Test, cl = BCD_Train_Labels, k = 15)
Pred15Errors <- (sum(BCD_Test_Labels == BCD_Test_Pred15)/length(BCD_Test_Labels))*100

BCD_Pred20 <- knn(train = BCD_Train, test = BCD_Test, cl = BCD_Train_Labels, k = 20)
BCD_Test_Pred20 <- knn(train = BCD_Train, test = BCD_Test, cl = BCD_Train_Labels, k = 20)
Pred20Errors <- (sum(BCD_Test_Labels == BCD_Test_Pred20)/length(BCD_Test_Labels))*100

results1 <-CrossTable(x = BCD_Test_Labels, y = BCD_Test_Pred1, prop.chisq = FALSE)
results2 <-CrossTable(x = BCD_Test_Labels, y = BCD_Test_Pred2, prop.chisq = FALSE)
results5 <-CrossTable(x = BCD_Test_Labels, y = BCD_Test_Pred5, prop.chisq = FALSE)
results7 <-CrossTable(x = BCD_Test_Labels, y = BCD_Test_Pred7, prop.chisq = FALSE)
results10 <-CrossTable(x = BCD_Test_Labels, y = BCD_Test_Pred10, prop.chisq = FALSE)
results15 <-CrossTable(x = BCD_Test_Labels, y = BCD_Test_Pred15, prop.chisq = FALSE)
results20 <-CrossTable(x = BCD_Test_Labels, y = BCD_Test_Pred20, prop.chisq = FALSE)

PredErrors <- c(Pred1Errors, Pred2Errors, Pred5Errors, Pred7Errors, Pred10Errors, Pred15Errors, Pred20Errors)
KCollection <- c(1, 2, 5, 7, 10, 15, 20)
toPlot <- data.frame(PredErrors, KCollection)
ggplot(data = toPlot, aes(KCollection, PredErrors)) + geom_smooth() + geom_point()