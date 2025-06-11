# Read Desharnais77 public dataset from promise uottawa repository
desharnais <- read.table("/media/user/娱乐/learnTitanic/the big R/dashar/desharnaisLogEffort77.csv",sep = ",", header = TRUE)
desha <- subset(desharnais,Project!=76)
desha <- subset(desha,Project!=67)
desha <- subset(desha,Project!=45)
desha <- subset(desha,Project!=40)
desha <- subset(desha,Language1==1)
desha <- desha[,c(1:13)] 
desha$TeamExp<-as.numeric(desha$TeamExp)
desha$ManagerExp<-as.numeric(desha$ManagerExp)
View(desha)
# Keep only Project+Effor + 8 independent var, rremove yearEnd, PointsNnAdjust, Lang3 
desha <- desha[,c(1,2,3,(5:8),(11:13))]
### randomly keep 4 project rows , remaining 71 rows, keep as Training set
deshaTrain <- subset(desha,Project!=1)
deshaTrain <- subset(deshaTrain,Project!=2)
deshaTrain <- subset(deshaTrain,Project!=3)
deshaTrain <- subset(deshaTrain,Project!=4)
deshaTrain <- subset(deshaTrain,Project!=5)
deshaTrain <- subset(deshaTrain,Project!=6)
### keep the 4 rows in Test dataset
deshaTest <- subset(desha,Project < 7)
### remove first column Project and Lang1 Lang2 from both Train and Test sets
deshaTrain <- deshaTrain[,c(2:8)]
deshaTest <- deshaTest[,c(2:8)]
deshaTrainY = deshaTrain$Effort
deshaTestY = deshaTest$Effort
#remove Effort from X set
deshaTrainX = deshaTrain[,c(1:3,5:7)]
deshaTestX = deshaTest[,c(1:3,5:7)]
swEngTestY = deshaTestY
swEngTrainY = deshaTrainY
swEngTrainX = deshaTrainX
swEngTestX = deshaTestX
swEngTrainXtrans = deshaTrainX
swEngTestXtrans = deshaTestX
###
View(deshaTrain)
#try Kuhn ch6 Linear Regression and cousins
library(AppliedPredictiveModeling)

library(lattice)
### Some initial plots of the data

xyplot(swEngTrainY ~ swEngTrainX$Entities, type = c("p", "g"), ylab = "Effort", main = "(a)", xlab = "Entities")
xyplot(swEngTrainY ~ swEngTrainX$Transactions, type = c("p", "g"), ylab = "Effort", xlab = "Trans")

library(caret)

library(corrplot)
#look at both X and Y variables, so look at deshaTrain instead of swEngTrainXtrans
corrplot::corrplot(cor(deshaTrain),  order = "hclust",  tl.cex = .8)
### Section 6.2 Linear Regression
### Save the test set results in a data frame                 
testResults <- data.frame(obs = swEngTestY,
                         Linear_Regression = predict(lmTune0, testXfiltered))

tooHigh <- findCorrelation(cor(swEngTrainXtrans), .8)  #findCorr.... not find
trainXfiltered <- swEngTrainXtrans[, -tooHigh]
#testXfiltered  <-  swEngTestXtrans[, -tooHigh]
set.seed(100)
indx <- createFolds(swEngTrainY, returnTrain = TRUE)
ctrl <- trainControl(method = "cv", index = indx)

set.seed(100)
lmTune <- train(x = trainXfiltered, y = swEngTrainY, method = "lm", trControl = ctrl)

lmTune
### Save the test set results in a data frame                 
testResults <- data.frame(obs = swEngTestY, Linear_Regression = predict(lmTune, testXfiltered))
testResults$lmTune <- predict(lmTune, swEngTestXtrans)
### no filter use all variables for regression
set.seed(100)
indx <- createFolds(swEngTrainY, returnTrain = TRUE)
ctrl <- trainControl(method = "cv", index = indx)
set.seed(100)

lmTune0 

testResults$lmTune0 <- predict(lmTune0, swEngTestXtrans)

### Section 6.4 Penalized Models
## There is now a simple ridge regression method.

ridgeGrid <- expand.grid(lambda = seq(0, .1, length = 15))

set.seed(100)
ridgeTune <- train(x = swEngTrainXtrans, y = swEngTrainY, method = "ridge", tuneGrid = ridgeGrid, trControl = ctrl, preProc = c("center", "scale"))
ridgeTune
