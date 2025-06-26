#!desharnaisLogEffort77kaggle3noMissingTrain.csv
#!desharnais123fillMedLog71TrainSet.csv


#desharnaisLogEffort77kaggleLang3.csv
#desharnaisLogEffort77kaggleLang3no1-7.csv

#cleanDesharnaisLogEffort77.csv

desharnais <- read.table("cleanDesharnaisLogEffort77.csv",
                         sep = ",", header = TRUE)


# 加载ggplot2包
library(ggplot2)


hist(desharnais$Length)
hist(desharnais$PointsAjust)

# 读取数据
#!desharnais123fillMedLog71TrainSet-loglength.csv
data <- read.csv("cleanDesharnaisLogEffort77.csv")

hist(data$Length)
hist(data$PointsAjust)

# 绘制散点图
ggplot(data, aes(x = data$LogPtsAjust, y = data$LogEffort, color = data$Language)) +
  geom_point() +
  labs(title = "Scatter plot of x vs y by group",
       x = "X Variable",
       y = "Y Variable",
       color = "Group")

#data$Language<-as.factor(data$Language)

# 绘制散点图并指定颜色
ggplot(data, aes(x = data$LogPtsAjust, y = data$LogEffort, color = data$Language)) +
  geom_point(size = 3) +
  scale_color_manual(values = c("1" = "red", "2" = "black", "3" = "green")) +
  labs(title = "Scatter plot of LogPtsAjust vs LogEffort by Language",
       x = "LogPtsAjust",
       y = "LogEffort",
       color = "Language") +
  theme_minimal()










desha <- desharnais[,c(1:17)] 

desha$TeamExp<-as.numeric(desha$TeamExp)
desha$ManagerExp<-as.numeric(desha$ManagerExp)
View(desha)

deshaTrain <- subset(desha,Project!=1)
deshaTrain <- subset(deshaTrain,Project!=2)
deshaTrain <- subset(deshaTrain,Project!=3)
deshaTrain <- subset(deshaTrain,Project!=4)
deshaTrain <- subset(deshaTrain,Project!=5)
deshaTrain <- subset(deshaTrain,Project!=6)
deshaTrain <- subset(deshaTrain,Project!=7)
#deshaTrain <- subset(deshaTrain,Project!=66)


#1,2,3,4,5,6,7
library(dplyr)

deshaTest <-desha%>%filter(Project%in%c(1,2,3,4,5,6,7))


#deshaTest <- subset(desha,Project < 9)

#choose TeamExp ManagerExp  LogLength  LogEffort LOgptsAjust
deshaTrain = deshaTrain[,c(2,15,16)]

deshaTest = deshaTest[,c(2,15,16)]

deshaTrainY = deshaTrain$LogEffort
deshaTestY = deshaTest$LogEffort

#remove Effort from X set

deshaTrainX = deshaTrain[,c(1,3)]
deshaTestX = deshaTest[,c(1,3)]

swEngTestY = deshaTestY
swEngTrainY = deshaTrainY
swEngTrainX = deshaTrainX
swEngTestX = deshaTestX
swEngTrainXtrans = deshaTrainX
swEngTestXtrans = deshaTestX

library(caret)
library(corrplot)
#look at both X and Y variables, so look at deshaTrain instead of swEngTrainXtrans
corrplot::corrplot(cor(deshaTrain), 
                   order = "hclust", 
                   tl.cex = .8)

#tooHigh <- findCorrelation(cor(swEngTrainXtrans), .8)
#trainXfiltered <- swEngTrainXtrans[, -tooHigh]
#testXfiltered  <-  swEngTestXtrans[, -tooHigh]
set.seed(100)
indx <- createFolds(swEngTrainY, returnTrain = TRUE)
ctrl <- trainControl(method = "cv", index = indx)

set.seed(100)

lmTune <- train(x = swEngTrainXtrans, y = swEngTrainY,
                method = "lm",
                trControl = ctrl)

lmTune


#MODEL <-lm(deshaTrainY~deshaTrainX)

#MODEL

#summary(MODEL)

#predicted<-predict(MODEL)

#RMSE
#rmse<-sqrt(mean((deshaTrainY~predicted)^2))
#print(paste("RESM:",resm))

#RSQ
#r_squared<-summary(MODEL)$r.squared
#print("R-squared:",r_squared)


### Save the test set results in a data frame                 
testResults <- data.frame(obs = swEngTestY,
                          Linear_Regression = predict(lmTune, swEngTestXtrans))
#testResults$lmTune <- predict(lmTune, swEngTestXtrans)
### no filter use all variables for regression
set.seed(100)
indx <- createFolds(swEngTrainY, returnTrain = TRUE)
ctrl <- trainControl(method = "cv", index = indx)
set.seed(100)
lmTune0 <- train(x = swEngTrainXtrans, y = swEngTrainY,
                 method = "lm",
                 trControl = ctrl)

lmTune0  


testResults$lmTune0 <- predict(lmTune0, swEngTestXtrans)

### Section 6.4 Penalized Models
## There is now a simple ridge regression method.

ridgeGrid <- expand.grid(lambda = seq(0, .1, length = 15))

set.seed(100)
ridgeTune <- train(x = swEngTrainXtrans, y = swEngTrainY,
                   method = "ridge",
                   tuneGrid = ridgeGrid,
                   trControl = ctrl,
                   preProc = c("center", "scale"))
ridgeTune


print(update(plot(ridgeTune), xlab = "Penalty"))
testResults$ridgeTune <- predict(ridgeTune, swEngTestXtrans)
#ElasticNet
enetGrid <- expand.grid(lambda = c(0, 0.01, .1), 
                        fraction = seq(.05, 1, length = 20))
set.seed(100)
enetTune <- train(x = swEngTrainXtrans, y = swEngTrainY,
                  method = "enet",
                  tuneGrid = enetGrid,
                  trControl = ctrl,
                  preProc = c("center", "scale"))
enetTune

plot(enetTune)

testResults$Enet <- predict(enetTune, swEngTestXtrans)


### Section 6.3 Partial Least Squares

## Run PLS and PCR on swEngubility data and compare results
# removed tune grid ncomp =... to use default , max number of components
set.seed(100)
plsTune <- train(x = swEngTrainXtrans, y = swEngTrainY,
                 method = "pls",
                 trControl = ctrl)
plsTune


testResults$PLS <- predict(plsTune, swEngTestXtrans)

set.seed(100)
pcrTune <- train(x = swEngTrainXtrans, y = swEngTrainY,
                 method = "pcr",
                 trControl = ctrl)
pcrTune   


testResults$PCR <- predict(pcrTune, swEngTestXtrans)

plsResamples <- plsTune$results
plsResamples$Model <- "PLS"
pcrResamples <- pcrTune$results
pcrResamples$Model <- "PCR"
plsPlotData <- rbind(plsResamples, pcrResamples)

xyplot(RMSE ~ ncomp,
       data = plsPlotData,
       #aspect = 1,
       xlab = "# Components",
       ylab = "RMSE (Cross-Validation)",
       auto.key = list(columns = 2),
       groups = Model,
       type = c("o", "g"))

plsImp <- varImp(plsTune, scale = FALSE)
plot(plsImp, top = 25, scales = list(y = list(cex = .95)))


write.table(testResults,file="/home/user/Downloads/testResultsLmPlsPcr.csv",sep=",") # output testResults.csv

library(AppliedPredictiveModeling)

### Create a control funciton that will be used across models. We
### create the fold assignments explictily instead of relying on the
### random number seed being set to identical values.创建一个可以跨模型使用的控件功能。我们明确地创建折叠分配，而不是依赖于随机数种子被设置为相同的值。

library(caret)
set.seed(100)
indx <- createFolds(swEngTrainY, returnTrain = TRUE)
ctrl <- trainControl(method = "cv", index = indx)


library(caret)

nnetGrid <- expand.grid(decay = c(0, 0.01, .1),
                        size = c(1, 3, 5, 7, 9, 11, 13),
                        bag = FALSE)

set.seed(100)
nnetTune <- train(x = swEngTrainXtrans, y = swEngTrainY,
                  method = "avNNet",
                  tuneGrid = nnetGrid,
                  trControl = ctrl,
                  preProc = c("center", "scale"),
                  linout = TRUE,
                  trace = FALSE,
                  MaxNWts = 13 * (ncol(swEngTrainXtrans) + 1) + 13 + 1,
                  maxit = 1000,
                  allowParallel = FALSE)
nnetTune

plot(nnetTune)

testResults <- data.frame(obs = swEngTestY,
                          NNet = predict(nnetTune, swEngTestXtrans))


set.seed(100)
marsTune <- train(x = swEngTrainXtrans, y = swEngTrainY,
                  method = "earth",
                  tuneGrid = expand.grid(degree = 1, nprune = 2:38),
                  trControl = ctrl)
marsTune


plot(marsTune)

testResults$MARS <- predict(marsTune, swEngTestXtrans)

marsImp <- varImp(marsTune, scale = FALSE)
plot(marsImp, top = 25)



set.seed(100)
svmRTune <- train(x = swEngTrainXtrans, y = swEngTrainY,
                  method = "svmRadial",
                  preProc = c("center", "scale"),
                  tuneLength = 14,
                  trControl = ctrl)
svmRTune



plot(svmRTune, scales = list(x = list(log = 2)))                 

svmGrid <- expand.grid(degree = 1:2,
                       scale = c(0.01, 0.005, 0.001),
                       C = 2^(-2:5))
set.seed(100)
svmPTune <- train(x = swEngTrainXtrans, y = swEngTrainY,
                  method = "svmPoly",
                  preProc = c("center", "scale"),
                  tuneGrid = svmGrid,
                  trControl = ctrl)

svmPTune


plot(svmPTune,
     scales = list(x = list(log = 2),
                   between = list(x = .5, y = 1)))                 

testResults$SVMr <- predict(svmRTune, swEngTestXtrans)
testResults$SVMp <- predict(svmPTune, swEngTestXtrans)


set.seed(100)
indx <- createFolds(swEngTrainY, returnTrain = TRUE)
ctrl <- trainControl(method = "cv", index = indx)
### Section 8.5 Random Forests

mtryGrid <- data.frame(mtry = floor(seq(10, ncol(swEngTrainXtrans), length = 10)))


### Tune the model using cross-validation
set.seed(100)
rfTune <- train(x = swEngTrainXtrans, y = swEngTrainY,
                method = "rf",
                tuneGrid = mtryGrid,
                ntree = 1000,
                importance = TRUE,
                trControl = ctrl)

rfTune

plot(rfTune)
testResults$RF <- predict(rfTune, swEngTestXtrans) #add RF predict results
rfImp <- varImp(rfTune, scale = FALSE)
rfImp



cbGrid <- expand.grid(committees = c(1:10, 20, 50, 75, 100),
                      neighbors = c(0, 1, 5, 9))

set.seed(100)
cubistTune <- train(swEngTrainXtrans, swEngTrainY,
                    "cubist",
                    tuneGrid = cbGrid,
                    trControl = ctrl)
cubistTune


plot(cubistTune, auto.key = list(columns = 4, lines = TRUE))

cbImp <- varImp(cubistTune, scale = FALSE)
cbImp

testResults$CUBIST <- predict(cubistTune, swEngTestXtrans) #add result of CUBIST
write.table(testResults,file="/home/user/Downloads/testResultsSvmRfCubist.csv",sep=",") # output testResults.csv
