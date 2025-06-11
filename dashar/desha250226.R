write.table(testResults,file="/home/user/Downloads/testResultsSvmRfCubist.csv",sep=",") # output testResults.csv
# Read Desharnais77 public dataset from promise uottawa repository
desharnais <- read.table("/media/user/娱乐/learnTitanic/the big R/dashar/desharnaisLogEffort77.csv",
                                                  sep = ",", header = TRUE)
desha <- subset(desharnais,Project!=76)
# continue to remove incomplete rows, bottom up, because not enough data in these projects
desha <- subset(desha,Project!=67)
desha <- subset(desha,Project!=45)
desha <- subset(desha,Project!=40)
#desha <- subset(desha,Language1==1)
desha <- desha[,c(1:13)] 


### because ? in some fields under these columns 
#   has to specify as numeric
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


# Read Desharnais77 public dataset from promise uottawa repository
desharnais <- read.table("/media/user/娱乐/learnTitanic/the big R/dashar/desharnaisLogEffort77.csv",
                                                  sep = ",", header = TRUE)
desha <- subset(desharnais,Project!=76)
# continue to remove incomplete rows, bottom up, because not enough data in these projects
desha <- subset(desha,Project!=67)
desha <- subset(desha,Project!=45)
desha <- subset(desha,Project!=40)
desha <- subset(desha,Language1==1)
desha <- desha[,c(1:13)] 
### because ? in some fields under these columns 
#   has to specify as numeric
desha$TeamExp<-as.numeric(desha$TeamExp)
desha$ManagerExp<-as.numeric(desha$ManagerExp)
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

xyplot(swEngTrainY ~ swEngTrainX$Entities, type = c("p", "g"),
               ylab = "Effort",
               main = "(a)",
               xlab = "Entities")
xyplot(swEngTrainY ~ swEngTrainX$Transactions, type = c("p", "g"),
               ylab = "Effort",
               xlab = "Trans")

library(caret)
library(corrplot)
#look at both X and Y variables, so look at deshaTrain instead of swEngTrainXtrans
corrplot::corrplot(cor(deshaTrain), 
                                       order = "hclust", 
                                       tl.cex = .8)

### Section 6.2 Linear Regression
### Save the test set results in a data frame                 
#testResults <- data.frame(obs = swEngTestY,
#                         Linear_Regression = predict(lmTune0, testXfiltered))

tooHigh <- findCorrelation(cor(swEngTrainXtrans), .8)
trainXfiltered <- swEngTrainXtrans[, -tooHigh]
testXfiltered  <-  swEngTestXtrans[, -tooHigh]
set.seed(100)
indx <- createFolds(swEngTrainY, returnTrain = TRUE)
ctrl <- trainControl(method = "cv", index = indx)

set.seed(100)


lmTune <- train(x = trainXfiltered, y = swEngTrainY,
                                method = "lm",
                                trControl = ctrl)

lmTune

### Save the test set results in a data frame                 
testResults <- data.frame(obs = swEngTestY,
                                                    Linear_Regression = predict(lmTune, testXfiltered))
testResults$lmTune <- predict(lmTune, swEngTestXtrans)
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



### Non-Linear Regression Models非线性回归模型
###
### Required packages: AppliedPredictiveModeling, caret, doMC (optional), earth,
###                    kernlab, lattice, nnet 必需的包: caret, doMC (optional), earth 等
###
### Data used: The swEngubility from the AppliedPredictiveModeling package使用的数据:来自AppliedPredictiveModeling包的可切换性
###
### Notes:说明
### This code should help the user reproduce the results in the
### text. There will be differences between this code and what is is
### the computing section. For example, the computing sections show
### how the source functions work (e.g. randomForest() or plsr()),
### which were not directly used when creating the book. Also, there may be
### syntax differences that occur over time as packages evolve. These files
### will reflect those changes.此代码此代码应帮助用户在文本中重现结果。这段代码和计算部分之间会有区别。例如，计算部分显示源函数如何工作(例如randomForest()或plsr())，它们在创建图书时没有直接使用。此外，随着包的发展，可能会出现语法差异。这些文件将反映这些变化。不保修。

###
###  In some cases, the calculations in the book were run in
### parallel. The sub-processes may reset the random number seed.
### Your results may slightly vary.在某些情况下，子进程可以重置随机数种子。你的结果可能略有不同。

################################################################################
# 10.24 added RandomForest, and Cubist from 08_Regression Tree,
#        and write testResults to ...RfCubist.csv从Regression Tree中添加




library(AppliedPredictiveModeling)

### Create a control funciton that will be used across models. We
### create the fold assignments explictily instead of relying on the
### random number seed being set to identical values.创建一个可以跨模型使用的控件功能。我们明确地创建折叠分配，而不是依赖于随机数种子被设置为相同的值。

library(caret)
set.seed(100)
indx <- createFolds(swEngTrainY, returnTrain = TRUE)
ctrl <- trainControl(method = "cv", index = indx)

################################################################################
### Neural Networks神经网络

### Optional: parallel processing can be used via the 'do' packages,
### such as doMC, doMPI etc. We used doMC (not on Windows) to speed
### up the computations. 可选:并行处理可以通过“do”包使用，比如doMC、doMPI等。我们使用doMC(不是在Windows上)来加速计算。

### WARNING: Be aware of how much memory is needed to parallel
### process. It can very quickly overwhelm the availible hardware. We
### estimate the memory usuage (VSIZE = total memory size) to be
### 2677M/core. 警告:注意并行进程需要多少内存。它可以很快地压倒现有的硬件。

#library(doMC)
#registerDoMC(4)




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



################################################################################
### Multivariate Adaptive Regression Splines 多元自适应回归样条

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


################################################################################
###  Support Vector Machines 支持向量机

## In a recent update to caret, the method to estimate the
## sigma parameter was slightly changed. These results will
## slightly differ from the text for that reason.在最近对插入符号的更新中，估计sigma参数的方法略有改变。由于这个原因，这些结果将与文本略有不同。

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


################################################################################
#write.table(testResults,file="testResults.csv",sep=",") # output testResults.csv

################################################################################
### K-Nearest Neighbors k最近邻算法

### First we remove near-zero variance predictors
#knnDescr <- swEngTrainXtrans[, -nearZeroVar(swEngTrainXtrans)]

#set.seed(100)
#knnTune <- train(x = knnDescr, y = swEngTrainY,
#                 method = "knn",
#                 preProc = c("center", "scale"),
#                 tuneGrid = data.frame(k = 1:20),
#                 trControl = ctrl)

#knnTune

#plot(knnTune)

#testResults$Knn <- predict(svmRTune, swEngTestXtrans[, names(knnDescr)])

################################################################################
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
