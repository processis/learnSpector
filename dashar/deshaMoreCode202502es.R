
#library(doMC)
#registerDoMC(4)

library(caret)

################################################################################
# 10.24 added RandomForest, and Cubist from 08_Regression Tree,
#        and write testResults to ...RfCubist.csv从Regression Tree中添加RandomForest和Cubist，并将测试结果写入…rfcubs .csv
library(AppliedPredictiveModeling)
 
### Create a control funciton that will be used across models. We
### create the fold assignments explictily instead of relying on the
### random number seed being set to identical values.创建一个可以跨模型使用的控件功能。我们明确地创建折叠分配，而不是依赖于随机数种子被设置为相同的值。

library(caret)
set.seed(100)
indx <- createFolds(swEngTrainY, returnTrain = TRUE)
ctrl <- trainControl(method = "cv", index = indx)


#######
nnetGrid <- expand.grid(decay = c(0, 0.01, .1), size = c(1, 3, 5, 7, 9, 11, 13), bag = FALSE)

set.seed(100)
nnetTune <- train(x = swEngTrainXtrans, y = swEngTrainY,
                    method = "avNNet",
                    trControl = ctrl,
                    preProc = c("center", "scale"),
                    linout = TRUE,
                   trace = FALSE,
                   MaxNWts = 13 * (ncol(swEngTrainXtrans) + 1) + 13 + 1,
                    maxit = 1000,
                   allowParallel = FALSE)
nnetTune

################################################################################
### Multivariate Adaptive Regression Splines 多元自适应回归样条

set.seed(100)
marsTune <- train(x = swEngTrainXtrans, y = swEngTrainY, method = "earth", tuneGrid = expand.grid(degree = 1, nprune = 2:38), trControl = ctrl)