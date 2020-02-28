#ch8 8.1 table example
data("UCBAdmissions")
ftable(UCBAdmissions)
library(stats4)
data("infert")
tt=table(infert$education,infert$parity)
tt
tt1 = addmargins(tt,1)
tt1
tt12 = addmargins(tt,c(1,2))
tt12
dimnames(tt12)
prop.table(tt,2)
#   sapply function
class (ChickWeight)
sapply(ChickWeight,class)
sapply(infert,class)
# e.g. df[,sapply(df,class) =='numeric'] to create a df w only numeric variables
infertNum = infert[,sapply(infert,class) =='numeric']
# 8.5 Mapping a Func based on Groups example
data("iris")
#find the means of all four variables by species
aggregate(iris[-5],iris[5],mean)
#calculate the mean weight 
cweights =  aggregate (ChickWeight$weight, ChickWeight[c('Time','Diet')],mean)
head(cweights)
#alternative
list(Time=ChickWeight$Time, Diet=ChickWeight$Diet)
listWeights = list(Time=ChickWeight$Time, Diet=ChickWeight$Diet)
head(listWeights)
# tapply
data("PlantGrowth")
maxweight = tapply(PlantGrowth$weight,PlantGrowth$group,max)
maxweight
as.data.frame(as.table(maxweight))
#to use name other than Freq in the data frame
as.data.frame.table(as.table(maxweight),responseName='MaxWeight')
#get range of weights for each group
ranges = tapply(PlantGrowth$weight,PlantGrowth$group,range)
ranges
ranges[[1]]
ranges[['trt1']]
#convert values to numeric matrix, then combine matrix with dimnames
data.frame(group=dimnames(ranges)[[1]],matrix(unlist(ranges),ncol=2,byrow=TRUE))
#E.g. process more than a single vector 
#Example, find the max eigenvalue of the correlation matrices of the 4 var in iris
#1/ define func to provide the required result for a single data frame
maxeig = function(df)eigen(cor(df))$val[1]
#2/ pass to split to get a list of df
frames = split(iris[-5],iris[5])
#pass to sapply with the function maxeig
sapply(frames,maxeig)
#alternate, less direct, pass vector of the row indeices to tapply
tapply(1:nrow(iris),iris['Species'],function(ind,data)eigen(cor(data[ind,-5]))
      $val[1],data=iris)
#similar to the original:
sapply(split(iris[-5],iris[5]),function(df)eigen(cor(df))$val[1])
#use by function
max.e=by(iris,iris$Species,function(df)eigen(cor(df[-5]))$val[1])
max.e
#by return a scalar, convert to data frame
as.data.frame(as.table(max.e))