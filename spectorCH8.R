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

#8.1
pets = c('dog','cat','duck','chicken','duck','cat','dog')
tt = table(pets)
tt
tt['duck']

tt['dog']
as.data.frame(tt)

hiinc = state.x77[,'Income'] > median(state.x77[,'Income'])
stateinc = table(state.region,hiinc)
stateinc

as.data.frame(stateinc)
x = data.frame(a=c(1,2,2,1,2,2,1),b=c(1,2,2,1,1,2,1),
               c=c(1,1,2,1,2,2,1))
x

as.data.frame(table(x))

tt = table(infert$education,infert$parity)
tt

tt1 = addmargins(tt,1)
tt1

tt12 = addmargins(tt,c(1,2))
tt12

dimnames(tt12)
prop.table(tt,2)

ftable(UCBAdmissions)
xtabs(~state.region + hiinc)
x = data.frame(a=c(1,2,2,1,2,2,1),b=c(1,2,2,1,1,2,1),
               c=c(1,1,2,1,2,2,1))
dfx = as.data.frame(table(x))
xtabs(Freq ~ a + b + c,data=dfx)

#8.3
text = c('R is a free environment for statistical analysis',
         'It compiles and runs on a variety of platforms',
         'Visit the R home page for more information')
result = strsplit(text,'')
result

length(result)
nwords = sapply(result,length)
nwords

class(ChickWeight)
sapply(ChickWeight,class)

#df[,sapply(df,class) == ’numeric’]
#maxcor = function(i,n=10,m=5){
#mat = matrix(rnorm(n*m),n,m)
#corr = cor(mat)
#diag(corr) = NA
#max(corr,na.rm=TRUE)
#}

maxcors = sapply(1:1000,maxcor,n=100)
mean(maxcors)

t.test(rnorm(10),rnorm(10))$statistic
tsim = replicate(10000,t.test(rnorm(10),rnorm(10))$statistic)
quantile(tsim,c(0.5,0.75,0.9,0.95,0.99))

#8.4
sstate = scale(state.x77,center=apply(state.x77,2,median),
               scale=apply(state.x77,2,mad))

#summfn = function(x)c(n=sum(!is.na(x)),mean=mean(x),sd=sd(x))

x = apply(state.x77,2,sumfun)
t(x)

x = 1:12
apply(matrix(x,ncol=3,byrow=TRUE),1,sum)
mns = colMeans(USJudgeRatings)
mns


jscore = rowSums(USJudgeRatings >= 8)
head(jscore)

maxes = apply(state.x77,2,max)
swept = sweep(state.x77,2,maxes,"/")
head(swept)

meds = apply(state.x77,2,median)
meanmed = function(var,med)mean(var[var>med])
meanmed(state.x77[,1],meds[1])
meanmed(state.x77[,2],meds[2])
sweep(state.x77,2,meds,meanmed)

mapply(meanmed,as.data.frame(state.x77),meds)

#8.5
aggregate(iris[-5],iris[5],mean)
cweights => aggregate(ChickWeight$weight,
                       ChickWeight[c('Time','Diet')],mean)
head(cweights)

maxweight = tapply(PlantGrowth$weight,PlantGrowth$group,max)

maxweight

as.data.frame(as.table(maxweight))
as.data.frame.table(as.table(maxweight),
                    responseName='MaxWeight')

ranges = tapply(PlantGrowth$weight,PlantGrowth$group,range)
ranges

ranges[[1]]
ranges[['trt1']]

data.frame(group=dimnames(ranges)[[1]],
           matrix(unlist(ranges),ncol=2,byrow=TRUE))

ranges1 = tapply(CO2$uptake,CO2[c('Type','Treatment')],range)
ranges1

ranges[['Quebec','chilled']]

data.frame(expand.grid(dimnames(ranges1)),
           matrix(unlist(ranges1),byrow=TRUE,ncol=2))

meds = tapply(CO2$uptake,CO2[c('Type','Treatment')],median)
inds = tapply(CO2$uptake,CO2[c('Type','Treatment')])
inds

adj.uptake = CO2$uptake - meds[inds]
adj.uptake = CO2$uptake -
  ave(CO2$uptake,CO2[c('Type','Treatment')],FUN=median)

adj.uptake = CO2$uptake -
  ave(CO2$uptake,CO2$Type,CO2$Treatment,FUN=median)

maxeig = function(df)eigen(cor(df))$val[1]
frames = split(iris[-5],iris[5])
sapply(frames,maxeig)

sapply(split(iris[-5],iris[5]),
       function(df)eigen(cor(df))$val[1])

tapply(1:nrow(iris),iris['Species'],
       function(ind,data)eigen(cor(data[ind,-5]))$val[1],
       data=iris)

max.e = by(iris,iris$Species,
           function(df)eigen(cor(df[-5]))$val[1])
max.e

as.data.frame(as.table(max.e))

sumfun = function(x)data.frame(n=length(x$uptake),
                               mean=mean(x$uptake),sd=sd(x$uptake))

bb = by(CO2,CO2[c('Type','Treatment')],sumfun)
bb

do.call(rbind,bb)
cbind(expand.grid(dimnames(bb)),do.call(rbind,bb))

#8.6

states = data.frame(state.x77,state=row.names(state.x77),
                    region=state.region,row.names=1:50)

library(reshape)
mstates = melt(states)

cast(mstates,region~variable,mean)
cast(mstates,variable~region,mean)

cast(mstates,region~variable,mean,
     subset=variable %in% c('Population','Life.Exp'))

cast(mstates,.~variable,c(mean,median,sd),
     subset=variable %in% c('Population','Life.Exp'))

cast(mstates,variable~.,c(mean,median,sd),
     subset=variable %in% c('Population','Life.Exp'))

cast(mstates,region~variable,c(mean,median,sd),
     subset=variable %in% c('Population','Life.Exp'))

cast(mstates,variable~.|region,
     c(mean,median,sd),
     subset=variable%in%c('Population','Life.Exp'))

mChick = melt(ChickWeight,measure.var='weight')
head(cast(mChick,Diet + Time ~ variable,median))

cast(mChick,Diet ~ Time + variable,mean)

cast(mChick,Time ~ variable|Diet,mean)

xChickWeight = subset(ChickWeight,
                      !(Diet == 1 & Time == 4))
mxChick = melt(xChickWeight,measure.var='weight')

head(cast(mxChick,Diet + Time ~ variable,median))

head(cast(mxChick,Diet + Time ~ variable,median,
          add.missing=TRUE))

head(recast(xChickWeight,measure.var='weight',
            Diet + Time ~ variable,median,
            add.missing=TRUE))

#8.7
dat = matrix(rnorm(1000000),10000,100)
system.time(mns <- rowMeans(dat))
system.time(mns <- apply(dat,2,mean))
system.time({m <- ncol(dat)
           for(i in 1:m)mns[i] <- mean(dat[,i])})

slowmean = function(dat){
  n = dim(dat)[1]
  m = dim(dat)[2]
  mns = numeric(m)
  for(i in 1:n){
    sum = 0;
    for(j in 1:m)sum = sum + dat[j]
    mns[i] = sum / n
  }
  return(mns)
}

system.time(mns <- slowmean(dat))

system.time({m = dim(dat)[1];mns = rep(1,m) %*% dat / m})

system.time(m <- matrix(1:100,10000,100,byrow=TRUE))

buildrow = function(){
  res = NULL
  for(i in 1:10000)res = rbind(res,1:100)
  res
}

system.time(buildrow())

buildcol = function(){
  res = NULL
  for(i in 1:10000)res = cbind(res,1:100)
  t(res)
}

system.time(buildcol())

buildrow1 = function(){
    res = matrix(0,10000,100)
      for(i in 1:10000)res[i,] = 1:100
          res
}
system.time(buildrow1())

somerow1 = function(){
    res = NULL
      for(i in 1:10000)if(runif(1) < .5)res = rbind(res,1:100)
          res
}
system.time(somerow1())

somerow2 = function(){
    res = matrix(0,10000,100)
      k = 0
        for(i in 1:10000)if(runif(1) < .5){
            k = k + 1
              res[k,] = 1:100
        }
        res[1:k,]
}
system.time(somerow2())

somerow3 = function(){
    res = list()
      for(i in 1:10000)if(runif(1) < .5)res = c(res,list(1:100))
          do.call(rbind,res)
}
system.time(somerow3())












