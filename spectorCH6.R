#6.4
nums = c(12,9,8,14,7,16,3,2,9)
nums > 10

nums[nums>10]

which(nums>10)

seq(along=nums)[nums>10]

nums[nums>10] = 0
nums

#6.5
x = matrix(1:12,4,3)
x

x[,1]

x[,c(3,1)]

x[2,]

x[10]

stack.x.a = stack.x[order(stack.x[,'Air.Flow']),]
head(stack.x.a)

with(iris.sortframe(iris.Sepal.Length.Sepal.Width))

riris = iris(rev(1:nrow(iris)),)
head(riris)

x = matrix(1:12,4,3)
x[,1]

x[,1,drop=FALSE]

x[,1]<3

x[x[,1]<3,]

mat = matrix(scan(),ncol=3,byrow=TRUE)
mat

newmat = matrix(NA,3,2)
newmat[mat[,1:2]] = mat[,3]
newmat

#6.6
method1=c(1,1,1,1,2,2,2,2,3,3,3,3,4,4,4,4)
method2=c(1,2,2,3,2,2,1,3,3,3,2,4,1,4,4,3)
tt=table(method1,method2)
tt

offd=row(tt)!=col(tt)
offd

tt[offd]

sum(tt[offd])


#6.7
simple = list(a=c('fred','sam','harry'),b=c(24,17,19,22))
mode(simple)

simple[2]

mode(simple[2])

mean(simple$b)
mean(simple[[2]])
mean(simple[['b']])

simple[1]
simple[[1]]

#6.8订阅数据帧

dd=data.frame(a=c(5,9,12,15,17,11),b=c(8,NA,12,10,NA,15))
dd[dd$b>10,]

dd(!is.na(dd$b)&dd$b>10,)

subset(dd,b>10)

some=subset(LifeCycleSavings,sr>10,select=c(pop15,pop75))

life1=subset(LifeCycleSavings,select=pop15:dpi)

life1=subset(LifeCycleSavings,select=1:3)

life2=subset(LifeCycleSavings,select=c(-pop15,-pop75))

life2=subset(LifeCycleSavings,select=c(2,3))

#beizhu  




