#9.1                        
Loblolly$logheight = log(Loblolly$height)
Loblolly['logheight'] = log(Loblolly['height'])

with(Loblolly,log(height))
Loblolly = transform(Loblolly,logheight = log(height))
iris[,-5] = sapply(iris[,-5],function(x)x/2.54)

#9.2
bigsepal = iris$Sepal.Length > 6
sepalgroup = 1 + (iris$Sepal.Length >= 5) +
  (iris$Sepal.Length >= 7)

sepalgroup = cut(iris$Sepal.Length,c(0,5,7,10),
                 include.lowest=TRUE,right=FALSE)

newgroup = ifelse(group %in% c(1,5),1,2)

newgroup = ifelse(group %in% c(1,5),1,
                  ifelse(group %in% c(2,4),2,3))

x = c(-1.2,-3.5,-2.8,-1.1,-0.7)
newx = ifelse(x > 0,log(x),abs(x))
newx

x = c(-1.2,-3.5,-2.8,1.1,-0.7)
newx = ifelse(x > 0,log(x),abs(x))
newx

newx = numeric(length(x))
newx[x > 0] = log(x[x > 0])
newx[x <= 0] = abs(x[x <= 0])
newx

newx = sapply(x,function(t)if(t > 0)log(t) else abs(t))

#9.3
newgroup = recode(group,'c(1,5)=1;c(2,4)=2;else=3')

#9.4
mydata = data.frame(grp1=c(12,15,19,22,25),
                    grp2=c(18,12,42,29,44),
                    grp3=c(8,17,22,19,31))

mydata

sdata = stack(mydata)
head(sdata)

mydata = unstack(sdata,values~ind)
head(mydata)

set.seed(17)
obs = data.frame(subj=rep(1:4,rep(3,4)),
                 time=rep(1:3),
                 x=rnorm(12),y=rnorm(12))
obs

wideobs = reshape(obs,idvar='subj',v.names=c('x','y'),
                  timevar='time',direction='wide')
wideobs

obs = reshape(wideobs)
head(obs)

usp = data.frame(type=rownames(USPersonalExpenditure),
                 USPersonalExpenditure,row.names=NULL)
usp


rr = reshape(usp,varying=list(names(usp)[-1]),direction='long')
head(rr)

rr=reshape(usp,varying=list(names(usp)[-1]),idvar='type',
           times=seq(1940,1960,by=5),v.names='expend',
           direction='long')
head(rr)

rr1 = reshape(usp,varying=names(usp)[-1],idvar='type',
              split=list(regexp='X1',include=TRUE),direction='long')
head(rr1)

#9.5
library(reshape)
usp = data.frame(type=rownames(USPersonalExpenditure),
                 USPersonalExpenditure,row.names=NULL)
musp = melt(usp)
head(musp)

musp$variable = as.numeric(sub('X','',musp$variable))
names(musp)[2:3] = c('time','expend')
head(musp)

set.seed(17)
obs = data.frame(subj=rep(1:4,rep(3,4)),
                 time=rep(1:3),
                 x=rnorm(12),y=rnorm(12))
mobs = melt(obs)
cast(subj ~ variable + time,data=mobs)

cast(subj ~variable|time,data=mobs)
cast(subj ~ variable + time,subset = variable == 'x',data=mobs)

#9.6
x = data.frame(a=c('A','B','C'),x=c(12,15,19))
y = data.frame(a=c('D','E','F','G'),x=c(19,21,14,12))

cbind(y,z=c(1,2))
cbind(x,y[1:3,])

intersect(names(x),names(y))
z = rbind(x,c(a='X',x=12))

z = rbind(x,data.frame(a='X',x=12))
levels(z$a)

x = data.frame(a=c(1,2,4,5,6),x=c(9,12,14,21,8))
y = data.frame(a=c(1,3,4,6),y=c(8,14,19,2))
merge(x,y)

merge(x,y,all=TRUE)
merge(x,y,all.x=TRUE)
merge(x,y,all.y=TRUE)

cities = data.frame(city=c('New York','Boston','Juneau',
                           'Anchorage','San Diego',
                           'Philadelphia','Los Angeles',
                           'Fairbanks','Ann Arbor','Seattle'),
                    state.abb= c('NY','MA','AK','AK','CA',
                                 'PA','CA','AK','MI','WA'))

cities

states = data.frame(state.abb= c('NY','MA','AK','CA',
                                  'PA','MI','WA'),
                    state=c('New York','Massachusetts','Alaska',
                             'California','Pennsylvania',
                             'Michigan','Washington'))
merge(cities,states)

zips = data.frame(state.abb=c('NY','MA','AK','AK','CA',
                               'PA','CA','AK','MI','WA'),
                  zip=c('10044','02129','99801','99516','92113',
                         '19127','90012','99709','48104','98104'))
merge(cities,zips)

#9.7
match(cities$state.abb,states$state.abb)

indices = match(x$a,y$a,nomatch=0)
y$a[indices]
x$a %in% y$a

