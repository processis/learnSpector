#4.1
as.Date('1915-6-16')
as.Date('1990/02/17')

as.Date('1-15-2001',format='%m%d%Y')
as.Date('April 26,2001',format='%B %d,%Y')
as.Date('22JUN01',format='%d%b%y')

thedate=as.Date('1/15/2001',format='%m%d%Y')
ndate=as.numeric(thedate)
ndate

class(ndate)='Date'
ndate

f=url('http://cran.cnr.berkeley.edu/src/base/R-2','r')

#rdates = data.frame()
#while (1) {
#  1=readLines(f,1)
#  if(length(1)==0)break
#  if(regexpr('href=''R-',1)>-1){
#    parts=strsplit(1,'')[[1]]
#    rver=sub('^.*>(R-.*').tar.gz.*','\\1',1)
#    date=parts[18]
#    rdates = rbind(rdates,data.frame(ver=rver,Date=date))
#  }
  
#}

#rdates$Date = as.Date(rdates$Date,’%d-%B-%Y’)

table(weekdays(rdates$Date))

#4.2

library(chron)
dtimes = c("2002-06-09 12:45:40","2003-01-29 09:30:40",
           "2002-09-04 16:45:40","2002-11-13 20:00:40",
           "2002-07-07 17:30:40")
dtparts = t(as.data.frame(strsplit(dtimes,'')))
row.names(dtparts) = NULL
thetimes = chron(dates=dtparts[,1],times=dtparts[,2],
                 format=c('y-m-d','h:m:s'))
thetimes

#4.3
dts = c("2005-10-21 18:47:22","2005-12-24 16:39:58",
        "2005-10-28 07:30:05 PDT")
as.POSIXlt(dts)

dts = c(1127056501,1104295502,1129233601,1113547501,
        1119826801,1132519502,1125298801,1113289201)
mydates = dts
class(mydates) = c('POSIXt','POSIXct')
mydates

mydates = structure(dts,class=c('POSIXt','POSIXct'))

mydate = strptime('16/Oct/2005:07:51:00',
                  format='%d/%b/%Y:%H:%M:%S')

mydates = c('20060515 112504.5','20060518 101000.3',
            '20060520 20035.1')

dtparts = t(as.data.frame(strsplit(mydates,'')))
dtimes = strptime(dtparts[,1],format='%Y%m%d') +
  as.numeric(dtparts[,2])
dtimes

ISOdate(2006,5,16,7,15,04,tz="PDT")

thedate = ISOdate(2005,10,21,18,47,22,tz="PDT")

format(thedate,'%A, %B %d, %Y %H:%M:%S')

mydate = as.POSIXlt('2005-4-19 7:01:00')
names(mydate)
mydate$mday

#4.4
rdates = scan(what="")
rdates = as.data.frame(matrix(rdates,ncol=2,byrow=TRUE))
rdates[,2] = as.Date(rdates[,2],format='%d%b%Y')
names(rdates) = c("Release","Date")
rdates

mean(rdates$Date)
range(rdates$Date)
rdates$Date[11] - rdates$Date[1]

#4.5

b1=ISOdate(1997,7,13)
b2=ISOdate(2003,8,14)
b2-b1

difftime(b2,b1,units='weeks')

ydiff = (b2 - b1) / 365.25
ydiff

attr(ydiff,'units') = 'years'
ydiff

#4.6

seq(as.Date('1976-7-4'),by='days',length=10)

seq(as.Date('2000-6-1'),to=as.Date('2000-8-1'),by='2 weeks')


table(format(rdates$Date,'%A'))
fdate = factor(format(rdates$Date,'%Y'))
fdate


