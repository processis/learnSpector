# 229 experiment on Mwiki stat access file, read dates
rawAccess <-read.csv('/media/user/1907USB/statWiki/op2R/access213comma.csv', header = FALSE)
#remove the first char in the date time column
rawAccess$V4 <- gsub('^\\[','',rawAccess$V4)
library(chron)
AccessTimes = c("13/Feb/2020:07:08:02","14/Feb/2020:07:08:02",
           "15/Jan/2020:07:08:02","13/Dec/2020:22:48:02",
           "13/Mar/2020:07:08:02")
#  aTparts = t(as.data.frame(strsplit(AccessTimes,'^:')))
#instead of strsplit, remove the last 9 char of the date/time field to get date only
rawAccess$V11<-rawAccess$V4
# rawAccess$V11<-as.character(rawAccess$V11) <- not required
rawAccess$V11 <- gsub('.{9}$','',rawAccess$V11)
#do the same to get time
rawAccess$V12<-rawAccess$V4
rawAccess$V12 <- gsub('^.{12}','',rawAccess$V12)
rawAccess$V13 = chron(dates=rawAccess$V11,times=rawAccess$V12,
                 format=c('d/mon/y','h:m:s'))
#strptime to convert original date time to POSIX format
rawAccess$V14 = strptime(rawAccess$V4,
                  format='%d/%b/%Y:%H:%M:%S')
# rawAccessPosix = data.frame(as.POSIXlt(rawAccess$V14)) #created single col df
# names(rawAccessPosix))
rawAccess$V15=(as.POSIXlt(rawAccess$V14))$hour
rawAccess$V16=(as.POSIXlt(rawAccess$V14))$min
#2020.3.1  try to read Date Time from mwiki logs
rawPgs <-read.csv('/media/user/1907USB/statWiki/op2R/#!hdsxRSKMutf8.csv',header = FALSE)
# try one date
tmp<- as.character("2020年2月13日")
tmp<-gsub('(年|月|日)','/',tmp)
tmp<-gsub('/$','',tmp)
tmp<- as.character("11:50?")
tmp<-gsub('\\?$','',tmp)
dtimes=c("11:50 :00","1:59 :00")
theTimes = chron(times=dtimes,format='h:m:s')
#convert date and time
rawPgs$V5 <- as.character(rawPgs$V5)
rawPgs$V5<-gsub('(年|月|日)','/',rawPgs$V5)
rawPgs$V5 <-gsub('/$','',rawPgs$V5)
rawPgs$V7 <- as.character(rawPgs$V7)
rawPgs$V7 <- gsub('\\?$','',rawPgs$V7)
rawPgs$V7 <- paste(rawPgs$V7,":00")
library(chron)
#create chron date/time and POSIX date/time from V5 V7
rawPgs$DateTime = chron(dates=rawPgs$V5,times=rawPgs$V7,
                      format=c('y/m/d','h:m:s'))

#strptime to convert original date time to POSIX format
rawPgs$Posix = gsub('^\\(','',rawPgs$DateTime)
rawPgs$Posix = gsub('\\)$','',rawPgs$Posix)
rawPgs$Posix = strptime(rawPgs$Posix,
                         format='%y/%m/%d %H:%M:%S')
# names(rawAccessPosix))
rawPgs$Posix.hour=(as.POSIXlt(rawPgs$Posix))$hour
