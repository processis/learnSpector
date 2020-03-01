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
