#try ggplot on Access Log data only
rawAccess <-read.csv('/media/user/1907USB/statWiki/op2R/access213comma.csv', header = FALSE)
#cleanse rawAccess
rawAccess <- rawAccess[,c("V1","V4","V6","V7","V8","V9","V10")]
#remove the first char in the date time column
rawAccess$V4 <- gsub('^\\[','',rawAccess$V4)
library(chron)
#instead of strsplit, remove the last 9 char of the date/time field to get date only
rawAccess$date<-rawAccess$V4
# rawAccess$V11<-as.character(rawAccess$V11) <- not required
rawAccess$date <- gsub('.{9}$','',rawAccess$date)
#do the same to get time
rawAccess$time<-rawAccess$V4
rawAccess$time <- gsub('^.{12}','',rawAccess$time)
rawAccess$DateTime = chron(dates=rawAccess$date,times=rawAccess$time,
                      format=c('d/mon/y','h:m:s'))
#strptime to convert original date time to POSIX format
rawAccess$POSIX = strptime(rawAccess$V4,
                         format='%d/%b/%Y:%H:%M:%S')
#separate the GET POST out to a variable column
rawAccess$action = as.factor(gsub(' .+$','',rawAccess$V6))
rawAccess$V6 = gsub('^[A-Z]+ ','',rawAccess$V6)
############
#check out strsplit
urlink = "/mediawiki.ma/load.php?debug=false&lang=zh-cn"
urlink= "/favicon.ico HTTP/1.1"
parts = strsplit(urlink,'/')
length(parts[[1]])
######
tmpSite = strsplit(rawAccess$V6,'/')
rawAccess$site = tmpSite(2)
tempSite = tmpSite[2,]