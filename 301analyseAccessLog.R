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
# tmpSite = strsplit(rawAccess$V6,'/')
# rawAccess$site = tmpSite(2)
# tempSite = tmpSite[2,]
rawAccess$V6 = sub('^/','',rawAccess$V6)
rawAccess$custSiteName = sub('^http://[a-zA-Z0-9.]+/([^ ]+)','\\1',rawAccess$V6)
rawAccess$custSiteName2 = sub('/.+','',rawAccess$custSiteName)
# rawAccess$custSiteName3 = sub(paste('^[a-z]+','\\.','[a-z]+'),'\\1',rawAccess$custSiteName2)
rawAccess$custSiteName3 = factor (rawAccess$custSiteName2,levels = c("mediawiki.ma","hdsx.cmmi","lenovo.cmmi",ordered=TRUE))
rawAccess$V7 = as.factor(rawAccess$V7)
#get some basic table counts
table(rawAccess$V7)
table(rawAccess$custSiteName3)
table(rawAccess$action)
summary(rawAccess$V8)
#look at size(V8) broken by GET(action) , custSiteName3 
library(ggplot2)
rawAccess$action = factor (rawAccess$action,levels = c("GET","POST"))
rawAccess$cod200 = sub("200","1",rawAccess$V7)
rawAccess$cod200 = sub("3[0-9]+","0",rawAccess$cod200)
rawAccess$cod200 = sub("2[0-9]+","0",rawAccess$cod200)
rawAccess$cod200 = sub("4[0-9]+","0",rawAccess$cod200)
rawAccess$cod200 = as.factor(rawAccess$cod200)

# good plot
ggplot(rawAccess, aes(x = custSiteName3, fill = factor(cod200))) +
  geom_bar() +
  xlab("Customer Sites") +
  ylab("Total Count") +
  labs(fill = "cod200") 
#
# Visualize the 3-way relationship , compare to analysis take very long to run
ggplot(rawAccess, aes(x = V8, fill = cod200)) +
  facet_wrap(~action + custSiteName3) + 
  ggtitle("POST vs customers") +
  geom_histogram(binwidth = 100) +
  xlab("V8 count") +
  ylab("Total Count") 