#5.1
data = c(1,2,2,3,1,2,3,3,1,2,3,3,1)
fdata = factor(data)
fdata

rdata = factor(data,labels=c("I","II","III"))
rdata

levels(fdata) = c('I','II','III')

fdata

mons = c("March","April","January","November","January",
         "September","October","September","November","August",
         "January","November","November","February","May","August",
         "July","December","August","August","September","November",
         "February","April")
mons = factor(mons)
table(mons)

mons = factor(mons,levels=c("January","February","March",
                            "April","May","June","July","August","September",
                            "October","November","December"),ordered=TRUE)
mons[1] < mons[2]

table(mons)

levels(InsectSprays$spray)
InsectSprays$spray = with(InsectSprays,
                          reorder(spray,count,mean))
levels(InsectSprays$spray)

attr(InsectSprays$spray,'scores')

levels(InsectSprays$spray)

InsectSprays$spray = relevel(InsectSprays$spray,'C')
levels(InsectSprays$spray)


#5.2

fert = c(10,20,20,50,10,20,10,50,20)
fert = factor(fert,levels=c(10,20,50),ordered=TRUE)
fert

mean(fert)
mean(as.numeric(levels(fert)[fert]))
mean(as.numeric(as.character(fert)))

#5.3

lets = sample(letters,size=100,replace=TRUE)
lets = factor(lets)
table(lets[1:5])

table(lets[1:5,drop=TRUE])

table(factor(lets[1:5]))

fact1 = factor(sample(letters,size=10,replace=TRUE))
fact2 = factor(sample(letters,size=10,replace=TRUE))
fact1

fact2

fact12 = factor(c(levels(fact1)[fact1],
                  levels(fact2)[fact2]))
fact12

#5.4
wfact = cut(women$weight,3)
table(wfact)

wfact = cut(women$weight,pretty(women$weight,3))
wfact

table(wfact)

wfact = cut(women$weight,3,labels=c('Low','Medium','High'))
table(wfact)

wfact = cut(women$weight,quantile(women$weight,(0:4)/4))
table(wfact)

#5.5
everyday = seq(from=as.Date('2005-1-1'),
               to=as.Date('2005-12-31'),by='day')

cmonth = format(everyday,'%b')
months = factor(cmonth,levels=unique(cmonth),ordered=TRUE)
table(months)

wks = cut(everyday,breaks='week')
head(wks)

qtrs = cut(everyday,"3 months",labels=paste('Q',1:4,sep=''))
head(qtrs)

#5.6
data(CO2)
newfact = interaction(CO2$Plant,CO2$Type)
nlevels(newfact)

newfact1 = interaction(CO2$Plant,CO2$Type,drop=TRUE)
nlevels(newfact1)

