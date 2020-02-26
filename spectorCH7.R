#7.1
nchar(state.name)

#7.2
x = 7
y = 10
cat('x should be greater than y,but x=',x,'and y=',y,'\n')

cat('LOng strings can','be displayed over','several linee using','the fill = argument',fill = 40)


paste('one',2,'three',4,'five')

paste(c('one','two','three','four'),collapse='')

paste('X',1:5,sep='')

paste(c('X','Y'),1:5,sep='')

paste(c('X','Y'),1:5,sep='',collapse='|')

paste(c('X','Y'),1:5,'^',c('a','b'),sep='',collapse='|')

paste(c('X','Y'),1:5,'^',c('a','b'),sep='')

#7.3
substring(state.name,2,6)

mystring = 'dog cat duck'
substring(mystring,c(1,5,9),c(3,7,12))

state = 'Mississippi'
ll=nchar(state)
ltrs=substring(state,1:11,1:11)
ltrs
which(ltrs=="s")

mystring = 'dog cat duck'
substring(mystring,5,7)='feline'
mystring

mystring = 'dog cat duck'
substring(mystring,5,7)='a'
mystring

#7.4
expr='.*\\.txt'
nchar(expr)
cat(expr,'\n')

expr=readline()
nchar(expr)

#7.5
strs=c('chicken','dog','cat')
expr=paste(strs,collapse='|')
expr

#7.6
sentence=
  'R is a free software environment for statistical computing'
parts=strsplit(sentence,'')
parts

length(parts)
length(parts[[1]])

more=c('R is a free software environment for statistical
       computing','It compiles and runs on a wide variety of UNIX platforms')

result=strsplit(more,'')
sapply(result,length)

allparts = unlist(result)
allparts

str='one two three four'
strsplit(str,'')

strsplit(str,' +')

words=c('one two','three four')
strsplit(words,'')


#7.7
grep('~pop',names(LifeCycleSavings))
grep('~pop',names(LifeCycleSavings),value=TRUE)

head(LifeCycleSavings[,grep('~pop',names(LifeCycleSavings))])

inp = c('run dog run','work doggedly','CAT AND DOG')
grep('\\<dog\\>',inp,ignore.case=TRUE)

str1=c('The R Foundation','is a not for profit
       organization','working in the public interest')

str2=c('It was founded by the members',
       'of the R Core Team in order',
       'to provide support for the R project')

any(grep('profit',str1))
any(grep('profit',str2))

tst=c('one x7 two b1','three c5 four b9',
      'five six seven','a8 eight nine')
wh=regexpr('[a-z][0-9]',tst)
wh


res=substring(tst,wh,wh+attr(wh,'match.length')-1)
res

res[res !='']

wh1 = gregexpr('[a-z][0-9]',tst)
wh1

rea1=list()
for(i in 1:length(wh1))
  rea1[[i]]=substring(tst[i],wh1[[i]],
                      wh1[[i]]+
                        attr(wh1[[i]],'match.length')-1)

rea1

getexpr = function(str.greg)substring(str.greg,
                                      greg + attr(greg,'match.length')-1)

res2=mapply(getexpr,tst,wh1)
res2

#7.8
values=c('$11,317.35','$11,234.51','$11,275.89',
         '$11,278.93','$11,294.94')

as.numeric(gsub('[$,]','',values))

values=c('75.99','(20.30)','55.20')
as.numeric(gsub('\\(([0-9.]+)\\)','-\\1',values))

str= 'report: 17 value=12 time=2:00'
sub('valus=([^ ]+)','\\1',str)

sub('^.*value=([^ ]+).*$','\\1',str)

str='report: 17 value=12 time=2:00'
greg=gregexpr('value=[^ ]+',str)[[1]]

sub('value=([^ ]+)','\\1',
    substring(str,greg,greg
              +attr(greg,'match.length')-1))


















