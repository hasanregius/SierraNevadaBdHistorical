# Costa Rica Table and Graph 
library(reshape2)
setwd("/Users/hasansulaeman/Dropbox/Sam's Sierra Bd work/Data")
data = read.csv("Samrawdata_final.csv", header=TRUE)
cr = dcast(data=data,Decade~data$BdStatus,value.var='Decade',fun.aggregate=length)
zr = dcast(data=data,year~Zscore,value.var='Zscore',fun.aggregate=length)
names(cr)[c(2,3)] = c('neg','pos')
cr$tot=cr$neg+cr$pos

for (i in 1:nrow(cr)){
  cr$lower[i]=binom.test(cr$pos[i],cr$tot[i])$conf.int[1]
  cr$upper[i]=binom.test(cr$pos[i],cr$tot[i])$conf.int[2]
}

cr$lower=cr$lower*100
cr$upper=cr$upper*100
cr$prev = cr$pos/cr$tot*100

cr$prob =  dbinom(0,cr$tot,.11)
write.csv(cr,'DecadeTable.csv')

# Retrieving Average Zscore
forties=subset(data, Decade %in% c("1900s", "1910s", "1920s", "1930s"))
forties=mean(forties$ZEScore)

forties=subset(data, Decade=="1940s")
forties=mean(forties$ZEScore)

fifties=subset(data, Decade=="1950s")
fifties=mean(fifties$ZEScore)

sixties=subset(data, Decade=="1960s")
sixties=mean(sixties$ZEScore)

seventies=subset(data, Decade=="1970s")
seventies=mean(seventies$ZEScore)

eighties=subset(data, Decade=="1980s")
eighties=mean(eighties$ZEScore)

nineties=subset(data, Decade=="1990s")
nineties=mean(nineties$ZEScore)

twothousands=subset(data, Decade=="2000s")
twothousands=mean(twothousands$ZEScore)

# Retrieving Max Zscore
thirties=subset(data, Decade %in% c("1900s", "1910s", "1920s", "1930s"))
thirties=max(thirties$ZEScore)

forties=subset(data, Decade=="1940s")
forties=max(forties$ZEScore)

fifties=subset(data, Decade=="1950s")
fifties=max(fifties$ZEScore)

sixties=subset(data, Decade=="1960s")
sixties=max(sixties$ZEScore)

seventies=subset(data, Decade=="1970s")
seventies=max(seventies$ZEScore)

eighties=subset(data, Decade=="1980s")
eighties=max(eighties$ZEScore)

nineties=subset(data, Decade=="1990s")
nineties=max(nineties$ZEScore)

twothousands=subset(data, Decade=="2000s")
twothousands=max(twothousands$ZEScore)