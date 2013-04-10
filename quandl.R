##### download quandl data on futures contracts
# libraries
library(plyr)
library(reshape2)
library(ggplot2)
# list the markets you want data on
# Currencies: Australian Dollar, Canadian Dollar, British Pound Sterling, Japanese Yen, Euro
currencies<-list("AD","CD","BP","JY","EC")
# Energy: WTI Crude Oil, Natural Gas
energy<-list("CL","NG")
# Grains: Corn, Soybeans
grains<-list("C","S")
# Metals: Gold, Copper
metals<-list("GC","HG")
# Softs: Sugar, Coffee
softs<-list("KC") # SB (sugar) is messed up from first glance
# Meats: Live Cattle
meats<-list("LC")
# Bonds: 30 Year Treasury Bond, 3-Month Eurodollar
bonds<-list("US","ED")
# Equities: S&P 500, Nikkei 225
equities<-list("SP","NK")
# All markets combined
markets<-c(currencies,energy,grains,metals,softs,meats,bonds,equities)
# quandl function
quandl<-function(symbol) {
  symbol<-read.csv(paste0('http://www.quandl.com/api/v1/datasets/OFDP/FUTURE_',noquote(symbol),'1.csv?&auth_token=Kmr4ZhymduxsKdgJgLt4'), colClasses=c('Date'='Date'))
}
# apply quandl function to all the markets and assign it to futures variable
# there is an API limit of 100 per day so be careful
futures.orig<-lapply(markets,quandl)
# rename the list of data frames to the correct futures market name
names(futures.orig)<-markets
# take only the date and the settle value
futures<-llply(futures.orig,function(x) x[,c(1,5)])
futures<-ldply(futures,melt,id="Date")
futures$variable<-futures$.id
futures<-futures[,-1]
colnames(futures)[1]<-"date"
futures<-arrange(futures,date)
head(futures)

data<-subset(futures,date>="2011-01-01")
data.ma<-ddply(data,.(variable),transform,MA100=rollmean(value,k=100,fill=NA,align="right"))
ggplot(data.ma,aes(x=date,y=value,group=variable,color=variable))+
  geom_line()+geom_line(aes(y=MA100,group=variable),color="black")+
  facet_wrap(~variable,scale="free")

