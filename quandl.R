##### download quandl data on futures contracts
# libraries
library(plyr)
library(reshape2)
library(ggplot2)
# list the markets you want data on
                 # Currencies: Australian Dollar, Canadian Dollar, British Pound Sterling, Japanese Yen, Euro
futures.list<-list(currency=c("AD","CD","BP","JY","EC"),
                 # Energy: WTI Crude Oil, Natural Gas
                 energy=c("CL","NG"),
                 # Metals: Gold, Copper
                 metals=c("GC","HG"),
                 # Grains: Corn, Soybeans
                 grains=c("C","S"),
                 # Softs: Coffee
                 softs=c("KC"),
                 # Meats: Live Cattle
                 meats=c("LC"),
                 # Bonds: 30 Year Treasury Bond, 3-Month Eurodollar
                 bonds=c("US","ED"),
                 # Equities: S&P 500, Nikkei 225
                 equities=c("SP","NK"))
futures.df<-ldply(marketDF,melt)
names(futures.df)<-c("sector","market")
futures.df$market<-as.character(futures.df$market)
# All markets combined
markets<-as.list(futures.df$market)
# quandl function
quandl<-function(symbol) {
  symbol<-read.csv(paste0('http://www.quandl.com/api/v1/datasets/OFDP/FUTURE_',noquote(symbol),'1.csv?&auth_token=Kmr4ZhymduxsKdgJgLt4'), colClasses=c('Date'='Date'))
}
# apply quandl function to all the markets and assign it to futures variable
# there is an API limit of 100 per day so be careful
futures.orig<-lapply(markets,quandl)
# rename the list of data frames to the correct futures market name
names(futures.orig)<-futures.df$market
# take only the date and the settle value
futures<-llply(futures.orig,function(x) x[,c(1,5)])
futures<-ldply(futures,melt,id="Date")
futures$variable<-futures$.id
futures<-futures[,-1]
colnames(futures)[1]<-"date"
futures<-arrange(futures,date)
names(futures)<-c("date","market","value")
futures<-join(futures,futures.df,by="market")
head(futures)


data<-subset(futures,date>="2011-01-01")
data.ma<-ddply(data,.(market),transform,MA100=rollmean(value,k=100,fill=NA,align="right"))
ggplot(data.ma,aes(x=date,y=value,group=market,color=sector))+
  geom_line()+geom_line(aes(y=MA100,group=market),color="black")+
  facet_wrap(~market,scale="free")


