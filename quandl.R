##### download quandl data on futures contracts
# libraries

# load packages
libs<-c("reshape2","plyr","data.table","lubridate","gdata","ggplot2","RColorBrewer","scales","devtools","jfreels","quantmod","data.table")
lapply(libs,require,character.only=TRUE)
#install_github("r_jfreels","jfreels")

# list the markets you want data on
                 # Currencies: Australian Dollar, Canadian Dollar, British Pound Sterling, Japanese Yen, Euro, Brazilian Real
futures.list<-list(currency=c("AD","CD","BP","JY","EC","BR"),
                 # Energy: WTI Crude Oil, Brent Crude Oil, Natural Gas
                 energy=c("CL","B","NG"),
                 # Metals: Gold, Silver, Copper
                 metals=c("GC","SI","HG"),
                 # Grains: Corn, Wheat, Soybeans
                 grains=c("C","W","S"),
                 # Softs: Coffee, Cocoa, Cotton
                 softs=c("KC","CC","CT"),
                 # Meats: Live Cattle, Lean Hogs, Feeder Cattle
                 meats=c("LC","LN","FC"),
                 # Bonds: 10-year Treasury Note, 30-year Treasury Bond, 3-Month Eurodollar
                 bonds=c("TY","US","ED"),
                 # Equities: S&P 500, Nikkei 225
                 equities=c("SP","NK"))
futures.df<-ldply(futures.list,melt)
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
futures<-llply(futures.orig,function(x) x[,c("Date","Settle")])
futures<-ldply(futures,melt,id="Date")
futures$variable<-futures$.id
futures$.id<-NULL
colnames(futures)<-tolower(colnames(futures))
futures<-arrange(futures,date)
names(futures)<-c("date","market","value")
futures<-join(futures,futures.df,by="market")
futures<-data.table(futures,key=c("market,date"))

# GGPLOT2 markets with 100 day moving average
DT<-futures[date>=as.Date('2011-01-01')]
DT.start.date<-max(DT[,min(date),by=market]$V1)
DT.end.date<-min(DT[,max(date),by=market]$V1)
DT[,MA100:=rollmean(value,k=100,fill=NA,align="right"),by=market]
ggplot(DT,aes(x=date,y=value,group=market,color=sector))+
  geom_line()+geom_line(aes(y=MA100,group=market),color="black")+
  facet_wrap(sector~market,scale="free")+
  labs(x=NULL,y=NULL,title="Futures Markets (2011-Present): Prices and 100 Day Moving Average")

# Horizon Plot of 100 day ma
DT[,ror100:=ror(value,100),by=market]
horizon.panel.ggplot(DT[,list(date,market,ror100)],"Futures Markets (2011-Present): Horizon Plot of 100 Day Rolling Returns")

# Horizon Plot of returns
DT.april2013<-futures[date>=as.Date('2013-04-01')]
DT.april2013[,ror:=ror(value),by=market]
horizon.panel.ggplot(DT.april2013[,list(date,market,ror)],title="Futures Markets (2011- Present): Horizon Plot of Daily Returns")

# data.table analysis
fdt<-data.table(futures,key=c("market","date"))