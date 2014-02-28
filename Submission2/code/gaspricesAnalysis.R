
library(lubridate)
library(plyr)
library(ggplot2)
library(scales)
library(gridExtra)
gasprices <- read.csv("./Submission2/data/GasPrices.csv", stringsAsFactors=F)
gasprices$date <- ymd(gasprices$date)
gasprices$year <- year(gasprices$date)
gasprices <- subset(gasprices, year>=1995)
gasprices$month <- floor_date(gasprices$date, "month")+days(14)
gasprices <- ddply(gasprices, .(date), transform, avg=mean(price), sd=sd(price))
monthly <- ddply(gasprices, .(month), summarize, sd=sd(price-avg), var=var(price-avg), sd.avg=sd(avg), price=mean(price), year=year(month[1]))
monthly <- ddply(monthly, .(year), transform, avg.sd=mean(sd))

## 
p1 <- ggplot()  + geom_jitter(aes(date, price), data=subset(gasprices, year>1995), size=2, alpha=.1, colour=I("grey50")) + 
  geom_line(aes(month, price), data=subset(monthly, year(month)>1995), colour="black", size=1) + 
  ylab("Price per gallon (USD)") + theme_bw()+ xlab("Time") + 
  ggtitle("Price of Gasoline in the US, 1995-2014") +
  scale_x_datetime(breaks=date_breaks("4 years"), labels = date_format("%Y"))

p2 <- ggplot( data=subset(monthly, year(month)>1995) ) + geom_line(aes(month, sd)) + ylab(expression(sigma))  + xlab("Time")+ theme_bw() + ggtitle("Standard Deviation of Monthly Prices")

grid.arrange(p1, p2, nrow=1)
