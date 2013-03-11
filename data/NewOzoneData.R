library(reshape)
library(plyr)
library(ggplot2)
library(lubridate)
oldwd <- getwd()
setwd("./data/Ozone")

# 
# # Temperature data from:
# # http://cdo.ncdc.noaa.gov/qclcd_ascii/
# 
# 
# tempfiles <- paste("2011", c(rep("0", 9), rep("", 3)), 1:12, sep="")
# 
# getTempData <- function(str){
#   filename <- paste("QCLCD", str, ".zip", sep="")
#   if(!filename %in% list.files())
#   download.file(paste("http://cdo.ncdc.noaa.gov/qclcd_ascii/QCLCD", str, ".zip", sep=""),filename)
#   d1 <- read.csv(unz(filename, paste(str, "daily.txt", sep="")), na.strings=c("M", "-"), stringsAsFactors=FALSE)
#   d2 <- read.csv(unz(filename, paste(str, "station.txt", sep="")), sep="|", stringsAsFactors=FALSE)
# 
# # Locate Houston-HOBBY airport code
#   wban <- d2$WBAN[grepl("HOBBY", d2$Location)]
#   d3 <- subset(d1, WBAN==wban)[,c(1, 2, 3, 5, 7, 11, 15, 17, 19, 21, 31, 33, 41)]
#   d3$PrecipTotal[d3$PrecipTotal=="  T"] <- 0
#   d3[2:ncol(d3)] <- apply(d3[,2:ncol(d3)], 2, as.numeric)
#   return(d3)
# }
# 
# tempdata <- rbind.fill(lapply(tempfiles, getTempData), stringsAsFactors=FALSE)
# tempdata$time <- ymd(tempdata$YearMonthDay)
# tempdata <- tempdata[,c(ncol(tempdata), 3:(ncol(tempdata)-1))]
# 
# # Ozone Data from 
# # http://www.epa.gov/airdata/ad_data_daily.html
# # Pollutant: Ozone
# # Year: 2011
# # Geographic Area - Select a City: Houston-SugarLand-Baytown, TX
# # All Sites
# # Include Exceptional Events data 
# #    (we can get rid of the outliers later...)
# 
# # ozonedata <- read.csv("OzoneData2012.csv")
# ozonedata <- read.csv("OzoneData2011.csv")
# ozonedata$time <- mdy(ozonedata$Date)
# ozonedata <- ozonedata[,c(21, 2, 4, 5, 19, 20)]
# names(ozonedata) <- c("time", "SiteID", "Ozone", "units", "Lat", "Long")
# ozoneavg <- ddply(ozonedata, .(time), summarize, Ozone=mean(Ozone))
# 
# 
# data <- merge(tempdata, ozonedata)
# write.csv(data, "2011OzoneTempData.csv", row.names=FALSE)
data <- read.csv("2011OzoneTempData.csv", stringsAsFactors=FALSE)
# qplot(data=subset(data, DewPoint<60 & Tmax>40), x=Tmax, y=Ozone)

library(locfit)

datasub <- subset(data, Tmax>45 & DewPoint<60)
# qplot(data=ddply(datasub, .(Tmax), summarize, var=var(Ozone)), x=Tmax, y=var)
model <- locfit(data=datasub, Ozone~Tmax)
modelderiv <- locfit(data=datasub, Ozone~Tmax, deriv=1)
quantiles <- quantile(datasub$Tmax, seq(0, .9, .1))
datasub$quant <- sapply(datasub$Tmax, function(i) sum(i>=quantiles))
datasub$fit <- fitted(model)
datasub$resid <- resid(model)
datasub$deriv.fit <- fitted(modelderiv)
datasub <- ddply(datasub, .(quant), transform, stdev=sd(resid))
# qplot(Tmax, stdev^2, data=datasub, geom="line", group=quant,ylab="Residual Variance of each Temperature Decile")


write.csv(datasub, "Ozone-subset.csv", row.names=FALSE)

setwd(oldwd)