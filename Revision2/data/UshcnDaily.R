d <- c(6, 4, 2, 4, rep(diff(c(16, 21, 22, 23, 24)), times=31))
# tmp <- readLines("~/Documents/R Projects/Datasets/UshcnDailyData.txt")
# 
# station.list <- read.fwf("~/Documents/R Projects/Datasets/ushcn-stations.txt", widths=diff(c(0, 6, 15, 25, 32, 35, 66, 73, 80, 87, 90)), na.strings="  ------", comment.char="")
# names(station.list) <- c("ID", "Lat", "Long", "Elevation", "State", "Name", "C1", "C2", "C3", "UTCOffset")
# station.list[which(station.list$ID<100000),1] <- paste("0", as.character(station.list[which(station.list$ID<100000),1]), sep="")
# station.list[,7:9] <- apply(station.list[,7:9], 2, function(str) gsub("------", "", str))
# station.list[,c(5, 6)] <- apply(station.list[,5:6], 2, factor)
# station.sub <- subset(station.list, abs(Lat-35)<5 & Elevation<500 & Long>-110)
# 
# library(stringr)
# # get which lines to read from tmp
# years <- str_sub(tmp, 7, 10)
# years <- as.numeric(years)
# elements <- str_sub(tmp, 13, 16)
# stations <- str_sub(tmp, 1, 6)
# 
# lines <- which((elements=="TMAX") & (years>2007) & (stations%in%unique(station.sub$ID)))
# 
# writeLines(tmp[lines], "~/Documents/R Projects/Datasets/UshcnDailyDataSubset.txt")

tmp <- read.fwf("~/Documents/R Projects/Datasets/UshcnDailyDataSubset.txt", widths=d, col.names=c("ID", "Year", "Month", "Element", unlist(lapply(1:31, function(i) paste(c("value","mflag", "qflag", "sflag"), i, sep="")))))

tmp$ID[which(tmp$ID<100000)] <-  paste("0", tmp$ID[which(tmp$ID<100000)], sep="")
tmp <- tmp[,c(1:4, 1+(1:31)*4)]
tmp2 <- tmp
tmp[,5:35][tmp[,5:35]==-9999] <- NA

tmp <- subset(tmp, Element%in%c("TMAX", "TMIN"))
library(reshape2)
library(lubridate)
tmp2 <- melt(tmp, id.vars=1:4, measure.vars=5:35, value.name="value", variable.name="Day", na.rm=TRUE)
tmp2 <- dcast(tmp2, ID+Year+Month+Day~Element)
tmp2$Day <- as.numeric(gsub("value", "", tmp2$Day))
tmp2$date <- ymd(paste(tmp2$Year, tmp2$Month, tmp2$Day, sep="-"))
tmp2$week <- floor_date(tmp2$date, "week")
tmp2$week.num <- week(tmp2$date)

ushcn <- ddply(tmp2, .(ID, week), summarize, max=max(TMAX), min=min(TMIN))
ushcn$week.num <- week(ushcn$week)
ushcn$week.num <- floor(ushcn$week.num/2)*2+1
ushcn$week.num <- year(ushcn$week)+ushcn$week.num/53.5

ushcn <- subset(ushcn, ID%in%names(table(ushcn$ID))[table(ushcn$ID)>200])

ushcn.weekly <- ddply(ushcn, .(week.num), summarize, week=min(week), temp=mean(max))

qplot(data=subset(ushcn, ID=="468384"), x=week, ymin=min, ymax=max, geom="ribbon", group=ID) 
+ geom_line(data=ushcn.weekly, aes(x=week, y=temp), colour="blue") + xlab("Time") + ylab("Temperature (F)") + theme_bw()
