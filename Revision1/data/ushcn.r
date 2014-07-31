# reads in the USHCN files
# only go back until year of cutoff:
cutoff <- 1980


sfiles <- dir("data/ushcn/ushcn.v2.5.0.20130107/")
sdirs <- paste("data/ushcn/ushcn.v2.5.0.20130107/", sfiles, sep="")
# ID                 1-11        Integer
# YEAR              13-16        Integer
# VALUE1            17-22        Integer
# DMFLAG1           23-23        Character
# QCFLAG1           24-24        Character
# DSFLAG1           25-25        Character
# .                 .             .
# .                 .             .
# .                 .             .
# VALUE12          116-121       Integer
# DMFLAG12         122-122       Character
# QCFLAG12         123-123       Character
# DSFLAG12         124-124       Character

library(reshape2)
library(lubridate)

getTemps <- function(fname) {
  f1 <- read.fwf(fname, widths=c(11,1, 4, rep(c( 6,1,1,1), 12)), header = FALSE, 
                 col.names = c("ID", "foo", "Year", paste(c( "Value", "DMFlag", "QCFlag", "DSFlag"), rep(1:12, each=4), sep="")))
  
  fm <- melt(f1, id.vars=c("ID", "Year"), measure.vars= (1:12)*4)
  fm$Month <- as.numeric(gsub("Value", "", fm$variable))
  
  fm$Time <- ymd("2012-03-01")
  year(fm$Time) <- fm$Year
  month(fm$Time) <- fm$Month
  
  fm$value[fm$value==-9999] <- NA
  fm
}

library(plyr)
ff <- ldply(sdirs, function(x) getTemps(x))
ff <- subset(ff, Year >= cutoff)
ff$Time <- as.Date(ff$Time)
ff$resid <- NA
times <- as.character(unique(ff$Time))
sds <- rep(NA, length(times))
avgs <- rep(NA, length(times))

for (i in 1:length(times)) {
  if (i %% 100 == 0) print(".")
  idx <- which(ff$Time == times[i])
  avgs[i] <- mean(ff$value[idx], na.rm=T)
  ff$resid[idx] <- ff$value[idx] - avgs[i]
  sds[i] <- sd(ff$value[idx], na.rm=T)
}

times <- data.frame(time=times, sd=sds, avg=avgs)
times$time <- as.Date(times$time)
times$month <- rep(1:12, length = nrow(times))
write.csv(times, "data/monthly-avgs.csv", row.names=FALSE)
write.csv(ff, file="data/ushcn.csv", row.names=FALSE)
##########################
# rest of file is not necessary - just playing around with some plots


##########################
library(ggplot2)
library(lubridate)
qplot(Time, value/100, data=ff, size=I(1), geom="jitter") + theme(legend.position="none") + ylab("Temperature (in Celsius)")
qplot(Time, resid, data=ff, size=I(1)) + theme(legend.position="none")


qplot(time, avg, data=times) + geom_smooth()
qplot(time, sds, data=times, geom="line")
qplot(month, sds, data=times) + geom_smooth()


qplot(Time, value/100, data=ff, size=I(1), geom="jitter") + geom_line(aes(time, avg/100), data=times, colour="steelblue")
ggplot()  + geom_jitter(aes(Time, value/100), data=ff, size=1) + geom_line(aes(time, avg/100), data=times, colour="steelblue", size=1.5) + ylab("Temperature (in Celsius)")
ggsave(file="figure/monthly-temps.pdf", width=15, height=4.5)

ggplot()  + geom_jitter(aes(Time, resid/100), data=ff, size=1) + geom_hline(yintercept=0, colour="steelblue", size=1) + ylab("Residual temperature (in Celsius)")
ggsave(file="figure/monthly-resid-temps.pdf", width=15, height=4.5)
######################################################################################################
# stations is a fixed width file

# COUNTRY CODE             1-2    Character
# NETWORK CODE               3    Character
# ID PLACEHOLDERS ("00")   4-5    Character
# COOP ID                 6-11    Character
# LATITUDE               13-20    Real
# LONGITUDE              22-30    Real
# ELEVATION              33-37    Real
# STATE                  39-40    Character
# NAME                   42-71    Character
# COMPONENT 1 (COOP ID)  73-78    Character
# COMPONENT 2 (COOP ID)  80-85    Character
# COMPONENT 3 (COOP ID)  87-92    Character
# UTC OFFSET             94-95    Integer


stations <- read.fwf(file="data/ushcn/ushcn-v2.5-stations.txt", 
                     widths=c(2+1+2+6, 9, 10, 7, 3, 31, 7, 7, 7, 3), 
                     header=FALSE, 
                     col.names=c("ID", "lat", "long", "elev", "state", "name", "c1", "c2", "c3", "utc"))

qplot(long, lat, data=stations, colour=ID) + theme(legend.position="none")