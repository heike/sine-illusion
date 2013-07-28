# Analysis of data from Amazon Turk experiment conducuted 7/22/13-7/28/13.

library(RMySQL)
library(plyr)
library(lubridate)
library(ggplot2)
library(grid)


#----------------- Database Access -------------------------
con <- dbConnect(MySQL(), group="stat")
tab <- dbReadTable(con, name="SineIllusionShiny")[-1,]
dbDisconnect(con)
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

diffs <- c(0, .5, .5, .5, .5, .25, .25, .25, .25, .25, .25, .25, .25, .1, .1, .05, .05, .05, .05, .05, .05, .025, .025, .025, .025, .025, .025, .025, .025, .025, .025, .025, .025, .05, .05, .05, .05, .1, .1, .1, .1, .1, .25, .25, .25, .25, .25, .25, .5, .5, .5, .5)
wopts <- -4 +cumsum(diffs)

tab$time2 <- ymd_hms(tab$time) # convert to lubridate time
tab <- ddply(tab, .(iphash, fingerprint, ipid), transform, ntrials=length(unique(q+skip))) # add number of trials
tab <- tab[which(tab$weight>0),] # remove "initial" observation that no longer corresponds to weight in this iteration of the program. 
tab$weight <- wopts[tab$weight] # convert javascript vector weight into actual meaningful weight measurement


tab$training <- (tab$time2>ymd("2013-7-21") & (tab$q+tab$skip)<2) # two "training" questions added on 7.21.13 that may be biased... test?

#------------------ Data Reshaping -------------------------
# dataset of individual questions, but without each individual change
data <- ddply(tab, .(iphash, fingerprint, ipid, q, skip, type, ntrials), 
              function(df){
                with(df, 
                     data.frame(startweight = weight[which.min(time2)], 
                                endweight =  weight[which.max(time2)], 
                                modeweight =  Mode(weight), 
                                len = length(weight), 
                                unique.weights = length(unique(weight)),
                                time.trial = max(time2)-min(time2),
                                training = training[1]))
              })

# dataset of individual questions with each individual observation and time. 
tab2 <- ddply(tab, .(iphash, fingerprint, ipid, q, skip, type, ntrials), transform,
              weight = weight,
              start.weight = rep(weight[which.min(time2)],length(time2)), 
              end.weight = rep(weight[which.max(time2)],length(time2)),
              time.diff = as.numeric(c(0, diff(time2))),
              end.time = rep(min(time2)-max(time2),length(time2)),
              len = rep(length(time2),length(time2)),
              trial.time = as.numeric(time2-max(time2)),
              seq = 1:length(time2))

data$fingerid <- as.numeric(factor(data$fingerprint))
tab2$fingerid <- as.numeric(factor(tab2$fingerprint))

write.csv(tab2[,-which(names(tab2)=="userid")], "./data/IndivTrajectory.csv")
write.csv(data, "./data/SummaryTable.csv")


#-----------------------Read in Data------------------------

# use if no access to the SQL database.
# data <- read.csv("SummaryTable.csv", row.names=1, stringsAsFactors=FALSE)
# tab2 <- read.csv("IndivTrajectory.csv", row.names=1, stringsAsFactors=FALSE)
# tab2$time2 <- ymd_hms(tab2$time2)

#----------- Plots and Exploration - raw data --------------

# plot of directional arrows indicating start and end point by user and trial type.
qplot(data=subset(data, len>1), 
      x=startweight, xend=endweight, 
      y=fingerprint, yend=fingerprint, geom="segment", 
      arrow=arrow(length = unit(0.1,"cm")), group=q+skip, alpha=I(.2)) + 
  geom_vline(aes(xintercept=0), linetype=2) +
  geom_vline(aes(xintercept=1), linetype=2) +
  facet_wrap(~type)

# plot of trial trajectories for each user and trial type (not so useful now that there are a bunch of users)
qplot(data=subset(tab2, len>2 & seq>1 & ntrials>3 & trial.time>-500 & !training), x=trial.time, y=weight, group=q+skip, geom="line", colour=factor((q+skip)%%6)) + geom_point(aes(x=0, y=end.weight)) + facet_grid(type~fingerprint, scales="free_x") + xlab("Time until Trial End") + ylab("Weight") + geom_hline(yintercept=1) + geom_hline(yintercept=0)

# plot of trial trajectories, for each trial type across all users. 
ggplot() + 
  geom_point(data=data, aes(x=0, y=endweight), alpha=.05) + 
  geom_rug(data=data, aes(y=endweight), alpha=.05, sides="r") +
  geom_line(data=subset(tab2, len>2 & seq>1 & ntrials>3 & trial.time>-100),
            aes(x=trial.time, y=weight, group=interaction(q+skip, ntrials, fingerid)), alpha=.1) + 
  facet_grid(type~., scales="free_x") + 
  xlab("Time until Trial End") + 
  ylab("Weight") + 
  geom_hline(yintercept=1) + 
  geom_hline(yintercept=0) +
  xlim(c(-25, 1)) + 
  ylim(c(-1, 2))


data$startweight.cat <- factor(
  sapply(data$startweight, function(i) sum(i<=quantile(data$startweight, seq(.2, 1, .2)))),
  labels=paste(quantile(data$startweight, seq(0, .8, .2)), quantile(data$startweight, seq(.2, 1, .2)), sep=" - "))

ggplot() + geom_density(data=data, aes(x=endweight, group=startweight.cat,
                                       colour=startweight.cat, fill=startweight.cat), alpha=I(.2)) + 
  geom_rug(data=data, aes(x=endweight), alpha=I(.1)) + 
  facet_grid(startweight.cat~type) + scale_fill_discrete("Starting Weight") + 
  scale_colour_discrete("Starting Weight") + 
  xlab("Final Weight") + ylab("Density") + ggtitle("Density of Final Weight") + xlim(c(-.5, 1.5))


probs <- ddply(data, .(startweight), summarise, plt80 = sum(endweight<.8)/length(endweight), plt0 = sum(endweight<0)/length(endweight), n = length(endweight), avg = mean(endweight), q95 = quantile(endweight, .95), q05=quantile(endweight, .05))

qplot(data=probs, geom="smooth", x=startweight, y=avg) + geom_point(alpha=.1)
qplot(data=data, geom="smooth", x=startweight, y=endweight) + geom_point(alpha=.01) + ylim(c(-.25, 1.25))
qplot(data=subset(data, ntrials>15), geom="smooth", x=startweight, y=endweight, colour=type)+ facet_wrap(~fingerid) + geom_point(alpha=.2) + ylim(c(-.25, 1.25)) + xlim(c(-.25, 1.25))

qplot(data=subset(data, ntrials>10), geom="boxplot", x=factor(fingerid), y=endweight) + facet_wrap(~type) + ylim(c(-1, 2)) + ggtitle("Individual boxplots")



#-----------------------Distribution of W----------------------------------------------------------
logpost <- function(data, par){
  temp <- sum(dtnorm(data$answer.w, mean=par[1], sd=par[2], lower=0, upper=1.4, log=TRUE))
  temp <- temp-max(temp)*.9
}

get_posterior_density <- function(data, pars){
  temp <- sapply(1:nrow(pars), function(i) logpost(data, pars[i,]))
  temp <- exp(temp)/sum(exp(temp))
  data.frame(mean=pars[,1], sd=pars[,2], f=temp)
}
