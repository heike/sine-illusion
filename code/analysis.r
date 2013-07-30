# Analysis of data from Amazon Turk experiment conducuted 7/22/13-7/28/13.

library(RMySQL)
library(plyr)
library(lubridate)
library(ggplot2)
library(grid)
library(reshape2)
library(doMC) 

registerDoMC() 
mcoptions <- list(preschedule=FALSE, set.seed=FALSE)
getDoParWorkers() 

# # ----------------- Database Access -------------------------
# con <- dbConnect(MySQL(), group="stat")
# tab <- dbReadTable(con, name="SineIllusionShiny")[-1,]
# dbDisconnect(con)
# Mode <- function(x) {
#   ux <- unique(x)
#   ux[which.max(tabulate(match(x, ux)))]
# }
# 
# diffs <- c(0, .5, .5, .5, .5, .25, .25, .25, .25, .25, .25, .25, .25, .1, .1, .05, .05, .05, .05, .05, .05, .025, .025, .025, .025, .025, .025, .025, .025, .025, .025, .025, .025, .05, .05, .05, .05, .1, .1, .1, .1, .1, .25, .25, .25, .25, .25, .25, .5, .5, .5, .5)
# wopts <- -4 +cumsum(diffs)
# 
# tab$time2 <- ymd_hms(tab$time) # convert to lubridate time
# tab <- ddply(tab, .(iphash, fingerprint, ipid), transform, ntrials=length(unique(q+skip))) # add number of trials
# tab <- tab[which(tab$weight>0),] # remove "initial" observation that no longer corresponds to weight in this iteration of the program. 
# tab <- subset(tab, !grepl("test", userid)) # remove "test" runs
# tab <- subset(tab, !grepl("76.84", ipid)) # remove data from Auburn/Nebraska City, NE, because it's full of "tests" that weren't marked as such. Whoops.
# tab$weight <- wopts[tab$weight] # convert javascript vector weight into actual meaningful weight measurement
# 
# 
# tab$training <- (tab$time2>ymd("2013-7-21") & (tab$q+tab$skip)<2) # two "training" questions added on 7.21.13 that may be biased... test?

# #------------------ Data Reshaping -------------------------
# # dataset of individual questions with each individual observation and time. 
# tab2 <- ddply(tab, .(iphash, fingerprint, ipid, q, skip, type, ntrials), transform,
#               weight = weight,
#               start.weight = rep(weight[which.min(time2)],length(time2)), 
#               end.weight = rep(weight[which.max(time2)],length(time2)),
#               time.diff = as.numeric(c(0, diff(time2))),
#               end.time = rep(min(time2)-max(time2),length(time2)),
#               len = rep(length(time2),length(time2)),
#               trial.time = as.numeric(time2-max(time2)),
#               seq = 1:length(time2),
#               post.training = time2>ymd("2013-7-21") & (q+skip)>=2)
# 
# # dataset of individual questions, but without each individual change
# # Includes only trials where the participant interacted with the graph at least twice. 
# data <- ddply(subset(tab2, len>2 & seq>1), .(iphash, fingerprint, ipid, q, skip, type, ntrials), 
#               function(df){
#                 with(df, 
#                      data.frame(startweight = weight[which.min(time2)], 
#                                 endweight =  weight[which.max(time2)], 
#                                 modeweight =  Mode(weight), 
#                                 len = length(weight), 
#                                 unique.weights = length(unique(weight)),
#                                 time.trial = max(time2)-min(time2),
#                                 training = training[1]),
#                                 post.training = time2>ymd("2013-7-21") & (q+skip)>=2)
#               })
# 
# data$fingerid <- as.numeric(factor(data$fingerprint))
# tab2$fingerid <- as.numeric(factor(tab2$fingerprint))
# 
# write.csv(tab2[,-which(names(tab2)=="userid")], "./data/IndivTrajectory.csv")
# write.csv(data, "./data/SummaryTable.csv")


# #-----------------------Read in Data------------------------

# use if no access to the SQL database.
data <- read.csv("./data/SummaryTable.csv", row.names=1, stringsAsFactors=FALSE)
tab2 <- read.csv("./data/IndivTrajectory.csv", row.names=1, stringsAsFactors=FALSE)
tab2$time2 <- ymd_hms(tab2$time2)

# #----------- Plots and Exploration - raw data --------------

# plot of directional arrows indicating start and end point by user and trial type.
qplot(data=subset(data, len>1, post.training=TRUE), 
      x=startweight, xend=endweight, 
      y=fingerprint, yend=fingerprint, geom="segment", 
      arrow=arrow(length = unit(0.1,"cm")), group=q+skip, alpha=I(.2)) + 
  geom_vline(aes(xintercept=0), linetype=2) +
  geom_vline(aes(xintercept=1), linetype=2) +
  facet_wrap(~type)

# plot of trial trajectories for each user and trial type (not so useful now that there are a bunch of users)
qplot(data=subset(tab2, len>2 & seq>1 & ntrials>6 & trial.time>-500 & !training), x=trial.time, y=weight, group=q+skip, geom="line", colour=factor((q+skip)%%6)) + geom_point(aes(x=0, y=end.weight)) + facet_grid(type~fingerprint, scales="free_x") + xlab("Time until Trial End") + ylab("Weight") + geom_hline(yintercept=1) + geom_hline(yintercept=0)


# plot of trial trajectories for each user
qplot(data=subset(tab2, len>2 & seq>1 & ntrials>15 & trial.time>-500 & !training), x=trial.time, y=weight, group=q+skip, geom="line", colour=type, alpha=I(.5)) + geom_point(aes(x=0, y=end.weight), alpha=.5) + facet_wrap(~fingerprint, scales="free_x") + xlab("Time until Trial End") + ylab("Weight") + geom_hline(yintercept=1) + geom_hline(yintercept=0)

# plot of trial trajectories, for each trial type across all users. 
ggplot() + 
  geom_point(data=data, aes(x=0, y=endweight), alpha=.05) + 
  geom_rug(data=data, aes(y=endweight), alpha=.05, sides="r") +
  geom_line(data=subset(tab2, len>2 & seq>1 & ntrials>4 & trial.time>-100),
            aes(x=trial.time, y=weight, group=interaction(q+skip, fingerprint)), alpha=.1) + 
  facet_grid(type~., scales="free_x") + 
  xlab("Time until Trial End") + 
  ylab("Weight") + 
  geom_hline(yintercept=1) + 
  geom_hline(yintercept=0) +
  xlim(c(-25, 1)) + 
  ylim(c(-1, 2))

# Where (approximately) did weight start?
data$startweight.cat <- factor(
  sapply(data$startweight, function(i) sum(i<=quantile(data$startweight, seq(.2, 1, .2)))),
  labels=paste(quantile(data$startweight, seq(0, .8, .2)), quantile(data$startweight, seq(.2, 1, .2)), sep=" - "))

ggplot() + geom_density(data=data, aes(x=endweight, group=startweight.cat,
                                       colour=startweight.cat, fill=startweight.cat), alpha=I(.2)) + 
  geom_rug(data=data, aes(x=endweight), alpha=I(.1)) + 
  facet_grid(startweight.cat~type) + scale_fill_discrete("Starting Weight") + 
  scale_colour_discrete("Starting Weight") + 
  xlab("Final Weight") + ylab("Density") + ggtitle("Density of Final Weight") + xlim(c(-.5, 1.5))

qplot(data=subset(data, ntrials>10 & q>1), geom="boxplot", x=factor(fingerid), y=endweight) + facet_wrap(~type) + ylim(c(-1, 2)) + ggtitle("Individual boxplots")


#-----------------------Distribution of W-------------------
logpost <- function(data, par){
  temp <- sum(dnorm(data$endweight, mean=par[1], sd=par[2], log=TRUE))
  temp <- temp-max(temp)*.9 # adjust by a constant so small values don't wash out.
}

get_posterior_density <- function(data, pars){
  temp <- sapply(1:nrow(pars), function(i) logpost(data, pars[i,]))
  temp <- exp(temp)/sum(exp(temp))
  data.frame(mean=pars[,1], sd=pars[,2], f=temp)
}

#------------------------- Overall Marginals ---------------

pars <- as.matrix(expand.grid(seq(-.5, 1.2, .005), seq(.4, 1, .005)))

overall <- ddply(subset(data, ntrials>6), .(type), get_posterior_density, pars=pars)
overall.mean <- ddply(overall[,-3], .(type, mean), summarise, f=sum(f))
overall.mean <- ddply(overall.mean, .(type), transform, f=f/sum(f))
#' Posterior marginal distribution over individual and std. deviation
qplot(data=overall.mean, x=mean, y=f, geom="line", colour=type, group=type) + 
  scale_colour_discrete("Function Type") + 
  xlab("Mean Preferred Weighting") + ylab("Density") + theme_bw()  + theme(legend.position="bottom") 
# ggsave("figure/fig-OverallMeansW.pdf", width=4, height=4, units="in")


overall.sd <- ddply(overall[,-2], .(type, sd), summarise, f=sum(f))
overall.sd <- ddply(overall.sd, .(type), transform, f=f/sum(f))

#' Posterior marginal distribution over individual and mean
qplot(data=overall.sd, x=sd, y=f, geom="line", colour=factor(type), group=type) + theme_bw() + xlab("Posterior SD") + ylab("Density")

#' Posterior joint dist of mean, sd over individuals
#' since stat_density2d won't use weights, ... improvise!
overall.joint.sample <- sample(1:nrow(overall), size=50000, replace=TRUE, prob=overall$f)
ggplot(data=overall[overall.joint.sample,], aes(x=mean, y=sd)) + 
  stat_density2d(n=c(75, 40), geom="density2d", aes(colour=type)) + 
#   facet_wrap(~type) + scale_colour_discrete(guide="none") + 
  theme_bw() + 
  xlab("Mean Preferred Weighting") + ylab("Std. Deviation")
# ggsave("figure/fig-Joint2dDensityW.pdf", width=4, height=4, units="in")

#--------------------Individual Distribution of Theta ------

# get posterior density for each individual
test <- ddply(subset(data, ntrials>12), .(fingerprint, fingerid, type), get_posterior_density, pars=pars, .parallel=TRUE)

test.mean <- ddply(test, .(fingerprint, fingerid, type, mean), summarise, f=sum(f))
test.mean <- ddply(test.mean, .(fingerprint, fingerid, type), transform, f=f/sum(f))

participants <- dcast(ddply(data, .(fingerprint, type), summarise, 
                            fingerid=unique(fingerid), n=length(type)), 
                      fingerprint+fingerid~type, value.var="n")
participants.max <- ddply(test.mean, .(fingerprint, fingerid, type), summarise, x = mean[which.max(f)], y=max(f))

ipsubset <- subset(participants, rowSums(is.na(participants))==0 & 
                     rowSums(participants[,3:4]>12, na.rm=TRUE)==2)$fingerprint

par_labeller <- function(var, value){
  n <- sapply(value, function(i) sum(subset(participants, fingerid%in%i)[,3:4]))
#   value <- subset(participants, fingerprint==value)$fingerid
  value <- paste("Participant ", as.character(value), "\n(n = ", n, ")", sep="")
  return(value)
}


#' Plot 4 individuals who did at least 12 trials of each type 
qplot(data=subset(test.mean, fingerprint%in%ipsubset), x=mean, y=f, group=type, colour=type, geom="line") + 
  facet_grid(.~fingerid, labeller=par_labeller) + scale_colour_discrete("Function Type") + theme_bw() + 
  theme(legend.position="bottom") + xlab("Preferred Weight") + ylab("Density") + 
  geom_segment(data=subset(participants.max, fingerprint%in%ipsubset), aes(x=x, xend=x, y=0, yend=y, colour=type))
# ggsave("figure/fig-IndivMeanAllFcnsW.pdf", width=7, height=3.5)      



#' Posterior mean estimates, including CI information for the individual MEAN 
#' (i.e. not for any individual observation)
test.post.indiv<- ddply(test.mean, .(fingerprint, fingerid, type), 
                        function(x){
                          ex=sum(x$mean*x$f)
                          n=sum(data$fingerprint==x$fingerprint[1] & data$type==x$type[1])
                          samp <- matrix(sample(x$mean, n*20, prob=x$f, replace=TRUE), ncol=20)
                          z <- as.numeric(quantile(rowMeans(samp), c(.025, .5, .975)))
                          data.frame(fingerprint=unique(x$fingerprint), type=unique(x$type), lb=z[1], mean = ex, median=z[2], ub=z[3], n=n)
                        })

overall.mean.f <- ddply(test.mean, .(type, mean), summarise, f=sum(f))
overall.mean.f <- ddply(overall.mean.f, .(type), transform, f=f/sum(f))

overall.mean.bounds <- ddply(overall.mean.f, .(type), function(x){
  ex=sum(x$mean*x$f)
  n=length(unique(subset(data, data$type==type)$fingerprint))
  samp <- matrix(sample(x$mean, n*11, prob=x$f, replace=TRUE), ncol=11)
  sample.mean = mean(samp)                          
  sdev = sd(rowMeans(samp))
  lb = as.numeric(quantile(rowMeans(samp), .025))
  med = as.numeric(quantile(rowMeans(samp), .5))
  ub = as.numeric(quantile(rowMeans(samp), .975))
  data.frame(lb=lb, mean=sample.mean, median=med, ub=ub)
})


qplot(data=test.post.indiv,  x=lb, xend=ub, y=factor(fingerid), yend=factor(fingerid), geom="segment", colour=type) + 
  facet_wrap(~type) + geom_point(aes(x=median), colour="black") + 
  geom_vline(data=overall.mean.bounds, aes(xintercept=lb), linetype=3) + 
  geom_vline(data=overall.mean.bounds, aes(xintercept=median)) + 
  geom_vline(data=overall.mean.bounds, aes(xintercept=ub), linetype=3) + 
  ylab("Participant ID") + xlab("Mean Preferred Weighting") + theme_bw() + theme(legend.position="none") + 
  scale_colour_discrete("Function Type")
# ggsave("figure/fig-CIindivMeanW.pdf", width=6, height=6, units="in")

#' Posterior estimates, including CI information for the individual observations 
#' (i.e. not for any individual observation)
indiv.value.bounds <- ddply(test.mean, .(fingerprint, type), function(x){
  lb=x$mean[which.min(abs(cumsum(x$f)-.025))]
  med=x$mean[which.min(abs(cumsum(x$f)-.5))]
  ub=x$mean[which.min(abs(cumsum(x$f)-.975))]
  data.frame(lb=lb, median=med, ub=ub)
})

overall.value.bounds <- ddply(overall.mean.f, .(type), function(x){
  xnew <- sample(x$mean, length(x$mean), prob=x$f, replace=TRUE)
  z <- as.numeric(quantile(xnew, c(.025, .5, .975)))
  data.frame(lb=z[1], median=z[2], ub=z[3])
})
# Posterior Distribution for theta without averaging over individuals
qplot(data=overall.mean.f, x=mean, y=f, geom="line", colour=type) + 
  xlab("Psychological Lie Factor\nEstimated Distribution for All Individuals") + 
  theme_bw() + theme(legend.position="bottom") + scale_color_discrete("Function Type") + 
  ylab("Density")

qplot(data=indiv.value.bounds,  x=lb, xend=ub, y=fingerprint, yend=fingerprint, geom="segment", colour=type) + 
  facet_wrap(~type) + geom_point(aes(x=median), colour="black") + 
  geom_vline(data=overall.value.bounds, aes(xintercept=lb), linetype=3) + 
  geom_vline(data=overall.value.bounds, aes(xintercept=median)) + 
  geom_vline(data=overall.value.bounds, aes(xintercept=ub), linetype=3) + 
  ylab("Participant ID") + xlab("Lie Factor") + theme_bw() + theme(legend.position="bottom") + 
  scale_colour_discrete("Function Type")

