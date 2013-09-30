library(lme4)

model2 <- lmer(data=lm.data, endweight~ (type-1) + startweight + (type-1|fingerprint))
## not necessary
# model3 <- lmer(data=lm.data, endweight~ (type-1) + startweight:(type-1) + (type-1|fingerprint))

model2.sim <- simulate(model2, nsim=1000)
res <- llply(model2.sim, function(x) model2 <- lmer(data=lm.data, x~ (type-1) + startweight + (type-1|fingerprint))
)

# extract pieces for confidence intervals
ranefs <- ldply(res, function(x) attr(VarCorr(x)$fingerprint, which="stddev"))
