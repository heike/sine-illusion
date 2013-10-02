# Functions to correct actual data in x and y
library(ggplot2)
# correct.x <- function(x, y, data, weight=.3595){ 
#   # since .3595 is the midpoint of the left and right estimates of optimal values.
#   require(locfit)
#   form <- as.formula(paste(y, "~lp(", x, ", deg=2, acri=)"))
#   
#   model <- locfit(data=data, formula=form)
#   deriv <- locfit(data=data, formula=form, deriv=1)
#   f <- function(z) predict(model, newdata=structure(data.frame(x=z, y=0), names=c(x, y)))
#   fprime <- function(z) predict(deriv, newdata=structure(data.frame(x=z, y=0), names=c(x,y)))
#   a <- min(data[,x])
#   b <- max(data[,x])
#   
#   const <- integrate(function(z) abs(fprime(z)), a, b)$value
#   trans <- sapply(data[,x], function(i) integrate(function(z) abs(fprime(z)), a, i)$value*(b-a)/const + a)
# #   const <- sum(abs(fprime(data[,x])))
# #   trans <- cumsum(abs(fprime(data[,x])))*(b-a)/const+a                  
#   
#   data[,paste(x, "correctx", sep=".")] <- trans*weight + data[,x]*(1-weight)
# 
#   data
# }

correct.x <- function(x, y, data, weight=.3595){ 
  # since .3595 is the midpoint of the left and right estimates of optimal values.
  
  model <- smooth.spline(data[,x], data[,y], df=floor(length(unique(data[,x]))/2))

  f <- function(z) predict(model, x=z)$y
  fprime <- function(z) abs(predict(model, x=z, deriv=1)$y)
  a <- min(data[,x])
  b <- max(data[,x])
  
  const <- integrate(function(z) fprime(z), a, b, subdivisions=10000)$value
  trans <- sapply(data[,x], function(i) integrate(function(z) abs(fprime(z)), a, i)$value*(b-a)/const + a)
  #   const <- sum(abs(fprime(data[,x])))
  #   trans <- cumsum(abs(fprime(data[,x])))*(b-a)/const+a                  
  
  data[,paste(x, "correctx", sep=".")] <- trans*weight + data[,x]*(1-weight)
  
  data
}

correct.y <- function(x, y, data, weight=.4045){
  # since .4045 is the midpoint of the left and right estimates of optimal values.
  form <- as.formula(paste(y, "~", x))
  model <- locfit(data=data, formula=form)
  deriv <- locfit(data=data, formula=form, deriv=1)
  deriv2 <- locfit(data=data, formula=form, deriv=2)
  f <- function(z) predict(model, newdata=structure(data.frame(x=z, y=0), names=c(x, y)))
  fprime <- function(z) predict(deriv, newdata=structure(data.frame(x=z, y=0), names=c(x,y)))
  f2prime <- function(z) predict(deriv2, newdata=structure(data.frame(x=z, y=0), names=c(x,y)))
  df <- data.frame(y=data[,y], pred=predict(model, newdata=data), x=data[,x], deriv=predict(deriv, newdata=data), deriv2 = predict(deriv2, newdata=data))
  df$resid <- df$y - df$pred
  
  dy1 <- diff(range(df$y))
  dy <- diff(range(df$pred))
  a <- dy/dy1 # aspect ratio : line "length" correction
  
  df$ell <- abs(df$resid)*2
  fp <- a*df$deriv
  f2p <- a*df$deriv2
  v <- 1 + fp^2
  lambdaminv <- 0.5*(sqrt(v^2+f2p*fp^2*df$ell) + v)
  lambdapinv <- 0.5*(sqrt(v^2-f2p*fp^2*df$ell) + v)
  data[,paste(y, "correcty", sep=".")] <- df$pred + sign(df$resid)*(
    (df$resid>=0)*0.5*abs(lambdaminv)/sqrt(v) + 
    (df$resid< 0)*0.5*abs(lambdapinv)/sqrt(v)
    )
  # use one correction if neg. resid, another if positive
  
  data

}

correct <- function(x, y, data, type){
  if(type=="x") correct.x(x, y, data)
  else correct.y(x, y, data)
}

# datasub <- read.csv("data/Ozone/Ozone-subset.csv")
# temp <- correct("Tmax", "Ozone", datasub, type="x")
# qplot(data=temp, x=Tmax.correctx, y=Ozone)
# temp2 <- correct("Tmax", "Ozone", datasub, type="y")
# qplot(data=temp2, x=Tmax, y=Ozone.correcty)
